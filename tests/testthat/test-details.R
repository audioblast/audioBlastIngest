detailsFixture <- function() {
  return(test_path("fixtures", "details.csv"))
}

#Details as ingestR() reads them from a source, which gives no record_source
readDetails <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(detailsFixture(), colClasses="character", encoding="UTF-8"))
  data$record_source <- rep_len("", nrow(data))
  colnames(data) <- names(getHeaders("details"))
  return(data)
}

#A detail of a source, filling in whatever isn't given
detail <- function(source="bio.acousti.ca", type="recordings", id="8961", name="cd_id",
                   delta="0", value="372", unit="", record_source="") {
  return(data.frame(source, type, id, name, delta, value, unit, record_source,
                    stringsAsFactors=FALSE))
}

test_that("details are normalised, and the details of one name are numbered from 0", {
  expect_warning(details <- normaliseDetails(readDetails()),
                 "Skipping 2 details of an unknown type, or with no record, name or value")

  expect_identical(names(details), names(getHeaders("details")))
  expect_identical(details$source, rep("bio.acousti.ca", 7))
  expect_identical(details$type, c(rep("recordings", 6), "specimens"))
  expect_identical(details$name,
                   c("cd_id", "cd_track", "temperature_start", "project", "project",
                     "description", "field_notes"))
  #The two projects of recording 8961 are 0 and 1, not the 0 and 2 of the source
  expect_identical(details$delta, c("0", "0", "0", "0", "1", "0", "0"))
  expect_identical(details$unit, c("", "", "\u00b0C", "", "", "", ""))
  #Values are HTML at bio.acousti.ca
  expect_identical(details$value[6], "\u201cSocial\u201d calls.")
  expect_identical(details$value[7], "On reeds & grass")
})

test_that("normalising details again changes nothing", {
  details <- suppressWarnings(normaliseDetails(readDetails()))

  expect_identical(normaliseDetails(details), details)
})

test_that("details of an unknown type, or with no record, name or value, are skipped", {
  table <- rbind(
    detail(value="372"),
    detail(type="tapes", value="D1"),
    detail(id="", value="1"),
    detail(name="", value="2"),
    detail(value=""))

  expect_warning(details <- normaliseDetails(table),
                 "Skipping 4 details of an unknown type, or with no record, name or value")

  expect_identical(details$value, "372")
})

test_that("uploadDetails replaces the details of each source in one transaction", {
  table <- rbind(detail(), detail(source="unp", name="gain", value="8"))

  upload <- suppressWarnings(mockUpload(uploadDetails, table))

  #The details of both sources are deleted, and the new ones inserted by one statement
  expect_identical(upload$calls, c("begin", "execute", "execute", "execute", "commit"))
  expect_identical(upload$executed[[1]]$sql, "DELETE FROM `details` WHERE `source` = ?")
  expect_identical(upload$executed[[1]]$params, list("bio.acousti.ca"))
  expect_identical(upload$executed[[2]]$params, list("unp"))
  columns <- names(getHeaders("details"))
  expect_identical(upload$executed[[3]]$sql,
                   insertSQL("details", columns, c("value", "unit"), 2))
  rows <- boundRows(upload$executed[[3]])
  expect_identical(rows[[1]], list("bio.acousti.ca", "recordings", "8961", "cd_id", "0", "372", NA_character_,
                                   "bio.acousti.ca"))
  expect_identical(rows[[2]][[6]], "8")
})

test_that("uploadDetails does nothing when no details can be used", {
  upload <- suppressWarnings(mockUpload(uploadDetails, detail(type="tapes")))

  expect_length(upload$calls, 0)
})

test_that("a source's details of another source's record are numbered apart from its own", {
  #A corpus giving the frequencies of a region of its own and of one of
  #xeno-canto's, which share an id and a type but are two records
  table <- rbind(
    detail(source="jeantet-dufourq-2023", type="annomate", id="1", name="frequency_low",
           value="1200", unit="Hz"),
    detail(source="jeantet-dufourq-2023", type="annomate", id="1", name="frequency_low",
           value="800", unit="Hz", record_source="xeno-canto"))

  details <- normaliseDetails(table)

  expect_identical(details$record_source, c("", "xeno-canto"))
  #Each is the first of its record's values of that name
  expect_identical(details$delta, c("0", "0"))
})

test_that("a record a source names as its own is written as its own", {
  table <- rbind(detail(), detail(name="cd_track", value="1", record_source="bio.acousti.ca"))

  details <- normaliseDetails(table)

  expect_identical(details$record_source, c("", ""))
})

test_that("details without record_source are of their source's own records", {
  #As a harvest streamed to files before the column was added would give them
  table <- detail()[setdiff(names(getHeaders("details")), "record_source")]

  details <- normaliseDetails(table)

  expect_identical(names(details), names(getHeaders("details")))
  expect_identical(details$record_source, "")
})

test_that("uploadDetails removes only what the giving source gave", {
  #A corpus's details of xeno-canto's recordings are the corpus's to replace,
  #and xeno-canto's own details are left to xeno-canto
  table <- rbind(
    detail(source="jeantet-dufourq-2023", id="280667", name="frequency_high",
           value="9000", unit="Hz", record_source="xeno-canto"),
    detail(source="jeantet-dufourq-2023", type="references", id="zenodo.7828148",
           name="split", value="Training"))

  upload <- mockUpload(uploadDetails, table)

  deletes <- Filter(function(e) grepl("^DELETE", e$sql), upload$executed)
  expect_length(deletes, 1)
  expect_identical(deletes[[1]]$params, list("jeantet-dufourq-2023"))
  rows <- boundRows(upload$executed[[2]])
  expect_identical(rows[[1]], list("jeantet-dufourq-2023", "recordings", "280667",
                                   "frequency_high", "0", "9000", "Hz", "xeno-canto"))
  #A detail of the corpus's own record names the corpus as its record's source,
  #so that a record's details are found by its source whoever gave them
  expect_identical(rows[[2]][[8]], "jeantet-dufourq-2023")
})

test_that("ingestR uploads details from details sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="details", url=detailsFixture(), process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadDetails=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("details")))
  expect_equal(nrow(uploaded), 9)
  expect_identical(unique(uploaded$source), "bio.acousti.ca")
  expect_identical(uploaded$value[1], "372")
  #The source gives no record_source, so its details are of its own records
  expect_true(all(uploaded$record_source == ""))
})

test_that("ingestR uploads the details a source gives of another source's records", {
  path <- withr::local_tempfile(fileext=".csv")
  writeLines(c('"type","id","name","delta","value","unit","record_source"',
               '"recordings","280667","frequency_low","0","800","Hz","xeno-canto"'),
             path)
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="jeantet-dufourq-2023", type="details", url=path, process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadDetails=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(uploaded$source, "jeantet-dufourq-2023")
  expect_identical(uploaded$record_source, "xeno-canto")
  expect_identical(uploaded$value, "800")
})

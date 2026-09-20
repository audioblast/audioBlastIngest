detailsFixture <- function() {
  return(test_path("fixtures", "details.csv"))
}

#Details as ingestR() reads them from a source
readDetails <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(detailsFixture(), colClasses="character", encoding="UTF-8"))
  colnames(data) <- names(getHeaders("details"))
  return(data)
}

#A detail of a source, filling in whatever isn't given
detail <- function(source="bio.acousti.ca", type="recordings", id="8961", name="cd_id",
                   delta="0", value="372", unit="") {
  return(data.frame(source, type, id, name, delta, value, unit, stringsAsFactors=FALSE))
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
  expect_identical(rows[[1]], list("bio.acousti.ca", "recordings", "8961", "cd_id", "0", "372", NA_character_))
  expect_identical(rows[[2]][[6]], "8")
})

test_that("uploadDetails does nothing when no details can be used", {
  upload <- suppressWarnings(mockUpload(uploadDetails, detail(type="tapes")))

  expect_length(upload$calls, 0)
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
})

#A table of the given columns, with each value naming its column and row
columnTable <- function(columns, rows=3) {
  data.frame(lapply(setNames(nm=columns), paste0, "-", seq_len(rows)), check.names=FALSE)
}

#The values of a table, row by row, each as the type its column holds
byRow <- function(table) {
  return(unlist(lapply(seq_len(nrow(table)), function(i) unname(as.list(table[i, , drop=FALSE]))),
                recursive=FALSE))
}

test_that("insertSQL inserts rows, updating rows already there", {
  expect_identical(
    insertSQL("t", c("a", "b", "c"), c("b", "c"), 2),
    paste("INSERT INTO `t` (`a`, `b`, `c`) VALUES (?, ?, ?), (?, ?, ?)",
          "ON DUPLICATE KEY UPDATE `b` = VALUES(`b`), `c` = VALUES(`c`)"))
})

#Each uploader, its database table, the columns it updates on rows already
#there, and what it does to values before uploading them
uploads <- list(
  traits=list(upload=uploadTraits, table="traits", update=-(1:2), normalise=normaliseTraits),
  recordings=list(upload=uploadRecordings, table="recordings", update=-(1:2), normalise=normaliseRecordings),
  deployments=list(upload=uploadDeployments, table="deployments", update=-(1:2)),
  "ann-o-mate"=list(upload=uploadAnnOmate, table="annomate", update=1:18, normalise=normaliseAnnOmate),
  references=list(upload=uploadReferences, table="references", update=-(1:2)),
  specimens=list(upload=uploadSpecimens, table="specimens", update=-(1:2), normalise=normaliseSpecimens),
  locations=list(upload=uploadLocations, table="locations", update=-(1:2), normalise=normaliseLocations))

for (type in names(uploads)) {
  test_that(paste("Uploading", type, "inserts every row, updating rows already there"), {
    columns <- names(getHeaders(type))
    table <- columnTable(columns)
    normalise <- if (is.null(uploads[[type]]$normalise)) identity else uploads[[type]]$normalise

    #Values such as "Date-1" can't be read as recordings' dates, so are warned of
    upload <- suppressWarnings(mockUpload(uploads[[type]]$upload, table))

    expect_identical(upload$calls, c("begin", "execute", "commit"))
    expect_identical(
      upload$executed[[1]]$sql,
      insertSQL(uploads[[type]]$table, columns, columns[uploads[[type]]$update], 3))
    expect_identical(upload$executed[[1]]$params, byRow(suppressWarnings(normalise(table))))
  })

  test_that(paste("Uploading", type, "executes nothing for an empty table"), {
    table <- columnTable(names(getHeaders(type)))[0, , drop=FALSE]

    upload <- mockUpload(uploads[[type]]$upload, table)

    expect_length(upload$executed, 0)
  })
}

test_that("an annotation is of a recording of its own source unless it names another", {
  columns <- names(getHeaders("ann-o-mate"))
  table <- columnTable(columns, rows=2)
  table$recording_source <- c("", "xeno-canto")

  upload <- mockUpload(uploadAnnOmate, table)

  recording_source <- which(columns == "recording_source")
  expect_identical(lapply(boundRows(upload$executed[[1]]), `[[`, recording_source),
                   list("source-1", "xeno-canto"))

  #A table without the column, as a source's file of the earlier columns is
  #read, is given it
  earlier <- columnTable(columns[seq_len(recording_source - 1)], rows=1)
  upload <- mockUpload(uploadAnnOmate, earlier)
  expect_identical(boundRows(upload$executed[[1]])[[1]][[recording_source]], "source-1")
})

test_that("a region's frequency bounds are numbers of Hz from 0 up, or NULL", {
  columns <- names(getHeaders("ann-o-mate"))
  bounds <- match(c("freq_low", "freq_high"), columns)
  table <- columnTable(columns, rows=5)
  #A region bounded from 0 Hz holds that frequency; one without bounds, or
  #with bounds that can't be frequencies, has none
  table$freq_low <- c("0", " 1691.9 ", "", "-5", "low")
  table$freq_high <- c("7309.05", "22050", NA, "8000", "3 kHz")

  rows <- boundRows(mockUpload(uploadAnnOmate, table)$executed[[1]])

  expect_identical(vapply(rows, `[[`, character(1), bounds[1]), c("0", "1691.9", NA, NA, NA))
  expect_identical(vapply(rows, `[[`, character(1), bounds[2]), c("7309.05", "22050", NA, "8000", NA))

  #A table from before the bounds were columns, as a source's file or a
  #streamed harvest written then is read, has none
  earlier <- columnTable(setdiff(columns, c("freq_low", "freq_high")), rows=1)
  row <- boundRows(mockUpload(uploadAnnOmate, earlier)$executed[[1]])[[1]]
  expect_identical(row[bounds], list(NA_character_, NA_character_))
})

test_that("uploadTaxa inserts taxa columns by name", {
  columns <- c("source", "id", "taxon", "parent_id", "Rank", "Kingdom",
               "Subkingdom", "Phylum", "Subphylum", "Class", "Order",
               "Suborder", "Infraorder", "Superfamily", "Family", "Subfamily",
               "Tribe", "Subtribe", "Genus", "Subgenus", "Species", "Subspecies",
               "Form", "taxonomicStatus", "nomenclaturalStatus", "acceptedNameUsageID",
               "acceptedNameUsage")
  table <- columnTable(rev(columns))

  upload <- mockUpload(uploadTaxa, table)

  expect_identical(upload$executed[[1]]$sql, insertSQL("taxa", columns, columns[-(1:2)], 3))
  expect_identical(upload$executed[[1]]$params, byRow(table[columns]))
})

test_that("uploadRows inserts each batch of rows in its own transaction", {
  values <- data.frame(a=as.character(1:5), b=letters[1:5])

  upload <- mockUpload(uploadRows, "t", c("a", "b"), values, update="b", batch=2)

  expect_identical(upload$calls, rep(c("begin", "execute", "commit"), 3))
  expect_identical(lapply(upload$executed, `[[`, "sql"), list(
    insertSQL("t", c("a", "b"), "b", 2),
    insertSQL("t", c("a", "b"), "b", 2),
    insertSQL("t", c("a", "b"), "b", 1)))
  expect_identical(lapply(upload$executed, `[[`, "params"), list(
    list("1", "a", "2", "b"),
    list("3", "c", "4", "d"),
    list("5", "e")))
})

test_that("uploadRows inserts batches of 1000 rows by default", {
  upload <- mockUpload(uploadRows, "t", "a", data.frame(a=as.character(1:2500)), update="a")

  expect_identical(lengths(lapply(upload$executed, `[[`, "params")), c(1000L, 1000L, 500L))
})

test_that("uploadRows rolls back a failed batch, keeping the batches before it", {
  calls <- character()
  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) {
      calls <<- c(calls, "execute")
      if ("3" %in% params) stop("Data too long for column")
      0L
    },
    useUTF8=function(db) invisible(TRUE))
  local_mocked_bindings(
    dbWithTransaction=function(conn, code) {
      calls <<- c(calls, "begin")
      tryCatch(code, error=function(e) {
        calls <<- c(calls, "rollback")
        stop(e)
      })
      calls <<- c(calls, "commit")
    },
    .package="DBI")

  expect_error(
    uploadRows("db", "t", "a", data.frame(a=as.character(1:5)), update="a", batch=2),
    "Data too long for column")
  #The second batch fails, so the third is never sent
  expect_identical(calls, c("begin", "execute", "commit", "begin", "execute", "rollback"))
})

#A database that stores what it is sent as the given bytes, recording the
#statements it executes
local_storingAs <- function(stored, env=parent.frame()) {
  log <- new.env()
  log$executed <- character()
  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) {
      log$executed <- c(log$executed, statement)
      0L
    },
    dbGetQuery=function(conn, statement, params=NULL, ...) {
      log$asked <- params
      data.frame(stored=stored)
    }, .env=env)
  local_mocked_bindings(dbWithTransaction=function(conn, code) code, .package="DBI", .env=env)
  return(log)
}

test_that("an upload has the connection talk UTF-8 before it sends anything", {
  log <- local_storingAs("C3A9")

  uploadRows("db", "t", "a", data.frame(a="1"), update="a")

  expect_identical(log$executed[1], "SET NAMES utf8mb4")
  expect_match(log$executed[2], "^INSERT INTO `t`")
  #and asks what an e acute would be stored as, which on a connection that
  #talks UTF-8 is its own two bytes
  expect_identical(log$asked, list(intToUtf8(233L)))
})

test_that("an upload over a connection that would mangle text stops before it sends any", {
  #A connection whose client says latin1 stores an e acute as the bytes of the
  #two characters its UTF-8 reads as in CP1252
  log <- local_storingAs("C383C2A9")

  expect_error(uploadRows("db", "t", "a", data.frame(a="1"), update="a"),
               "would be stored as C383C2A9")
  expect_identical(log$executed, "SET NAMES utf8mb4")
})

test_that("a streamed upload checks the connection before it removes what its source gave", {
  dir <- withr::local_tempdir()
  streamTable(dir, "details", columnTable(names(getHeaders("details")), rows=1))
  log <- local_storingAs("C383C2A9")

  expect_error(uploadStreamed("db", "s", dir), "would be stored as C383C2A9")
  #The source's details, which are removed before the first chunk of them, are
  #still there
  expect_identical(log$executed, "SET NAMES utf8mb4")
})

test_that("an empty upload asks nothing of the connection", {
  log <- local_storingAs("C3A9")

  uploadRows("db", "t", "a", data.frame(a=character(0)), update="a")

  expect_length(log$executed, 0)
})

test_that("the ids a source holds are read back over a connection that talks UTF-8", {
  calls <- character()
  local_mocked_bindings(
    useUTF8=function(db) calls <<- c(calls, "utf8"),
    dbGetQuery=function(conn, statement, params=NULL, ...) {
      calls <<- c(calls, "read")
      data.frame(id=character(0))
    })

  withdrawRecordings("db", "s", "1")

  expect_identical(calls, c("utf8", "read"))
})

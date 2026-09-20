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
  "ann-o-mate"=list(upload=uploadAnnOmate, table="annomate", update=1:15),
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

test_that("uploadTaxa inserts taxa columns by name", {
  columns <- c("source", "id", "taxon", "parent_id", "Rank", "Kingdom",
               "Subkingdom", "Phylum", "Subphylum", "Class", "Order",
               "Suborder", "Infraorder", "Superfamily", "Family", "Subfamily",
               "Tribe", "Subtribe", "Genus", "Subgenus", "Species", "Subspecies")
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
    })
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

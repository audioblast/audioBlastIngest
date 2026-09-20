awkward <- function() {
  data.frame(
    source=c("", "", ""),
    type=c("recordings", "recordings", "recordings"),
    id=c("1", "2", "3"),
    name=c("rmk", "rmk", "rmk"),
    delta=c("0", "0", "0"),
    #A remark with a comma, a quote and a line of its own in it
    value=c("Calling, then silent", "He said \"a wren\"", "First line\nsecond line"),
    unit=c("", "", ""),
    stringsAsFactors=FALSE)
}

test_that("a table is streamed to a file of its type and read back as it went in", {
  dir <- withr::local_tempdir()
  table <- awkward()

  streamTable(dir, "details", table[1, ])
  streamTable(dir, "details", table[2:3, ])

  expect_identical(list.files(dir), "details.csv")
  read <- NULL
  rows <- readStream(streamPath(dir, "details"), 50, function(chunk) read <<- rbind(read, chunk))

  expect_equal(rows, 3)
  expect_identical(names(read), names(table))
  #A comma, a quote and a newline in a value all survive the round trip
  expect_identical(read$value, table$value)
  expect_identical(read, table)
})

test_that("a streamed table is read a chunk at a time", {
  dir <- withr::local_tempdir()
  table <- data.frame(source=rep("", 10), id=as.character(1:10), stringsAsFactors=FALSE)
  for (i in 1:10) streamTable(dir, "recordings", table[i, ])

  chunks <- list()
  rows <- readStream(streamPath(dir, "recordings"), 4, function(chunk) {
    chunks[[length(chunks) + 1]] <<- chunk
  })

  expect_equal(rows, 10)
  expect_identical(vapply(chunks, nrow, integer(1)), c(4L, 4L, 2L))
  expect_identical(unlist(lapply(chunks, `[[`, "id"), use.names=FALSE), as.character(1:10))
  #The header is written once, however many times the file is appended to
  expect_equal(sum(grepl("^\"source\"", readLines(streamPath(dir, "recordings")))), 1)
})

test_that("a whole streamed table is read in one chunk", {
  dir <- withr::local_tempdir()
  streamTable(dir, "taxa", data.frame(id=as.character(1:7), stringsAsFactors=FALSE))

  chunks <- 0
  rows <- readStream(streamPath(dir, "taxa"), -1L, function(chunk) chunks <<- chunks + 1)

  expect_equal(rows, 7)
  expect_equal(chunks, 1)
})

test_that("a streamed table that was never written has no rows", {
  dir <- withr::local_tempdir()
  called <- FALSE
  expect_equal(readStream(streamPath(dir, "links"), 10, function(chunk) called <<- TRUE), 0)
  expect_false(called)

  #A table whose every page was empty has a header and nothing else
  streamTable(dir, "links", getHeaders("links"))
  expect_equal(readStream(streamPath(dir, "links"), 10, function(chunk) called <<- TRUE), 0)
  expect_false(called)
})

test_that("streamed values keep the encoding they were harvested with", {
  dir <- withr::local_tempdir()
  name <- "Jos\u00e9 N\u00fa\u00f1ez"
  streamTable(dir, "recordings", data.frame(author=name, stringsAsFactors=FALSE))

  read <- NULL
  readStream(streamPath(dir, "recordings"), 10, function(chunk) read <<- chunk)

  expect_identical(read$author, name)
})

test_that("a streamed harvest is uploaded a chunk at a time", {
  dir <- withr::local_tempdir()
  #Three pages of two recordings, each with a detail
  for (page in 1:3) {
    ids <- as.character(c(page * 2 - 1, page * 2))
    recordings <- getHeaders("recordings")
    recordings[1:2, ] <- ""
    recordings$id <- ids
    recordings$file <- paste0("https://example.org/", ids, ".wav")
    streamTable(dir, "recordings", recordings)

    details <- getHeaders("details")
    details[1:2, ] <- ""
    details[c("type", "id", "name", "delta", "value")] <-
      list("recordings", ids, "q", "0", "A")
    streamTable(dir, "details", details)
  }

  upload <- mockUpload(function(db) uploadStreamed(db, "xeno-canto", dir, each=4))

  sql <- vapply(upload$executed, `[[`, character(1), "sql")
  #What the source gave before is removed once, not once per chunk
  deletes <- grep("^DELETE", sql, value=TRUE)
  expect_length(deletes, 1)
  expect_match(deletes, "DELETE FROM `details`", fixed=TRUE)

  #Six recordings in two chunks of four and two, six details in the same
  inserts <- grep("^INSERT", sql, value=TRUE)
  expect_true(any(grepl("INTO `recordings`", inserts, fixed=TRUE)))
  expect_true(any(grepl("INTO `details`", inserts, fixed=TRUE)))

  ids <- unlist(lapply(upload$executed[grepl("INTO `recordings`", sql, fixed=TRUE)],
                       function(e) vapply(boundRows(e), `[[`, character(1), 2)))
  expect_identical(sort(ids), as.character(1:6))
  #Every chunk names the source that harvested it
  sources <- unlist(lapply(upload$executed[grepl("INTO `recordings`", sql, fixed=TRUE)],
                           function(e) vapply(boundRows(e), `[[`, character(1), 1)))
  expect_true(all(sources == "xeno-canto"))
})

test_that("a type a harvest gave nothing of is not uploaded", {
  dir <- withr::local_tempdir()
  recordings <- getHeaders("recordings")
  recordings[1, ] <- ""
  recordings$id <- "1"
  recordings$file <- "https://example.org/1.wav"
  streamTable(dir, "recordings", recordings)

  upload <- mockUpload(function(db) uploadStreamed(db, "xeno-canto", dir))

  sql <- vapply(upload$executed, `[[`, character(1), "sql")
  #No links file, so links are neither emptied nor inserted
  expect_false(any(grepl("`links`", sql, fixed=TRUE)))
  expect_false(any(grepl("`details`", sql, fixed=TRUE)))
})

#The least a row of each type needs to survive its normaliser, so that a
#streamed chunk is not dropped before it reaches the database
streamFixture <- function(type, table) {
  if (type == "images") table$file <- paste0("https://example.org/", table$id, ".jpg")
  if (type == "descriptions") table$value <- "What it says."
  if (type == "onomatopoeia") table$word <- "zic"
  if (type == "vernacularnames") table$vernacularName <- "Field Cricket"
  if (type == "details") {
    table$type <- "recordings"
    table$name <- "q"
    table$delta <- "0"
    table$value <- "A"
  }
  if (type == "links") {
    table$subject_type <- "recordings"
    table$subject_id <- table$id
    table$predicate <- "http://purl.obolibrary.org/obo/IAO_0000136"
    table$object_type <- "taxa"
    table$object_id <- "1"
  }
  return(table)
}

test_that("a table a source replaces is emptied once, however many chunks it takes", {
  #A chunked upload that emptied its source for each chunk would keep only the
  #last one. images did exactly that: it was streamed, but uploaded with the
  #DELETE its unstreamed form does.
  replaced <- c("images", "descriptions", "onomatopoeia", "vernacularnames",
                "details", "links")
  for (type in replaced) {
    dir <- withr::local_tempdir()
    for (chunk in 1:3) {
      table <- getHeaders(type)
      table[1:2, ] <- ""
      table$source <- ""
      table$id <- as.character(c(chunk * 2 - 1, chunk * 2))
      table <- streamFixture(type, table)
      streamTable(dir, type, table)
    }

    upload <- mockUpload(function(db) uploadStreamed(db, "a-source", dir, each=2))
    sql <- vapply(upload$executed, `[[`, character(1), "sql")

    deletes <- grep(paste0("^DELETE FROM `", type, "`"), sql, value=TRUE)
    expect_identical(length(deletes), 1L, info=type)
    #And every chunk still arrives: six rows, whatever the type calls its id
    inserted <- sum(vapply(upload$executed[grepl(paste0("INTO `", type, "`"), sql, fixed=TRUE)],
                           function(e) length(boundRows(e)), integer(1)))
    expect_identical(inserted, 6L, info=type)
  }
})

test_that("every type a harvest can give can be streamed", {
  #uploadStreamed() reads streamUploads rather than the directory, so a type
  #left out of it is silently not uploaded
  harvested <- c("recordings", "taxa", "references", "traits", "images",
                 "ann-o-mate", "descriptions", "onomatopoeia", "vernacularnames",
                 "details", "links")
  expect_identical(sort(names(streamUploads)), sort(harvested))

  #References are uploaded before the records that cite them
  order <- names(streamUploads)
  for (cites in c("traits", "descriptions", "onomatopoeia", "vernacularnames", "links")) {
    expect_lt(match("references", order), match(cites, order))
  }
})

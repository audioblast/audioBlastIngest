#A harvest that would not fit in memory is written to files as it arrives and
#uploaded from them a chunk at a time, so that a source of a million recordings
#costs one page of memory to harvest and one chunk to upload rather than all of
#it at once. xeno-canto's six groups are over a million recordings, which is
#some two gigabytes of tables; streamed, the same harvest holds a page.
#
#Each type of table a harvest gives is one CSV in the directory it was given,
#in UTF-8, with its header written the first time and rows appended after that.
#Values are quoted as read.csv() reads them, so a remark with a comma, a quote
#or a line of its own in it comes back as it went in.

#The file a type of table is streamed to
streamPath <- function(dir, type) {
  return(file.path(dir, paste0(type, ".csv")))
}

#Appends a table's rows to the file of its type, which is given its header the
#first time it is written to. Returns the path.
#' @importFrom utils write.table
streamTable <- function(dir, type, table) {
  path <- streamPath(dir, type)
  started <- file.exists(path)
  if (!started) {
    dir.create(dir, showWarnings=FALSE, recursive=TRUE)
  }
  write.table(table, path, sep=",", row.names=FALSE, col.names=!started,
              append=started, qmethod="double", na="", fileEncoding="UTF-8")
  return(path)
}

#Reads a streamed table and gives it to a function a chunk of rows at a time,
#returning how many rows there were. Rows are read as text, as sources are, and
#the connection keeps its place between chunks, so the file is read once
#however many chunks it takes. An each of -1 reads the whole table, which is
#what a taxonomy needs: a taxon reaches its parent by walking the table it is
#in, so a chunk that held a species without its genus would lose the walk.
#' @importFrom utils read.csv
readStream <- function(path, each, FUN) {
  if (!file.exists(path) || file.size(path) == 0) return(invisible(0L))
  connection <- file(path, "r", encoding="UTF-8")
  on.exit(close(connection))

  header <- as.character(read.csv(connection, nrows=1, header=FALSE,
                                  colClasses="character")[1, ])
  rows <- 0L
  repeat {
    #Reading past the end of a connection is an error rather than no rows
    chunk <- tryCatch(
      read.csv(connection, nrows=each, header=FALSE, colClasses="character",
               col.names=header, check.names=FALSE),
      error=function(e) NULL)
    if (is.null(chunk) || nrow(chunk) == 0) break
    FUN(chunk)
    rows <- rows + nrow(chunk)
    if (each > 0 && nrow(chunk) < each) break
  }
  return(invisible(rows))
}

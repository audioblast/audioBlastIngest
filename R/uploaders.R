#' uploadTaxa
#'
#' Replaces the database taxa table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadTaxa <- function(db, i) {
  dbWriteTable(db, "taxa", i, overwrite=TRUE)
}

#' uploadTraits
#'
#' Replaces the database traits table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of traits to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadTraits <- function(db, i) {
  dbWriteTable(db, "traits", i, overwrite=TRUE)
}

#' uploadRecordings
#'
#' Replaces the database recordings table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of recordings to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadRecordings <- function(db, i) {
  dbWriteTable(db, "recordings", i, overwrite=TRUE)
}

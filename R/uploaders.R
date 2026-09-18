#' Upload Taxa
#'
#' Replaces the database taxa table with contents of a data frame
#'
#' @param db database connector
#' @param table data.frame of taxa to upload.
#' @export
#' @importFrom DBI dbBind dbSendQuery
uploadTaxa <- function(db, table) {
  columns <- c("source", "id", "taxon", "parent_id", "Rank", "Kingdom",
               "Subkingdom", "Phylum", "Subphylum", "Class", "Order",
               "Suborder", "Infraorder", "Superfamily", "Family", "Subfamily",
               "Tribe", "Subtribe", "Genus", "Subgenus", "Species", "Subspecies")
  uploadRows(db, "taxa", columns, table[columns], update=columns[-(1:2)])
}

#' Upload Traits
#'
#' Replaces the database traits table with contents of a data frame
#'
#' @param db database connector
#' @param table dataframe of traits to upload.
#' @export
uploadTraits <- function(db, table) {
  columns <- names(getHeaders("traits"))
  uploadRows(db, "traits", columns, table[1:13], update=columns[-(1:2)])
}

#' Upload Recordings
#'
#' Adds recordings from a data frame to the database recordings table, updating
#' recordings already in it. Values are normalised first, so that each column
#' holds one form of value whichever source a recording came from: dates are
#' ISO 8601 dates, times are 24-hour clock times, and a time that isn't a clock
#' time, such as "morning", is kept as the time of day. Values that can't be read
#' are uploaded as NULL.
#'
#' @param db database connector
#' @param table dataframe of recordings to upload.
#' @export
#' @importFrom DBI dbQuoteString dbExecute dbBind dbClearResult dbSendQuery
uploadRecordings <- function(db, table) {
  columns <- names(getHeaders("recordings"))
  uploadRows(db, "recordings", columns, normaliseRecordings(table), update=columns[-(1:2)])
}

#' Upload Deployments
#'
#' Replaces the database taxa table with contents of a data frame
#'
#' @param db database connector
#' @param table dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbSendQuery dbBind
uploadDeployments <- function(db, table) {
  columns <- names(getHeaders("deployments"))
  uploadRows(db, "deployments", columns, table[1:5], update=columns[-(1:2)])
}

uploadAnnOmate <- function(db, table) {
  columns <- names(getHeaders("ann-o-mate"))
  uploadRows(db, "annomate", columns, table[1:15], update=columns)
}

#' Upload References
#'
#' Adds references from a data frame to the database references table, updating
#' references already in it. Empty values are uploaded as NULL.
#'
#' @param db database connector
#' @param table dataframe of references to upload, e.g. from bibtexR() or
#'   referencesR().
#' @export
#' @importFrom DBI dbSendQuery dbBind dbClearResult
uploadReferences <- function(db, table) {
  columns <- names(getHeaders("references"))
  update <- columns[!columns %in% c("source", "id")]
  table <- table[, columns]
  for (column in update) {
    table[which(table[, column] == ""), column] <- NA
  }

  uploadRows(db, "references", columns, table, update)
}

#Inserts values (a data frame with a column for each of columns) into a table,
#updating the update columns of rows already there. Rows are inserted in
#batches, each by one statement in its own transaction, so a batch that fails
#is rolled back and the batches before it stay uploaded.
uploadRows <- function(db, name, columns, values, update, batch=1000) {
  rows <- seq_len(nrow(values))
  for (i in split(rows, ceiling(rows / batch))) {
    #Values are bound row by row, to match the placeholders
    params <- vector("list", length(i) * length(columns))
    for (j in seq_along(columns)) {
      params[seq(j, by=length(columns), length.out=length(i))] <- as.list(values[[j]][i])
    }
    sql <- insertSQL(name, columns, update, length(i))
    DBI::dbWithTransaction(db, dbExecute(db, sql, params=params))
  }
}

#An insert of rows into a table, updating the update columns of rows that are
#already there
insertSQL <- function(name, columns, update, rows) {
  row <- paste0("(", paste(rep("?", length(columns)), collapse=", "), ")")
  paste(
    paste0("INSERT INTO `", name, "`"),
    paste0("(", paste0("`", columns, "`", collapse=", "), ")"),
    "VALUES", paste(rep(row, rows), collapse=", "),
    "ON DUPLICATE KEY UPDATE",
    paste0("`", update, "` = VALUES(`", update, "`)", collapse=", "))
}

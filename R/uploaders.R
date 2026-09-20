#' Upload Taxa
#'
#' Adds taxa from a data frame to the database taxa table, updating taxa
#' already in it. Each taxon has a column for its own rank and for the rank of
#' every taxon above it (see taxonomiseR()). A source whose taxa don't reach
#' every rank is given the ranks it doesn't use empty, and a rank the table has
#' no column for is left out with a warning.
#'
#' @param db database connector
#' @param table data.frame of taxa to upload, as taxonomiseR() gives them.
#' @export
#' @importFrom DBI dbBind dbSendQuery
uploadTaxa <- function(db, table) {
  columns <- c("source", "id", "taxon", "parent_id", "Rank", "Kingdom",
               "Subkingdom", "Phylum", "Subphylum", "Class", "Order",
               "Suborder", "Infraorder", "Superfamily", "Family", "Subfamily",
               "Tribe", "Subtribe", "Genus", "Subgenus", "Species", "Subspecies")
  unknown <- setdiff(names(table), columns)
  if (length(unknown) > 0) {
    warning("The taxa table has no column for the rank ",
            paste(unknown, collapse=", "), ", so it is left out", call.=FALSE)
  }
  for (column in setdiff(columns, names(table))) {
    table[[column]] <- rep_len(NA_character_, nrow(table))
  }
  uploadRows(db, "taxa", columns, table[columns], update=columns[-(1:2)])
}

#' Upload Traits
#'
#' Adds traits from a data frame to the database traits table, updating traits
#' already in it. A value written as a range is uploaded with the ends of that
#' range as well, as seperatoR() reads them; a value that isn't a range has
#' none, and a value's own text is uploaded whether or not it is one.
#'
#' @param db database connector
#' @param table dataframe of traits to upload, as seperatoR() gives them.
#' @export
uploadTraits <- function(db, table) {
  columns <- names(getHeaders("traits"))
  uploadRows(db, "traits", columns, normaliseTraits(table), update=columns[-(1:2)])
}

#Normalises a data frame of traits (see uploadTraits()), so that each column
#holds one form of value whichever source a trait came from. Traits from before
#the call's part, link and qualifier were added (see linkTraits()), or from a
#source that gives no range, are given those columns. Values that can't be read
#are set to NA, which is uploaded as NULL:
#
#* min and max, the ends of the range a value is written as, are numbers, so
#  that traits can be found by the range they cover.
#
#Normalising traits that are already normalised leaves them unchanged.
normaliseTraits <- function(table) {
  for (column in c("Call.Part", "Call.Type.Link", "Call.Qualifier")) {
    if (!is.element(column, names(table))) table[[column]] <- rep_len(NA_character_, nrow(table))
    table[which(table[[column]] == ""), column] <- NA
  }
  for (column in c("min", "max")) {
    if (!is.element(column, names(table))) table[[column]] <- rep_len(NA_real_, nrow(table))
    table[[column]] <- suppressWarnings(as.numeric(table[[column]]))
  }
  return(table[names(getHeaders("traits"))])
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

#' Upload Descriptions
#'
#' Replaces the descriptions that each source gives in the database
#' descriptions table with those in a data frame. A description is what a
#' source says about something in prose, such as how a taxon behaves, with a
#' topic saying what it is of. What it is about, and the references it rests
#' on, are links, not columns.
#'
#' Descriptions with no id or nothing to say are skipped with a warning.
#' Values are made plain text, as sources often hold them as HTML. The
#' descriptions of each source in the data frame are deleted and the new ones
#' inserted in one transaction, so a description a source no longer gives is
#' removed.
#'
#' @param db database connector
#' @param table dataframe of descriptions to upload, with the columns of
#'   getHeaders("descriptions").
#' @export
#' @importFrom DBI dbExecute
uploadDescriptions <- function(db, table) {
  descriptions <- normaliseDescriptions(table)
  if (nrow(descriptions) == 0) return(invisible(NULL))

  columns <- names(getHeaders("descriptions"))
  DBI::dbWithTransaction(db, {
    for (source in unique(descriptions$source)) {
      dbExecute(db, "DELETE FROM `descriptions` WHERE `source` = ?", params=list(source))
    }
    uploadRows(db, "descriptions", columns, descriptions[columns], update=columns[-(1:2)],
               transaction=FALSE)
  })
}

#' Upload Locations
#'
#' Adds locations from a data frame to the database locations table, updating
#' locations already in it. A location is a place that records were made or
#' collected at, described once however many of them share it, in columns named
#' after the Darwin Core terms for them. Which records are of a place is a link
#' (dwciri:inDescribedPlace), not a column here. Values are normalised first,
#' so that each column holds one form of value whichever source a place came
#' from: coordinates are decimal degrees, countries are ISO 3166-1 alpha-2
#' codes, and elevations are numbers of metres. Values that can't be read are
#' uploaded as NULL.
#'
#' @param db database connector
#' @param table dataframe of locations to upload, with the columns of
#'   getHeaders("locations").
#' @export
uploadLocations <- function(db, table) {
  columns <- names(getHeaders("locations"))
  uploadRows(db, "locations", columns, normaliseLocations(table), update=columns[-(1:2)])
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

#' Upload Specimens
#'
#' Adds specimens from a data frame to the database specimens table, updating
#' specimens already in it. A specimen is the specimen or the individual that
#' a recording is of, held in columns named after the Darwin Core terms for
#' them. Values are normalised first, so that each column holds one form of
#' value whichever source a specimen came from: dates are ISO 8601 dates,
#' coordinates are decimal degrees, and remarks are plain text. Values that
#' can't be read are uploaded as NULL.
#'
#' @param db database connector
#' @param table dataframe of specimens to upload, with the columns of
#'   getHeaders("specimens").
#' @export
uploadSpecimens <- function(db, table) {
  columns <- names(getHeaders("specimens"))
  uploadRows(db, "specimens", columns, normaliseSpecimens(table), update=columns[-(1:2)])
}

#' Upload Details
#'
#' Replaces the details that each source gives in the database details table
#' with those in a data frame. A detail is one of the things a record holds
#' that has no column of its own, such as the tape a recording was made on or
#' the temperature it was made at: a value, named, with a unit where it is
#' measured, belonging to the record of a data module (its type) with an id.
#' The details a record has of one name are numbered from 0 by delta.
#'
#' Details that belong to no record, or that have no name or no value, are
#' skipped with a warning. Values are made plain text, as sources often hold
#' them as HTML. The details of each source in the data frame are deleted and
#' the new ones inserted in one transaction, so details that a source no
#' longer gives are removed. Empty units are uploaded as NULL.
#'
#' @param db database connector
#' @param table dataframe of details to upload, with the columns of
#'   getHeaders("details").
#' @export
#' @importFrom DBI dbExecute
uploadDetails <- function(db, table) {
  details <- normaliseDetails(table)
  if (nrow(details) == 0) return(invisible(NULL))
  details[which(details$unit == ""), "unit"] <- NA

  columns <- names(getHeaders("details"))
  DBI::dbWithTransaction(db, {
    for (source in unique(details$source)) {
      dbExecute(db, "DELETE FROM `details` WHERE `source` = ?", params=list(source))
    }
    uploadRows(db, "details", columns, details[columns], update=c("value", "unit"),
               transaction=FALSE)
  })
}

#Inserts values (a data frame with a column for each of columns) into a table,
#updating the update columns of rows already there. Rows are inserted in
#batches, each by one statement in its own transaction, so a batch that fails
#is rolled back and the batches before it stay uploaded. With transaction
#FALSE the batches are left to a transaction the caller has begun.
uploadRows <- function(db, name, columns, values, update, batch=1000, transaction=TRUE) {
  rows <- seq_len(nrow(values))
  for (i in split(rows, ceiling(rows / batch))) {
    #Values are bound row by row, to match the placeholders
    params <- vector("list", length(i) * length(columns))
    for (j in seq_along(columns)) {
      params[seq(j, by=length(columns), length.out=length(i))] <- as.list(values[[j]][i])
    }
    sql <- insertSQL(name, columns, update, length(i))
    if (transaction) {
      DBI::dbWithTransaction(db, dbExecute(db, sql, params=params))
    } else {
      dbExecute(db, sql, params=params)
    }
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

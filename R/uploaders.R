#' Upload Taxa
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

#' Upload Traits
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

#' Upload Recordings
#'
#' Replaces the database recordings table with contents of a data frame
#'
#' @param db database connector
#' @param table dataframe of recordings to upload.
#' @export
#' @importFrom DBI dbQuoteString dbExecute dbBind dbClearResult dbSendQuery
uploadRecordings <- function(db, table) {
  table[is.na(table)] <- 0
  #If duration is negative set to NULL
  table[which(table[,14] < 0),14] <- "NULL"
  #Set size_raw to NULL if empty
  table[which(table[,9] == ''),9] <- "NULL"

  # Prepare the SQL statement
  sql <- "INSERT INTO `recordings`
(`source`, `id`, `Title`, `taxon`, `file`, `author`,
`post_date`, `size`, `size_raw`, `type`, `NonSpecimen`,
`Date`,`Time`,`Duration`, `deployment`)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
ON DUPLICATE KEY UPDATE
`Title` = ?, `taxon` = ?, `file` = ?, `author` = ?,
`post_date`= ?, `size` = ?, `size_raw` = ?, `type` = ?,
`NonSpecimen` = ?, `Date` = ?, `Time` = ?, `Duration` = ?,
`deployment` = ?"

  # Prepare the query
  query <- dbSendQuery(db, sql)

  for (i in 1:nrow(table)) {
    # Bind the parameters
    DBI::dbBind(query, list(table[i,1], table[i,2], table[i,3], table[i,4], table[i,5],
                            table[i,6], table[i,7], table[i,8], table[i,9], table[i,10],
                            table[i,11], table[i,12], table[i,13], table[i,14], table[i,15],
                            table[i,3], table[i,4], table[i,5], table[i,6], table[i,7],
                            table[i,8], table[i,9], table[i,10], table[i,11], table[i,12],
                            table[i,13], table[i,14], table[i,15]))
  }

  # Clear the result
  dbClearResult(query)
}

#' Upload Deployments
#'
#' Replaces the database taxa table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadDeployments <- function(db, i) {
  dbWriteTable(db, "deployments", i, overwrite=TRUE)
}

#' Upload Devices
#'
#' Replaces the database devices table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadDevices <- function(db, i) {
  dbWriteTable(db, "devices", i, overwrite=TRUE)
}

#' Upload Sensors
#'
#' Replaces the database sensors table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable dbSendQuery dbFetch dbClearResult
uploadSensors <- function(db, i) {
  dbWriteTable(db, "sensors", i, overwrite=TRUE)
}

#' Upload Abiotic
#'
#' Replaces the database abiotic table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadAbiotic <- function(db, i) {
  dbWriteTable(db, "abiotic", i, overwrite=TRUE)
}

uploadAnnOmate <- function(db, i) {
  dbWriteTable(db, "annomate", i, overwrite=TRUE)
}

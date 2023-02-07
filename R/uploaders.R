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
uploadRecordings <- function(db, table) {
  #write.csv(i, file="complete_recordings.csv")
  #dbWriteTable(db, "recordings", i, overwrite=TRUE)

  table[is.na(table)] <- 0

  #If duration is negative set to NULL
  table[which(table[,14] < 0),14] <- "NULL"

  #Set size_raw to NULL if empty
  table[which(table[,9] == ''),9] <- "NULL"
  for (i in 2:nrow(table)) {
    sql <- paste0("INSERT INTO `recordings` ",
                  "(`source`, `id`, `Title`, `taxon`, `file`, `author`, ",
                  "`post_date`, `size`, `size_raw`, `type`, `NonSpecimen`, ",
                  "`Date`,`Time`,`Duration`) ",
                  "VALUES (",
                  dbQuoteString(db, table[i,1]), ", ",
                  dbQuoteString(db, table[i,2]), ", ",
                  dbQuoteString(db ,table[i,3]), ", ",
                  dbQuoteString(db, table[i,4]), ", ",
                  dbQuoteString(db, table[i,5]), ", ",
                  dbQuoteString(db, table[i,6]), ", ",
                  dbQuoteString(db, table[i,7]), ", ",
                  dbQuoteString(db, table[i,8]), ", ",
                  table[i,9], ", ",
                  dbQuoteString(db, table[i,10]), ", ",
                  dbQuoteString(db, table[i,11]), ", ",
                  dbQuoteString(db, table[i,12]), ", ",
                  dbQuoteString(db, table[i,13]), ", ",
                  table[i,14],
                  ") ON DUPLICATE KEY UPDATE ",
                  "`Title` = ", dbQuoteString(db, table[i,3]), ", ",
                  "`taxon` = ", dbQuoteString(db, table[i,4]), ", ",
                  "`file` = ", dbQuoteString(db, table[i,5]), ", ",
                  "`author` = ", dbQuoteString(db, table[i,6]), ", ",
                  "`post_date`= ", dbQuoteString(db, table[i,7]), ", ",
                  "`size` = ", dbQuoteString(db, table[i,8]), ", ",
                  "`size_raw` = ",table[i,9], ", ",
                  "`type` = ", dbQuoteString(db, table[i,10]), ", ",
                  "`NonSpecimen` = ", dbQuoteString(db, table[i,11]), ", ",
                  "`Date` = ", dbQuoteString(db, table[i,12]), ", ",
                  "`Time` = ", dbQuoteString(db, table[i,13]), ", ",
                  "`Duration` = ",table[i,14],
                  ";"
    )
    #print(sql)
    dbExecute(db, sql)
  }
}

#' uploadDeployments
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

#' uploadDeploymentLocations
#'
#' Replaces the database deployment_locations table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadDeploymentLocations <- function(db, i) {
  dbWriteTable(db, "deployment_locations", i, overwrite=TRUE)
}

#' uploadDevices
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

#' uploadSensors
#'
#' Replaces the database sensors table with contents of a data frame
#'
#' @param db database connector
#' @param i dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbConnect dbWriteTable
uploadSensors <- function(db, i) {
  dbWriteTable(db, "sensors", i, overwrite=TRUE)
}

#' uploadAbiotic
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

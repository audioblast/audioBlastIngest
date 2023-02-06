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
  #write.csv(i, file="complete_recordings.csv")
  #dbWriteTable(db, "recordings", i, overwrite=TRUE)
  fields <- head(table, 1)
  print(fields)
  for (i in 1:nrow(table))
    sql <- paste0("INSERT INTO `recordings` ",
                  "(`source`, `id`, `Title`, `taxon`, `file`, `author`, ",
                  "`post_date`, `size`, `size_raw`, `type`, `NonSpecimen`, ",
                  "`Date`,`Time`,`Duration`) ",
                  "VALUES (",
                  "'", table[i,1], "', ",
                  "'", table[i,2], "', ",
                  "'", table[i,3], "', ",
                  "'", table[i,4], "', ",
                  "'", table[i,5], "', ",
                  "'", table[i,6], "', ",
                  "'", table[i,7], "', ",
                  "'", table[i,8], "', ",
                  table[i,9], ", ",
                  "'", table[i,10], "', ",
                  "'", table[i,11], "', ",
                  "'", table[i,12], "', ",
                  "'", table[i,13], "', ",
                  table[i,14],
                  ") ON DUPLICATE UPDATE ",
                  "`Title` = '", table[i,3], "', ",
                  "`taxon` = '", table[i,4], "', ",
                  "`file` = '", table[i,5], "', ",
                  "`author` = '", table[i,6], "', ",
                  "`post_date`= '", table[i,7], "'",
                  "`size` = '", table[i,8], "',",
                  "`size_raw` = ",table[i,9], ", ",
                  "`type` = '", table[i,10], "', ",
                  "`NonSpecimen` = '", table[i,11], "', ",
                  "`Date` = '", table[i,12], "', ",
                  "`Time` = '", table[i,13], "', ",
                  "`Duration` = ",table[i,14],
                  ";"
    )
  print(sql);exit;
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

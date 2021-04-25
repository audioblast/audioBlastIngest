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

#' Upload Taxa
#'
#' Replaces the database taxa table with contents of a data frame
#'
#' @param db database connector
#' @param table data.frame of taxa to upload.
#' @export
#' @importFrom DBI dbBind dbSendQuery
uploadTaxa <- function(db, table) {
  sql <- "INSERT INTO `taxa`
    (`source`, `id`, `taxon`, `parent_id`, `Rank`, `Kingdom`, `Subkingdom`,
     `Phylum`, `Subphylum`, `Class`, `Order`, `Suborder`, `Infraorder`,
     `Superfamily`,`Family`, `Subfamily`, `Tribe`, `Subtribe`, `Genus`,
     `Subgenus`, `Species`, `Subspecies`)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON DUPLICATE KEY UPDATE
    `taxon` = ?, `parent_id` = ?, `Rank` = ?, `Kingdom` = ?,
    `Subkingdom` = ?, `Phylum` = ?, `Subphylum` = ?, `Class` = ?, `Order` = ?, `Suborder` = ?,
    `Infraorder` = ?, `Superfamily` = ?, `Family` = ?, `Subfamily` = ?, `Tribe` = ?,
    `Subtribe` = ?, `Genus` = ?, `Subgenus` = ?, `Species` = ?, `Subspecies` = ?"
  query <- dbSendQuery(db, sql)

  for (i in 1:nrow(table)) {
    dbBind(query, list(table[i,"source"], table[i,"id"], table[i,"taxon"],
                       table[i,"parent_id"], table[i,"Rank"], table[i,"Kingdom"],
                       table[i,"Subkingdom"], table[i,"Phylum"], table[i,"Subphylum"],
                       table[i,"Class"], table[i,"Order"], table[i,"Suborder"],
                       table[i,"Infraorder"], table[i,"Superfamily"], table[i,"Family"],
                       table[i,"Subfamily"], table[i,"Tribe"], table[i,"Subtribe"],
                       table[i,"Genus"], table[i,"Subgenus"], table[i,"Species"],
                       table[i,"Subspecies"],
                       table[i,"taxon"],
                       table[i,"parent_id"], table[i,"Rank"], table[i,"Kingdom"],
                       table[i,"Subkingdom"], table[i,"Phylum"], table[i,"Subphylum"],
                       table[i,"Class"], table[i,"Order"], table[i,"Suborder"],
                       table[i,"Infraorder"], table[i,"Superfamily"], table[i,"Family"],
                       table[i,"Subfamily"], table[i,"Tribe"], table[i,"Subtribe"],
                       table[i,"Genus"], table[i,"Subgenus"], table[i,"Species"],
                       table[i,"Subspecies"]))
  }
  dbClearResult(query)
}

#' Upload Traits
#'
#' Replaces the database traits table with contents of a data frame
#'
#' @param db database connector
#' @param table dataframe of traits to upload.
#' @export
uploadTraits <- function(db, table) {
  sql <- "INSERT INTO `traits`
    (`source`,`traitID`,`taxonID`,`Taxonomic.name`,`Trait`,`Ontology.Link`,
     `Value`,`Call.Type`,`Sex`,`Temperature`,`Reference`,`Cascade`,
     `Annotation.ID`)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ON DUPLICATE KEY UPDATE
    `taxonID` = ?, `Taxonomic.name` = ?, `Trait` = ?, `Ontology.Link` = ?, `Value` = ?,
    `Call.Type` = ?, `Sex` = ?, `Temperature` = ?, `Reference` = ?,
    `Cascade` = ?, `Annotation.ID` = ?"
  query <- dbSendQuery(db, sql)

  for (i in 1:nrow(table)) {
    # Bind the parameters
    DBI::dbBind(
      query,
      list(table[i,1], table[i,2], table[i,3], table[i,4], table[i,5],
           table[i,6], table[i,7], table[i,8], table[i,9], table[i,10],
           table[i,11], table[i,12], table[i,13],
           table[i,3], table[i,4], table[i,5], table[i,6], table[i,7],
           table[i,8], table[i,9], table[i,10], table[i,11], table[i,12],
           table[i,13]))
  }

  dbClearResult(query)

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
  #If duration is negative set to NULL
  table[which(table[, 14] < 0), 14] <- NA
  #Set size_raw to NULL if empty
  table[which(table[, 9] == ""), 9] <- NA

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
    DBI::dbBind(
      query,
      list(table[i,1], table[i,2], table[i,3], table[i,4], table[i,5],
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
#' @param table dataframe of taxa to upload.
#' @export
#' @importFrom DBI dbSendQuery dbBind
uploadDeployments <- function(db, table) {
  sql <- "INSERT INTO `deployments`
    (`source`, `id`, `name`, `lat`, `lon`)
    VALUES (?, ?, ?, ?, ?)
    ON DUPLICATE KEY UPDATE
    `name` = ?, `lat` = ?, `lon` = ?;"
  query <- dbSendQuery(db, sql)
  for (i in 1:nrow(table)) {
    # Bind the parameters
    DBI::dbBind(
      query,
      list(table[i,1], table[i,2], table[i,3], table[i,4], table[i,5],
           table[i,3], table[i,4], table[i,5]))
  }
  # Clear the result
  dbClearResult(query)
}

uploadAnnOmate <- function(db, table) {
  sql <- paste("INSERT INTO annomate (`source`, `source_id`, `annotator`,",
               "`annotation_id`, `annotation_date`, `annotation_info_url`,",
               "`recording_url`, `recording_info_url`, `time_start`,",
               "`time_end`, `taxon`, `type`, `lat`, `lon`, `contact`)",
               "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
               "ON DUPLICATE KEY UPDATE",
               "`source` = ?, `source_id` = ?, `annotator` = ? ,",
               "`annotation_id` = ?, `annotation_date` = ?,",
               "`annotation_info_url` = ?, `recording_url` = ? ,",
               "`recording_info_url` = ?, `time_start` = ?, `time_end` = ? ,",
               "`taxon` = ?, `type` = ?, `lat` = ?, `lon` = ?, `contact` = ?")
  query <- dbSendQuery(db, sql)
  for (i in 1:nrow(table)) {
    # Bind the parameters
    DBI::dbBind(
      query,
      list(table[i,1], table[i,2], table[i,3], table[i,4], table[i,5],
           table[i,6], table[i,7], table[i,8], table[i,9], table[i,10],
           table[i,11], table[i,12], table[i,13], table[i,14], table[i,15],
           table[i,1], table[i,2], table[i,3], table[i,4], table[i,5],
           table[i,6], table[i,7], table[i,8], table[i,9], table[i,10],
           table[i,11], table[i,12], table[i,13], table[i,14], table[i,15]))
  }
  # Clear the result
  dbClearResult(query)
}

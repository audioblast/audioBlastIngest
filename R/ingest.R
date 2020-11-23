#' ingestR
#'
#' Processes a taxonomy file in the format provided from BioAcoustica.
#'
#' @param db Database connection
#' @export
#' @importFrom utils read.csv
ingestR <- function(db) {
  #Taxa ingest
  taxa <- taxonomiseR(read.csv("https://github.com/BioAcoustica/audioblast_ingest/raw/main/taxa.txt"))

  #Traits ingest
  traits <- seperatoR(traitoR(read.csv("https://raw.githubusercontent.com/BioAcoustica/audioblast_ingest/main/traits.txt")))

  #Recordings ingest
  recordings <- read.csv("https://raw.githubusercontent.com/BioAcoustica/audioblast_ingest/main/recordings.csv")
  col_names <- c("source", colnames(recordings))
  recordings <- cbind(rep_len("bio.acousti.ca", nrow(recordings)), recordings)
  colnames(recordings) <- col_names

  #Upload
  uploadTaxa(db, taxa)
  uploadTraits(db, traits)
  uploadRecordings(db, recordings)
}

#' Get sources from audioBLAST! API
#'
#' Uses the audioBLAST! API to get a list of data sources
#'
#' @export
#' @importFrom rjson fromJSON
getSources <- function() {
  json_data <- fromJSON(file="https://api.audioblast.org/standalone/modules/list_sources/")
  sources <- list()
  for (i in 1:length(json_data$data)) {
    source <- names(json_data$data)[[i]]
    for (j in 1: length(json_data$data[[1]])) {
      row <- list(c(source, json_data$data[[i]][[j]]))
      sources <- c(row, sources)
    }
  }
  return(sources)
}

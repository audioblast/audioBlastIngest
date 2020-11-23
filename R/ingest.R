#' ingestR
#'
#' Ingest data sources for audioBLAST!
#'
#' @param db Database connection
#' @export
#' @importFrom utils read.csv
ingestR <- function(db=NULL) {
  sources <- getSources()

  #Get header files
  taxa <- getHeaders("taxa")
  traits <- getHeaders("traits")
  recordings <- getHeaders("recordings")

  for (i in 1:seq_along(sources)) {
    source <- sources[[i]]
    data <- read.csv(source$url)
    for (j in 1:seq_along(source$process)) {
      if (source$process[[j]] == "sourceR") {
        data <- sourceR(source$name, data)
      }
    }
    if (source$type == "taxa") {
      taxa <- rbind(taxa, data)
    }
    if (source$type == "recordings") {
      recordings <- rbind(recordings, data)
    }
  }

  #Traits ingest
  #traits <- seperatoR(traitoR(read.csv("https://raw.githubusercontent.com/BioAcoustica/audioblast_ingest/main/traits.txt")))

  #Upload
  if (!is.null(db)) {
    uploadRecordings(db, recordings)
    uploadTaxa(db, taxonomiseR(taxa))
    #uploadTraits(db, traits)
  }
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
      row <- list(c(name=source, json_data$data[[i]][[j]]))
      sources <- c(row, sources)
    }
  }
  return(sources)
}

getHeaders <- function(type) {
  if (type == "taxa") {
    heads <-   col_names <- c("id","taxon","Unit name 1","Unit name 2","Unit name 3","Unit name 4","Rank","parent_id","parent_taxon")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "recordings") {
    heads <-   col_names <- c("id","Title","taxon","file","author","post_date","size","size_raw","type","NonSpecimen","Date","Time","Duration")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }}

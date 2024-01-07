#' Flatten the Catalogue of Life taxonomy
#'
#' Flatten the parent-child relationships in the Catalogue of Life taxonomy
#'
#' @param data dataframe of the CoL taxonomy.
#' @param matches vector of taxa to match.
#' @param dataPreprocessed Boolean representing whether the data frame has been
#'   preprocessed by preprocessCoL().
#' @importFrom stringr str_replace str_trim
#'
#' @export
col2flat  <- function(data, matches, dataPreprocessed=F) {
  if (!dataPreprocessed) {
    data <- preprocessCoL(data)
  }
  ranks_used <- unique(as.character(data$`dwc.taxonRank`))
  ranks_used <- ranks_used[ranks_used != ""]
  col_names <- c("id","taxon","valid", "parent_id", "Rank", ranks_used)

  #Only work with maatched names
  matches <- matches[tolower(matches) %in% data$lowerName]
  num_taxa <- length(matches)

  output <- data.frame(matrix(NA, nrow=length(matches), ncol=length(col_names)))
  colnames(output) <- col_names
  output$taxon <- matches

  for (i in 1:num_taxa) {
    #ToDo: Only insert ranks used by audioblast
    print(paste(i, ":", num_taxa))
    matched_name <- tolower(as.character(output[i, "taxon"]))
    use_id <- as.character(data[data$lowerName==matched_name, "dwc.taxonID"])[1]
    name <- as.character(data[data$`dwc.taxonID`==use_id, "dwc.scientificName"])
    rank <- as.character(data[data$`dwc.taxonID`==use_id,"dwc.taxonRank"])
    if (rank != "") {
      output[i,rank] <- as.character(data[data$`dwc.taxonID`==use_id, "dwc.scientificName"])
    }
    parent_id <- as.character(data[data$`dwc.taxonID`==use_id, "dwc.parentNameUsageID"])
    while (length(parent_id) > 0) {
      parent_rank <- as.character(data[data$`dwc.taxonID`==parent_id, "dwc.taxonRank"])
      if (!identical(parent_rank, character(0))){
        parent_name <- as.character(data[data$`dwc.taxonID`==parent_id, "dwc.scientificName"])
        output[i,parent_rank] <- parent_name
      }
      parent_id <- as.character(data[data$`dwc.taxonID`==parent_id, "dwc.parentNameUsageID"])
    }
  }
  return(as.data.frame(output))
}

preprocessCoL <- function(data) {
  data$`dwc.scientificName` <- str_trim(str_replace(data$`dwc.scientificName`,paste0("\\Q",data$`dwc.scientificNameAuthorship`,"\\E"), ""))
  lower <- tolower(data$`dwc.scientificName`)
  names <- c(colnames(data),"lowerName")
  data <- cbind(data,lower)
  colnames(data) <- names
  data$`dwc.taxonomicStatus` <- replace(data$`dwc.taxonomicStatus`, data$`dwc.taxonomicStatus`!="accepted", "not accepted")
  data$`dwc.parentNameUsageID` <- paste0(data$`dwc.parentNameUsageID`, data$`dwc.acceptedNameUsageID`)
  return(data)
}

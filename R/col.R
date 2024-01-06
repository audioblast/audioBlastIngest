#' Flatten the Catalogue of Life taxonomy
#'
#' Flatten the parent-child relationships in the Catalogue of Life taxonomy
#'
#' @param data dataframe of the CoL taxonomy
#' @param matches vector of taxa to match
#' @importFrom stringr str_replace str_trim
#'
#' @export
col2flat  <- function(data, matches) {
  data <- preprocessCoL(data)
  ranks_used <- unique(as.character(data$`dwc.taxonRank`))
  ranks_used <- ranks_used[ranks_used != ""]
  col_names <- c("id","taxon","valid", "parent_id", "Rank", ranks_used)
  num_taxa <- nrow(data)
  output <- data.frame(matrix(NA, nrow=length(matches), ncol=length(col_names)))
  colnames(output) <- col_names
  output$taxon <- matches

  for (i in 1:num_taxa) {
    if (!data[i, "dwc.scientificName"] %in% matches) {
      next
    }

    name <- as.character(data[i, "dwc.scientificName"])
    rank <- as.character(data[i,"dwc.taxonRank"])
    if (rank != "") {
      output[output$taxon==name,rank] <- as.character(data[i, "dwc.scientificName"])
    }
    parent_id <- as.character(data[i, "dwc.parentNameUsageID"])
    while (length(parent_id) > 0) {
      parent_rank <- as.character(data[data$`dwc.taxonID`==parent_id, "dwc.taxonRank"])
      if (!identical(parent_rank, character(0))){
        parent_name <- as.character(data[data$`dwc.taxonID`==parent_id, "dwc.scientificName"])
        output[output$taxon==name,parent_rank] <- parent_name
      }
      parent_id <- as.character(data[data$`dwc.taxonID`==parent_id, "dwc.parentNameUsageID"])
    }
  }
  return(as.data.frame(output))
}

preprocessCoL <- function(data) {
  data$`dwc.scientificName` <- str_trim(str_replace(data$`dwc.scientificName`,paste0("\\Q",data$`dwc.scientificNameAuthorship`,"\\E"), ""))
  data$`dwc.taxonomicStatus` <- replace(data$`dwc.taxonomicStatus`, data$`dwc.taxonomicStatus`!="accepted", "not accepted")
  data$`dwc.parentNameUsageID` <- paste0(data$`dwc.parentNameUsageID`, data$`dwc.acceptedNameUsageID`)
  return(data)
}

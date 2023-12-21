#' Flatten the Catalogue of Life taxonomy
#'
#' Flatten the parent-child relationships in the Catalogue of Life taxonomy
#'
#' @param input dataframe of the CoL taxonomy
#' @importFrom stringr str_replace str_trim
#'
#' @export
col2flat  <- function(input) {

  ranks_used <- unique(as.character(input$`dwc.taxonRank`))
  ranks_used <- ranks_used[ranks_used != ""]
  col_names <- c("id","taxon","valid", "parent_id", "Rank", ranks_used)
  num_taxa <- nrow(input)
  output <- data.frame(matrix(NA, nrow=num_taxa, ncol=length(col_names)))
  colnames(output) <- col_names
  output$taxon <- str_trim(str_replace(input$`dwc.scientificName`,paste0("\\Q",input$`dwc.scientificNameAuthorship`,"\\E"), ""))
  output$valid <- input$`dwc.taxonomicStatus`
  output$valid <- replace(output$valid, output$valid!="accepted", "not accepted")
  output$id <- input$`dwc.taxonID`
  output$parent_id <- paste0(input$`dwc.parentNameUsageID`, input$`dwc.acceptedNameUsageID`)
  output$Rank <- input$`dwc.taxonRank`
  for (i in 1:num_taxa) {
    rank <- as.character(input[i,"dwc.taxonRank"])
    if (rank != "") {
      output[i,rank] <- as.character(output[i, "taxon"])
    }
    parent_id <- as.character(output[i, "parent_id"])
    while (length(parent_id) > 0) {
      parent_rank <- as.character(output[output$id==parent_id, "Rank"])
      if (!identical(parent_rank, character(0))){
        parent_name <- as.character(output[output$id==parent_id, "taxon"])
        output[i,parent_rank] <- parent_name
      }
      parent_id <- as.character(output[output$id==parent_id, "parent_id"])
    }
  }
  return(as.data.frame(output))
}

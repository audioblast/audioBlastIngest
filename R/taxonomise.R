#' taxonomiseR
#'
#' Processes a taxonomy file in the format provided from BioAcoustica.
#'
#' @param input dataframe of taxa to process.
#' @return Data frame of processed data
#' @export
#' @importFrom utils read.csv
taxonomiseR  <- function(input) {
  ranks_used <- unique(as.character(input$Rank))
  ranks_used <- ranks_used[ranks_used != ""]
  col_names <- c("source", "id","taxon","parent_id", "Rank", ranks_used)
  num_taxa <- nrow(input)
  output <- data.frame(matrix(NA, nrow=num_taxa, ncol=length(col_names)))
  colnames(output) <- col_names
  output$taxon <- input$taxon
  output$id <- input$id
  output$parent_id <- input$parent_id
  output$Rank <- input$Rank
  output$source <- rep_len("bio.acousti.ca", num_taxa)
  for (i in 1:num_taxa) {
    rank <- as.character(input[i,"Rank"])
    if (rank != "") {
      output[i,rank] <- as.character(input[i, "taxon"])
    }
    parent_id <- as.character(input[i, "parent_id"])
    while (parent_id != 0) {
      parent_rank <- as.character(output[output$id==parent_id, "Rank"])
      if (parent_rank != ""){
       parent_name <- as.character(output[output$id==parent_id, "taxon"])
       output[i,parent_rank] <- parent_name
      }
      parent_id <- as.character(output[output$id==parent_id, "parent_id"])
    }
  }
  return(output)
}


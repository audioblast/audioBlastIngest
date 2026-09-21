#' Process a taxonomy file
#'
#' Gives each taxon of a taxonomy a column for its own rank and for the rank of
#' every taxon above it, so that a taxon can be found by its family or its
#' order. A taxon's classification is read by following its parent, then its
#' parent's parent, and so on; a parent that isn't in the taxonomy, or a taxon
#' that is its own ancestor, ends the walk rather than looping. Where a rank is
#' reached twice, because a source puts a taxon inside another of its rank, the
#' nearest one keeps it, so that a taxon always names itself at its own rank.
#'
#' A rank is titled as the taxa table names its columns, so that a source
#' writing species and one writing Species are asking for the same rank rather
#' than one of them for a rank the table has no column for, and so that ranks
#' can be compared between sources without each comparison having to know that
#' they are cased differently.
#'
#' @param input dataframe of taxa to process, with the columns of
#'   getHeaders("taxa").
#' @return Data frame of processed data
#' @export
#' @importFrom stats setNames
#' @importFrom stringr str_to_title
#' @importFrom utils read.csv
taxonomiseR  <- function(input) {
  input <- as.data.frame(lapply(input, function(x) ifelse(is.na(x), "", as.character(x))),
                         stringsAsFactors=FALSE, check.names=FALSE)
  input$Rank <- str_to_title(input$Rank)
  ranks <- unique(input$Rank[input$Rank != ""])
  output <- data.frame(matrix(NA_character_, nrow=nrow(input), ncol=5 + length(ranks)),
                       stringsAsFactors=FALSE)
  colnames(output) <- c("source", "id", "taxon", "parent_id", "Rank", ranks)
  for (column in c("source", "id", "taxon", "parent_id", "Rank")) {
    output[[column]] <- input[[column]]
  }

  #Taxa are looked up by id rather than searched for, as the taxonomy is walked
  #once for each of its taxa
  parent <- setNames(input$parent_id, input$id)
  rank <- setNames(input$Rank, input$id)
  taxon <- setNames(input$taxon, input$id)
  for (i in seq_len(nrow(input))) {
    id <- input$id[i]
    seen <- character()
    while (is.element(id, names(rank)) && !is.element(id, seen)) {
      if (rank[[id]] != "" && is.na(output[i, rank[[id]]])) {
        output[i, rank[[id]]] <- taxon[[id]]
      }
      seen <- c(seen, id)
      id <- parent[[id]]
    }
  }
  return(output)
}

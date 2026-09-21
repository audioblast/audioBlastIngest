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
#' @importFrom stringr str_to_title
#' @importFrom utils read.csv
taxonomiseR  <- function(input) {
  input <- as.data.frame(lapply(input, function(x) ifelse(is.na(x), "", as.character(x))),
                         stringsAsFactors=FALSE, check.names=FALSE)
  input$Rank <- str_to_title(input$Rank)
  ranks <- unique(input$Rank[input$Rank != ""])
  columns <- c("source", "id", "taxon", "parent_id", "Rank", ranks)
  #Filled in as a matrix and made a data frame once at the end, as writing a
  #cell of a data frame rewrites the column it is in
  output <- matrix(NA_character_, nrow=nrow(input), ncol=length(columns),
                   dimnames=list(NULL, columns))
  for (column in c("source", "id", "taxon", "parent_id", "Rank")) {
    output[, column] <- input[[column]]
  }

  #The taxonomy is walked once for each of its taxa, so each lookup a walk
  #makes is made here once for all of them instead: a taxon's parent, and the
  #column its rank is named in, become a row number and a column number, or NA
  #where the taxonomy has no such taxon and the taxa table no such column. The
  #walk is then integer pointer-chasing, and NA ends it exactly where a parent
  #that isn't in the taxonomy ends it. Looking a taxon up by its id inside the
  #walk instead read the whole taxonomy at every step of every walk: 8,844 taxa
  #took 40 s that way and take 0.4 s this way, and the Catalogue of Life
  #import alone gives 18,443 of them.
  start <- match(input$id, input$id)
  parent <- match(input$parent_id, input$id)
  column <- match(input$Rank, colnames(output))
  taxon <- input$taxon
  #A taxon that is its own ancestor is walked through once and no further: a
  #row carrying this walk's own number has been reached by it before
  seen <- integer(nrow(input))
  for (i in seq_len(nrow(input))) {
    #Where rows share an id, the first of them is the taxon that id names, as
    #it is the one every parent naming that id is resolved to
    row <- start[i]
    while (!is.na(row) && seen[row] != i) {
      seen[row] <- i
      rank <- column[row]
      if (!is.na(rank) && is.na(output[i, rank])) {
        output[i, rank] <- taxon[row]
      }
      row <- parent[row]
    }
  }
  return(data.frame(output, stringsAsFactors=FALSE, check.names=FALSE))
}

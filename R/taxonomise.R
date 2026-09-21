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
#' A taxon inside nothing is given no parent, as the Catalogue of Life and
#' iNaturalist give their roots. A source whose parent column cannot be empty
#' has to write something else for it, and bio.acousti.ca writes 0; that id
#' names no taxon, so it is cleared, and the row then says what its source
#' means by it. An id that does name a taxon is never cleared. A parent the
#' taxonomy does not hold is a taxon that has gone missing rather than a tree
#' that has ended, so it is kept as the source gives it, and reported.
#'
#' @param input dataframe of taxa to process, with the columns of
#'   getHeaders("taxa").
#' @return Data frame of processed data
#' @export
#' @importFrom stringr str_to_title
#' @importFrom utils read.csv head
taxonomiseR  <- function(input) {
  input <- as.data.frame(lapply(input, function(x) ifelse(is.na(x), "", as.character(x))),
                         stringsAsFactors=FALSE, check.names=FALSE)

  #What a source writes for a taxon that is inside nothing. bio.acousti.ca
  #holds Animalia and Plantae as inside taxon 0 and has no taxon 0, which reads
  #as a parent gone missing: every bio.acousti.ca classification stopped one
  #taxon short of its root, and audioBLAST! would not give a whole
  #classification for any of them because it could not tell a root from a loss.
  #Only an id naming no taxon of this taxonomy is read this way, so a source
  #that does have a taxon 0 keeps it.
  input$parent_id[input$parent_id == "0" & !("0" %in% input$id)] <- ""

  #A parent the taxonomy does not hold is a taxon that has gone missing: the id
  #is kept, since it is what the source says, and the loss is reported rather
  #than passed off as the top of a tree. bio.acousti.ca points at terms that
  #have been deleted from it, and a classification that stops there stops for a
  #reason worth knowing.
  lost <- input$parent_id != "" & !(input$parent_id %in% input$id)
  if (any(lost)) {
    ids <- unique(input$parent_id[lost])
    warning(paste0(sum(lost), " taxa are inside a taxon the taxonomy does not hold, so their ",
                   "classification stops there, e.g. ", input$taxon[which(lost)[1]],
                   " inside ", ids[1],
                   if (length(ids) > 1) {
                     paste0(" (", length(ids), " such parents: ",
                            paste(head(ids, 5), collapse=", "),
                            if (length(ids) > 5) ", ..." else "", ")")
                   } else ""))
  }

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

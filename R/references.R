#' Read references from CSV
#'
#' Reads a CSV file of references, such as the bibliography exported from
#' BioAcoustica, into the audioBlast! references format. Columns are matched
#' to the columns of the references table by name, so they can be in any
#' order, and those that a file does not have are left empty. A file must have
#' an id column.
#'
#' Values are converted to plain text from the HTML that some sources (e.g.
#' the Drupal Biblio module) use in titles, abstracts and notes, and the soft
#' hyphens, hyphens and ligatures (e.g. U+FB02 for fl) that text copied from
#' PDFs often has are replaced with plain letters and hyphens. Authors and
#' editors are separated by semicolons, and each is read as in BibTeX, e.g.
#' `Charles Darwin`, `von Frisch, Karl` or, for a corporate name, `{World
#' Wildlife Fund}`. They are listed surname first, e.g. `Darwin, Charles; von
#' Frisch, Karl`. DOIs are given without a resolver, e.g.
#' `10.1093/database/bav054`.
#'
#' @param file Path or URL of a CSV file, encoded as UTF-8.
#' @return Data frame of references, with an empty source column (see
#'   sourceR()).
#' @examples
#' \dontrun{
#' references <- sourceR("bio.acousti.ca", referencesR("references.csv"))
#' uploadReferences(db, references)
#' }
#' @importFrom stringi stri_trans_nfc
#' @importFrom utils read.csv
#' @export
referencesR <- function(file) {
  csv <- read.csv(file, colClasses="character", encoding="UTF-8", check.names=FALSE,
                  na.strings=character(0))
  columns <- names(getHeaders("references"))
  if (!"id" %in% names(csv)) {
    stop("References have no id column")
  }
  ignored <- setdiff(names(csv), columns)
  if (length(ignored) > 0) {
    warning(paste("Ignoring columns that references do not have:", paste(ignored, collapse=", ")))
  }

  data <- as.data.frame(
    matrix("", nrow=nrow(csv), ncol=length(columns), dimnames=list(NULL, columns)),
    stringsAsFactors=FALSE)
  for (column in intersect(columns[-1], names(csv))) {
    data[[column]] <- csv[[column]]
  }

  #As in HTML, runs of white space (including line breaks) are a space
  for (column in columns[-1]) {
    value <- trimws(gsub("\\s+", " ", data[[column]]))
    if (column %in% c("author", "editor")) {
      value <- bibtexNames(value, split=";")
    } else if (!column %in% c("id", "doi", "url", "attachments", "info_url")) {
      value <- tidyText(stri_trans_nfc(html2text(value)))
    }
    data[[column]] <- value
  }
  data$doi <- sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", data$doi, ignore.case=TRUE, perl=TRUE)

  return(data)
}

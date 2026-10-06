#Normalises a data frame of details (see uploadDetails()). A detail is one of
#the things a record holds that has no column of its own: the tape a recording
#was made on, the temperature it was made at, the field notes of a specimen.
#Values are trimmed, and:
#
#* value is plain text, as sources often hold it as HTML.
#* unit is empty where the value isn't a measurement.
#* record_source is empty where the record is the giving source's own, however
#  the source wrote it, so that a record is written one way. A table from
#  before the column was added, such as a harvest streamed to files whose
#  upload failed, is given it empty.
#* The details a record has of one name are numbered from 0 by delta, in the
#  order the source gives them, so that two of them never have one number. A
#  record is named by its source, its type and its id, so a source giving
#  details of its own record 1 and of another source's record 1 numbers the
#  details of each of them apart.
#
#Details that belong to no record, or that have no name or no value, are left
#out with a warning.
#
#Normalising details that are already normalised leaves them unchanged.
#' @importFrom stats ave
normaliseDetails <- function(table) {
  columns <- names(getHeaders("details"))
  if (!is.element("record_source", names(table))) {
    table$record_source <- rep_len("", nrow(table))
  }
  details <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE)
  details$value <- html2text(details$value)
  details$record_source[details$record_source == details$source] <- ""

  usable <- details$type %in% recordTypes & details$id != "" & details$name != "" &
    details$value != ""
  if (!all(usable)) {
    warning(paste0("Skipping ", sum(!usable), " details of an unknown type, or with no record, name or value, e.g. ",
                   paste(unlist(details[which(!usable)[1], ]), collapse=" | ")))
  }
  details <- details[usable, , drop=FALSE]

  delta <- suppressWarnings(as.integer(details$delta))
  delta[is.na(delta)] <- 0L
  record <- do.call(paste, c(details[c("source", "record_source", "type", "id", "name")], sep="\n"))
  details$delta <- as.character(ave(delta, record, FUN=function(x) rank(x, ties.method="first") - 1))
  rownames(details) <- NULL
  return(details)
}

#Normalises a data frame of descriptions (see uploadDescriptions()). A
#description is what a source says about something in prose, such as how a
#taxon behaves. Values are trimmed, and:
#
#* value is plain text, as sources often hold it as HTML.
#* topic says what the description is of, e.g. behaviour, in the source's own
#  words until they are matched to vocabulary terms.
#* info_url is an http(s) URL.
#
#What a description is about, and the references it rests on, are links, so
#they are not columns here.
#
#Descriptions that have nothing to say are left out with a warning.
#
#Normalising descriptions that are already normalised leaves them unchanged.
normaliseDescriptions <- function(table) {
  columns <- names(getHeaders("descriptions"))
  descriptions <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE)
  descriptions$value <- html2text(descriptions$value)

  usable <- descriptions$id != "" & descriptions$value != ""
  if (!all(usable)) {
    warning(paste0("Skipping ", sum(!usable), " descriptions with no id or nothing to say, e.g. ",
                   paste(unlist(descriptions[which(!usable)[1], ]), collapse=" | ")))
  }
  descriptions <- descriptions[usable, , drop=FALSE]

  descriptions$info_url <- httpURL(descriptions$info_url)
  descriptions$topic[descriptions$topic == ""] <- NA
  rownames(descriptions) <- NULL
  return(descriptions)
}

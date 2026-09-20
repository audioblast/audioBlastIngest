#Normalises a data frame of images (see uploadImages()), so that each column
#holds one form of value whichever source an image came from. Values are
#trimmed, and values that can't be read are set to NA, which is uploaded as
#NULL:
#
#* file is the http(s) URL the image itself is at, and license the http(s) URL
#  of the licence it is under. A source needn't give a licence, and one that
#  gives none says nothing about what may be done with the image.
#* type is a lower case MIME type, and size_raw, width and height are numbers
#  of bytes and of pixels.
#* post_date, the date the image was uploaded, is an ISO 8601 date.
#* caption is plain text, as sources often hold it as HTML.
#
#subtype is what kind of image it is in the source's own words, such as a
#photograph or a scanning electron micrograph. What an image shows are links,
#so they are not columns here.
#
#Images with no id, or with no file to show, are left out with a warning.
#
#Normalising images that are already normalised leaves them unchanged.
normaliseImages <- function(table) {
  columns <- names(getHeaders("images"))
  for (column in setdiff(columns, names(table))) {
    table[[column]] <- rep_len("", nrow(table))
  }
  images <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE, check.names=FALSE)

  images$file <- httpURL(images$file)
  usable <- images$id != "" & !is.na(images$file)
  if (!all(usable)) {
    warning(paste0("Skipping ", sum(!usable), " images with no id or no file to show, e.g. ",
                   paste(unlist(table[which(!usable)[1], columns]), collapse=" | ")))
  }
  images <- images[usable, , drop=FALSE]

  images$license <- httpURL(images$license)
  images$type <- mimeType(images$type)
  for (column in c("size_raw", "width", "height")) {
    images[[column]] <- wholeNumber(images[[column]])
  }
  dates <- isoDate(images$post_date)
  warnUnread("images", "post_date", images$post_date, dates)
  images$post_date <- dates
  images$caption <- html2text(images$caption)
  for (column in c("title", "subtype", "creator", "caption")) {
    images[[column]][!is.na(images[[column]]) & images[[column]] == ""] <- NA
  }
  rownames(images) <- NULL
  return(images)
}

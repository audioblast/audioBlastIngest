#' date2dateAndTime
#'
#' Some sources provide datetime information in the date field, this function
#' separates them.
#'
#' @param data Data frame to process
#' @param postdate Treat the post_date field to the same treatment?
#' @importFrom parsedate parse_date
date2dateAndTime <- function(data, postdate=TRUE) {
  datetime <- parse_date(data[,"Date"])
  data[,"Date"] <- strftime(datetime, format="%Y-%m-%d", tz="UTC")
  data[,"Time"] <- strftime(datetime, format="%H%M", tz="UTC")
  if (postdate == FALSE) {
    return(data)
  }
  datetime <- parse_date(data[,"post_date"])
  data[,"post_date"] <- strftime(datetime, format="%Y-%m-%d", tz="UTC")
  return(data)
}

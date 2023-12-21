#' Convert Hz to kHz
#'
#' Append a source column to the start of a data frame
#'
#' @param data dataframe
#' @return Data frame of processed data
#' @export
hz2khz  <- function(data) {
  data[, "Value"] <- as.numeric(data[,"Value"]) / 1000
  return(data)
}

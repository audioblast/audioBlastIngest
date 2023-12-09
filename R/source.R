#' Add source column
#'
#' Append a source column to the start of a data frame
#'
#' @param source source string to append
#' @param data dataframe
#' @return Data frame of processed data
#' @export
sourceR  <- function(source, data) {
  #If source column already exists then delete
  if ("source" %in% colnames(data)) {
    data <- data[, -which(names(data) == "source")]
  }

  col_names <- c("source", colnames(data))
  data <- cbind(rep_len(source, nrow(data)), data)
  colnames(data) <- col_names
  return(data)
}

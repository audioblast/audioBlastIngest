#' Build data.frame
#'
#' Takes a column mapping and url to external data and builds a new data.frame
#' containing only matched columns
#'
#' @param source_info Single source info form getSources()
buildDataFrame <- function(source_info) {
  raw_data <- read.csv(source_info$url, colClasses = "character")
  mapped_data <- raw_data[,names(source_info$mapping)]
  colnames(mapped_data) <- source_info$mapping

  #check for source column, add if not existing
  if (!is.element("source", source_info$mapping)) {
    mapped_data <- sourceR(source_info$name, mapped_data)
  }
  return(mapped_data)
}

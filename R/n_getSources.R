#' Get sources from audioBLAST! API
#'
#' Uses the audioBLAST! API to get a list of data sources
#'
#' @importFrom rjson fromJSON
getSources <- function() {
  json_data <- fromJSON(file="https://api.audioblast.org/standalone/modules/list_sources/")
  sources <- list()
  for (i in 1:length(json_data$data)) {
    source <- names(json_data$data)[[i]]
    for (j in 1: length(json_data$data[[i]])) {
      row <- list(c(name=source, json_data$data[[i]][[j]]))
      sources <- c(row, sources)
    }
  }
  return(sources)
}

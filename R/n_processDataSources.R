#' Data source verifications
#'
#' Verifications that should be run on all data sources
#'
#' @param source_info Single source info from getSources()
verifyDataSource <- function(source_info) {
  #There must be an 'id' field
  verifyIdInMapping(source_info$mapping)
}

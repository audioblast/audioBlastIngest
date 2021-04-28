#' Verify a mapping against a column list
#'
#' Verifies that target columns used in a mapping exist in the
#' audioBLAST! database.
#'
#' @param mapping List of mappings from getSources()
#' @param columns Vector of column names to check against
verifyMapping <- function(mapping, columns) {
  #Check that all target columns exist
  for (i in 1:length(mapping)) {
    if (!is.element(mapping[[i]], columns)) {
      print(paste(mapping[[i]],"is not a valid destination column"))
    }
  }

  #Check that mappings are unique
  if (length(mapping) != length(unique(mapping))) {
    print("Multiple fields target the same column.")
  }
}

#' Verify a mapping against a column list
#'
#' Verifies that target columns used in a mapping exist in the
#' audioBLAST! database.
#'
#' @param mapping List of mappings from getSources()
verifyIdInMapping <- function(mapping) {
  if (!is.element("id", mapping)) {
      print(paste("One field must map to column 'id'"))
  }
}

findDuplicates <- function(values) {
  d <- values[duplicated(values)]
  return(d)
}

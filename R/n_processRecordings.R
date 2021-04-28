#' Recording columns
#'
#' Returns a list of recording columns
columnsRecording <- function(){
  columns <- c("source",
               "id",
               "Title",
               "taxon",
               "file",
               "author",
               "post_date",
               "size",
               "size_raw",
               "type",
               "NonSpecimen",
               "Date",
               "Time",
               "Duration"
               )
  return(columns)
}

#' Verify recording source
#'
#' Verify integrity of a recording source
#'
#' @param source_info A single source from getSources()
#' @export
verifyRecordingSource <- function(source_info) {
  print("Verifying recording source")

  #Generic test on data sources
  verifyDataSource(source_info)

  #All fields must map to an existing column
  verifyMapping(source_info$mapping, columnsRecording())

  print("--DONE--")
}

#' Verify recording data
#'
#' Verify integrity of recording data
#'
#' @param data The data frame to be verified
#' @export
verifyRecordingData <- function(data) {
  #id column should have no duplicates
  d <- findDuplicates(data[,"id"])
  if (length(d) != 0) {
    print(paste("Duplicate value found in 'id':",d))
  }

  return(data)
}

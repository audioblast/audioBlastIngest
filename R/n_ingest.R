#' ingestR
#'
#' Ingest data sources for audioBLAST!
#'
#' @param db Database connection
#' @export
#' @importFrom utils read.csv
ingestR <-function(db) {
  #List sources
  sources <- getSources()

  for (i in 10:length(sources)) {
    if (sources[[i]]$type == "recordings") {
      verifyRecordingSource(sources[[i]])
      data <- buildDataFrame(sources[[i]])
      data <- verifyRecordingData(data)
      upload(db, "recordings", c("source", "id"), data)
    }
  }
}

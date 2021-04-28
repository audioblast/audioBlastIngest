#' Upload to audioBLAST!
#'
#' Function to upload data to audioBLAST!
#'
#' @param db A database connection
#' @param table Database table to upload to
#' @param keys If these columns match existing data no INSERT will happen but an
#' UPDATE will
#' @param data A table of data to upload
#' @importFrom DBI dbExecute
#' @export
upload <- function(db, table, keys, data) {
  cols <- colnames(data)
  #TODO: Push into apply
  for (i in (nrow(data)-1):nrow(data)) {
    sql <- paste0(
      "REPLACE INTO ",
      table,
      " (`", paste(cols, collapse="`, `"), "`) ",
      "VALUES ('", paste(data[i,], collapse="', '"), "');"
    )
    dbExecute(db, sql)
  }
}

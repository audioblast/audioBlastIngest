#Runs an uploader against a mocked database, returning the statements it
#executed (each with its SQL and parameters) and the database calls it made,
#in order
mockUpload <- function(upload, ...) {
  executed <- list()
  calls <- character()
  local_mocked_bindings(
    dbExecute=function(conn, statement, params=NULL, ...) {
      executed[[length(executed) + 1]] <<- list(sql=statement, params=params)
      calls <<- c(calls, "execute")
      0L
    })
  local_mocked_bindings(
    dbWithTransaction=function(conn, code) {
      calls <<- c(calls, "begin")
      code
      calls <<- c(calls, "commit")
    },
    .package="DBI")
  upload("db", ...)
  return(list(executed=executed, calls=calls))
}

#The rows of values bound to an executed multi-row insert
boundRows <- function(executed) {
  columns <- regmatches(executed$sql, regexpr("\\([^)]*\\)", executed$sql))
  n <- lengths(gregexpr("`[^`]+`", columns))
  unname(split(executed$params, ceiling(seq_along(executed$params) / n)))
}

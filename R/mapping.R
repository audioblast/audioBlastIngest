colmap <- function(source, data){
  #Ann-o-mate allows for minimum confidence
  if (is.element("confidence", names(source))) {
    col <- source$confidence$column
    min <- source$confidence$minimum
    data <- data[which(data[[col]]> min),]
  }

  headers <- names(getHeaders(source$type))

  n <- nrow(data)
  data2 <- as.data.frame(1:n)
  for (i in seq_along(headers)) {
    col <- headers[i]
    if (is.element(col, names(data))) {
      data2 <- cbind(data2, data[, col])
    } else if (is.element(col, names(source$mapping))) {
      sourcecol <- source$mapping[which(names(source$mapping)==col)][[1]]
      coldata <- data[,which(names(data)==sourcecol)]
      data2 <- cbind(data2, coldata)
    } else {
      empty <- vector(mode="character", length=n)
      data2 <- cbind(data2, empty)
    }
  }
  data2 <- data2[,2:ncol(data2)]
  names(data2) <- headers

  #Apply override values. An override can include the values of other columns
  #of the same row, written as {column}, e.g. "https://example.org/sounds/{id}"
  if (is.element("override", names(source))) {
    for (i in 1:length(names(source$override))) {
      colname <- names(source$override)[[i]]
      override <- as.character(source$override[[i]])
      colval <- rep_len(source$override[[i]], nrow(data2))
      for (placeholder in unique(unlist(regmatches(override, gregexpr("\\{[^{}]+\\}", override))))) {
        column <- substr(placeholder, 2, nchar(placeholder) - 1)
        if (is.element(column, names(data2))) {
          values <- as.character(data2[[column]])
          colval <- vapply(seq_along(colval), function(k) gsub(placeholder, values[k], colval[k], fixed=TRUE),
                           character(1))
        }
      }
      data2[,which(names(data2)==colname)] <- colval
    }
  }


  return(data2)
}

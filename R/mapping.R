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
    if (is.element(col, names(source$mapping))) {
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

  #Apply override values
  if (is.element("override", names(source))) {
    for (i in 1:length(names(source$override))) {
      colname <- names(source$override)[[i]]
      colval <- source$override[[i]]
      data2[which(names(data2)==colname)] <- rep_len(colval, nrow(data2))
    }
  }


  return(data2)
}

seperatoR <- function(i) {
  #Split multiple values
  multi <- which(grepl(";", i$Value))
  for (i in length(multi):1) {
    row_num <- multi[[i]]
    row <- i[row_num,]
    i <- i[-c(row_num),]
    values <- strsplit(as.character(row$Value), split=";")
    for (j in 1:length(values[[1]])) {
      if (identical(values[[1]], character(0))) {next()}
      row$Value <- values[[1]][[j]]
      i <- rbind(i, row)
    }
  }

  min <- max <- rep_len(NA, nrow(i))
  i <- cbind(i, min)
  i <- cbind(i, max)

  #Turn ± (\u00B1) into ranges
  multi <- which(grepl("\u00B1", i$Value))
  for (i in length(multi):1) {
    row_num <- multi[[i]]
    row <- i[row_num,]
    values <- strsplit(as.character(row$Value), split="\u00B1")
    values <- as.numeric(values[[1]])
    if (length(values)==2) {
      i <- i[-c(row_num),]
      row$min <- values[[1]] - values[[2]]
      row$max = values[[1]] + values[[2]]

      i <- rbind(i, row)

    }
  }

  #Turn x-y into ranges
  multi <- which(grepl("-", i$Value))
  for (i in length(multi):1) {
    row_num <- multi[[i]]
    row <- i[row_num,]
    values <- strsplit(as.character(row$Value), split="-")
    values <- as.numeric(values[[1]])
    if (length(values)==2) {
      if (is.na(values[[1]] | is.na(values[[2]]))) {next()}
      i <- i[-c(row_num),]
      row$min <- values[[1]]
      row$max = values[[2]]

      i <- rbind(i, row)

    }
  }

  return(i)
}

sepaeratoR <- function(input=traitoR()) {
  #Split multiple values
  multi <- which(grepl(";", input$Value))
  for (i in length(multi):1) {
    row_num <- multi[[i]]
    row <- input[row_num,]
    input <- input[-c(row_num),]
    values <- strsplit(as.character(row$Value), split=";")
    for (j in 1:length(values[[1]])) {
      if (identical(values[[1]], character(0))) {next()}
      row$Value <- values[[1]][[j]]
      input <- rbind(input, row)
    }
  }

  min <- max <- rep_len(NA, nrow(input))
  input <- cbind(input, min)
  input <- cbind(input, max)

  #Turn ± into ranges
  multi <- which(grepl("\u00B1", input$Value))
  for (i in length(multi):1) {
    row_num <- multi[[i]]
    row <- input[row_num,]
    values <- strsplit(as.character(row$Value), split="\u00B1")
    values <- as.numeric(values[[1]])
    if (length(values)==2) {
      input <- input[-c(row_num),]
      row$min <- values[[1]] - values[[2]]
      row$max = values[[1]] + values[[2]]

      input <- rbind(input, row)

    }
  }

  #Turn x-y into ranges
  multi <- which(grepl("-", input$Value))
  for (i in length(multi):1) {
    row_num <- multi[[i]]
    row <- input[row_num,]
    values <- strsplit(as.character(row$Value), split="-")
    values <- as.numeric(values[[1]])
    if (length(values)==2) {
      if (is.na(values[[1]] | is.na(values[[2]]))) {next()}
      input <- input[-c(row_num),]
      row$min <- values[[1]]
      row$max = values[[2]]

      input <- rbind(input, row)

    }
  }

  return(input)
}

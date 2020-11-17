traitoR  <- function(input) {
  if (is.null(input)) {
    output <- read.csv("../test_data/traits.txt")
    #TODO: Provide notice data is out of date
  } else {
    #TODO: read from web
  }
  col_names <- c("source", colnames(output))
  output <- cbind(rep_len("bio.acousti.ca", nrow(output)), output)
  colnames(output) <- col_names
  return(output)
}

traitoR  <- function(output) {
  col_names <- c("source", colnames(output))
  output <- cbind(rep_len("bio.acousti.ca", nrow(output)), output)
  colnames(output) <- col_names
  return(output)
}

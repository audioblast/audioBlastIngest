birdnetVernacular <- function(data) {
  labels <- read.csv('https://raw.githubusercontent.com/kahst/BirdNET-Lite/main/model/labels.txt', sep="_", col.names =c("scientific", "vernacular"), header=F)
  for (i in 1:nrow(data)) {
    data$taxon[[i]] <-labels$scientific[[which(labels$vernacular==data$taxon[[i]])]]
  }
  return(data)
}


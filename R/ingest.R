ingest <- function(db) {
  #Taxa ingest
  taxa <- taxonomiseR(read.csv("https://github.com/BioAcoustica/audioblast_ingest/raw/main/taxa.txt"))

  #Traits ingest
  traits <- seperator(traitor(read.csv("https://raw.githubusercontent.com/BioAcoustica/audioblast_ingest/main/traits.txt")))

  #Recordings ingest
  recordings <- read.csv("https://raw.githubusercontent.com/BioAcoustica/audioblast_ingest/main/recordings.csv")
  col_names <- c("source", colnames(recordings))
  recordings <- cbind(rep_len("bio.acousti.ca", nrow(recordings)), recordings)
  colnames(recordings) <- col_names

  #Upload
  uploadTaxa(db, taxa)
  uploadTraits(db, traits)
  uploadRecordings(dv, recordings)
}

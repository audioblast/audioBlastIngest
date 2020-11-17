ingest <- function(db) {
  #Taxa ingest
  taxa <- taxonomiseR(read.csv("https://github.com/BioAcoustica/audioblast_ingest/raw/main/taxa.txt"))

  #Upload
  uploadTaxa(db, taxa)


}

#Normalises a data frame of descriptions (see uploadDescriptions()). A
#description is what a source says about something in prose, such as how a
#taxon behaves. Values are trimmed, and:
#
#* value is plain text, as sources often hold it as HTML.
#* topic says what the description is of, in the source's own words, and
#  topic_link is the Species Profile Model info item it names (see spmInfoItem()).
#* info_url is an http(s) URL.
#
#What a description is about, and the references it rests on, are links, so
#they are not columns here.
#
#Descriptions that have nothing to say are left out with a warning.
#
#Normalising descriptions that are already normalised leaves them unchanged.
normaliseDescriptions <- function(table) {
  columns <- names(getHeaders("descriptions"))
  for (column in setdiff(columns, names(table))) {
    table[[column]] <- rep_len("", nrow(table))
  }
  descriptions <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE)
  descriptions$value <- html2text(descriptions$value)

  usable <- descriptions$id != "" & descriptions$value != ""
  if (!all(usable)) {
    warning(paste0("Skipping ", sum(!usable), " descriptions with no id or nothing to say, e.g. ",
                   paste(unlist(descriptions[which(!usable)[1], ]), collapse=" | ")))
  }
  descriptions <- descriptions[usable, , drop=FALSE]

  descriptions$info_url <- httpURL(descriptions$info_url)
  descriptions$topic_link <- spmInfoItem(descriptions$topic)
  descriptions$topic[descriptions$topic == ""] <- NA
  rownames(descriptions) <- NULL
  return(descriptions)
}

#The IRI of the Species Profile Model info item that a topic names, or NA where
#it names none. A topic matches an info item however it is spaced, hyphenated
#or capitalised, a few are shortened or spelled as sources shorten and spell
#them, and one that is of a sound is of behaviour.
#
#The Species Profile Model is the standard a Scratchpads species profile
#implements, so the fields such a profile has are these info items. TDWG no
#longer develops the ontology and no longer recommends it, but nothing has
#replaced it, its concepts still carry their definitions, and they are what
#GBIF's Taxon Description extension and the Encyclopedia of Life name a
#description's topic with. A bare word names nothing at all.
spmInfoItem <- function(topic) {
  key <- gsub("[^a-z]", "", tolower(as.character(topic)))
  item <- spmInfoItems[match(key, tolower(spmInfoItems))]
  item[is.na(item)] <- unname(spmOtherWords[key[is.na(item)]])
  #The Species Profile Model defines nothing acoustic, so a topic that is of a
  #sound is of the behaviour of making it, which is the nearest thing it does
  #define. The topic itself is kept as the source's own word, so what the
  #description is of is not lost to the approximation.
  item[is.na(item) & grepl(spmAcoustic, key)] <- "Behaviour"
  return(ifelse(is.na(item), NA_character_, paste0(spm, item)))
}

#Topics that name an info item in other words: one a source shortens, and one
#it spells differently. diagnosis is what a treatment calls a diagnostic
#description, biology_ecology is Plazi's own name for a section that is both,
#and behavior is Behaviour, which the Species Profile Model spells the British
#way.
spmOtherWords <- c("diagnostic"="DiagnosticDescription",
                   "diagnosis"="DiagnosticDescription",
                   "general"="GeneralDescription",
                   "generaldescription"="GeneralDescription",
                   "biologyecology"="Biology",
                   "behavior"="Behaviour")

#Topics that are of a sound. Everything but the letters is gone by the time
#these are matched, so they are the stems that survive: bioacoustics, calling
#song, song, stridulation and the pictures of a sound. call is not among them,
#as a callus is a part of a wing.
spmAcoustic <- "song|acoustic|stridulat|sonogram|oscillogram|spectrogram"

#The namespace of the Species Profile Model's info items
spm <- "http://rs.tdwg.org/ontology/voc/SPMInfoItems#"

#Every info item the Species Profile Model defines, which are the things a
#species profile can describe
spmInfoItems <- c(
  "Associations", "Behaviour", "Biology", "Conservation", "ConservationStatus",
  "Cyclicity", "Cytology", "Description", "DiagnosticDescription", "Diseases",
  "Dispersal", "Distribution", "Ecology", "Evolution", "GeneralDescription",
  "Genetics", "Growth", "Habitat", "Key", "Legislation", "LifeCycle",
  "LifeExpectancy", "LookAlikes", "Management", "Migration", "MolecularBiology",
  "Morphology", "Physiology", "PopulationBiology", "Procedures", "Reproduction",
  "RiskStatement", "Size", "TaxonBiology", "Threats", "Trends", "TrophicStrategy",
  "Use"
)

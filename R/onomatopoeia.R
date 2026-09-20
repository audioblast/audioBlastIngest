#Normalises a data frame of onomatopoeia and imitations (see
#uploadOnomatopoeia()): the words a human language renders an animal's sound
#with, such as bow-wow for a dog barking. Values are trimmed, and:
#
#* word, locality and remarks are plain text, as sources often hold them as
#  HTML, and the whitespace within a word is a single space.
#* kind says what sort of rendering it is, in the source's own words, and
#  kind_link is the class that word names (see onomatopoeiaType()).
#* language is a BCP 47 language tag, written as the standard writes it (see
#  languageTag()). One that can't be read is set to NA, which is uploaded as
#  NULL, as is one a source doesn't give: a rendering in an unrecorded language
#  is not a rendering in none, and musical notation is in no language at all.
#* info_url is an http(s) URL.
#
#The taxon whose sound a rendering renders, and the reference it was taken
#from, are links, so they are not columns here. A rendering is about its taxon
#rather than denoting it: unlike a vernacular name, it is never read onto the
#taxon as one of the names the taxon is known by.
#
#Renderings that have no id, or no word, are left out with a warning.
#
#Normalising onomatopoeia that are already normalised leaves them unchanged.
normaliseOnomatopoeia <- function(table) {
  columns <- names(getHeaders("onomatopoeia"))
  for (column in setdiff(columns, names(table))) {
    table[[column]] <- rep_len("", nrow(table))
  }
  onomatopoeia <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE)

  onomatopoeia$word <- gsub("\\s+", " ", html2text(onomatopoeia$word))
  for (column in c("kind", "sex", "lifeStage", "locality", "remarks")) {
    onomatopoeia[[column]] <- html2text(onomatopoeia[[column]])
  }

  usable <- onomatopoeia$id != "" & onomatopoeia$word != ""
  if (!all(usable)) {
    warning(sum(!usable), " onomatopoeia have no id or no word, so they are left out",
            call.=FALSE)
  }
  onomatopoeia <- onomatopoeia[usable, , drop=FALSE]

  language <- languageTag(onomatopoeia$language)
  warnUnread("onomatopoeia", "language", onomatopoeia$language, language)
  onomatopoeia$language <- language

  onomatopoeia$info_url <- httpURL(onomatopoeia$info_url)
  onomatopoeia$kind_link <- onomatopoeiaType(onomatopoeia$kind)

  for (column in columns[-(1:2)]) {
    onomatopoeia[[column]][!is.na(onomatopoeia[[column]]) & onomatopoeia[[column]] == ""] <- NA
  }
  rownames(onomatopoeia) <- NULL
  return(onomatopoeia)
}

#The IRI of the class that a kind of rendering names, or NA where it names
#none. A kind matches however it is spaced, hyphenated or capitalised.
#
#OLiA is the only vocabulary that names these at all: Darwin Core, the GBIF
#vernacular name extension, Audiovisual Core, OntoLex-Lemon, lexinfo and every
#ontology the EBI's lookup service indexes have no term for onomatopoeia
#(checked 2026-09-20). Its OnomatopoeticWord covers both a word that imitates a
#sound (bow-wow) and one lexicalised from imitating it (bark), which differ in
#their part of speech rather than in being onomatopoeic; the source's own word
#is kept in kind, so the difference is not lost.
#
#A mnemonic ("Get the beer check", for remembering a song) and a line of
#musical notation are renderings too, but nothing names either, so they take no
#IRI rather than an invented one. See docs/vocabulary-backlog.md in
#api.audioblast.org for the term that would replace this.
onomatopoeiaType <- function(kind) {
  classes <- c("imitation"="OnomatopoeticWord",
               "onomatopoeia"="OnomatopoeticWord",
               "onomatopoeiaverb"="OnomatopoeticWord",
               "onomatopoeicword"="OnomatopoeticWord",
               "onomatopoeticword"="OnomatopoeticWord",
               "ideophone"="Ideophone")
  key <- gsub("[^a-z]", "", tolower(as.character(kind)))
  class <- unname(classes[key])
  return(ifelse(is.na(class), NA_character_, paste0(olia, class)))
}

#The namespace of OLiA's classes
olia <- "http://purl.org/olia/olia.owl#"

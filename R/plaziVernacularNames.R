#Reading the names a treatment says a taxon is known by (see plaziR()). A
#vernacular name denotes the taxon: unlike an onomatopoeia, it is read back
#onto the animal as one of its names, which is why the two are separate modules
#even when the same word is both.
#
#Two kinds of name are taken. Most sit in a section or under a heading given
#over to them, where a treatment sets them out in a regular shape: a name of
#its own, a list of "Language: name" separated by slashes, or a comma-separated
#list of other names. A few sit in prose, where a treatment says that what a
#people calls an animal imitates the sound it makes -- the Ye'kwana Tururu, the
#Malay kekek. Those are names of imitative origin, and a name is a name.

#Sections and headings given over to the names a taxon is known by
plaziNameSections <- "vernacular|common ?name"
plaziNameHeading <- "^(vernacular|common ?name|other common name|proposed common name|suggested)"

#Labels a treatment opens such a section with, which name the list rather than
#belonging to it
plaziNameLabels <- paste0("^\\s*((other |proposed |suggested |[0-9.i]+ )*",
                          "(common ?names?|vernacular names?)|names?)\\s*[.:\u2010-\u2015\u2212]\\s*")

#A name of imitative origin, said of a name rather than of a sound. The marker
#is the same one plaziOnomatopoeia() looks for; what makes this a name and not
#a rendering is that the sentence says what the animal is called.
plaziNamed <- paste0("\\b(name|called|known as|moniker|term for)\\b")

#The languages a treatment labels a name with, as the tags BCP 47 writes them.
#Only a language the treatment names is used: a name whose language is not
#given is not a name in no language, and the shape of a section is not evidence
#of one, so an unlabelled name is left without a tag rather than assumed to be
#English.
plaziLanguages <- c(
  english="en", french="fr", german="de", spanish="es", portuguese="pt",
  italian="it", dutch="nl", danish="da", swedish="sv", norwegian="no",
  finnish="fi", polish="pl", czech="cs", russian="ru", greek="el",
  turkish="tr", hungarian="hu", romanian="ro", japanese="ja", chinese="zh",
  korean="ko", thai="th", vietnamese="vi", malay="ms", indonesian="id",
  hindi="hi", arabic="ar", swahili="sw", kiswahili="sw", afrikaans="af",
  catalan="ca", basque="eu", galician="gl", welsh="cy", irish="ga",
  icelandic="is", estonian="et", latvian="lv", lithuanian="lt",
  bulgarian="bg", croatian="hr", serbian="sr", slovak="sk", slovenian="sl",
  ukrainian="uk", hebrew="he", persian="fa", nepali="ne", bengali="bn",
  tamil="ta", filipino="fil", tagalog="tl")

#The longest a vernacular name plausibly is, in words. A "name" longer than
#this is a sentence about one.
plaziNameWords <- 8

#Sentences that say where a word came from rather than what an animal is
#called. A genus's etymology reads like a list of names and is not one:
#Chatogekko is "a composite word from the Spanish and Portuguese Chato,
#derived from the Greek Platus, meaning flat ... and gekko from the Malay
#gekoq, onomatopoeic of the call of Gekko gecko". Chato, Platus, flat and
#gekoq are the origins of the genus name and one translation of it; none of
#them is a name for the gecko.
plaziWordOrigin <- paste0("\\b(etymolog|epithet|composite word|derived from|",
                          "derivation|from the (latin|greek|spanish|portuguese|malay|",
                          "arabic|sanskrit|french|german)|allud|referring to|",
                          "in reference to|named (for|after))")

#The vernacular names a treatment's XML gives, with the links that say what
#each denotes and where it came from, as list(vernacularnames=, links=).
#' @importFrom xml2 xml_attr xml_find_all xml_find_first xml_text
plaziVernacularNames <- function(document, treatment) {
  empty <- list(vernacularnames=getHeaders("vernacularnames"), links=getHeaders("links"))
  taxon <- plaziTaxonName(document)
  if (taxon == "") return(empty)
  rows <- list()
  for (paragraph in xml_find_all(document, "//paragraph")) {
    id <- plaziValue(xml_attr(paragraph, "id"))
    if (id == "") next
    section <- xml_find_first(paragraph, "ancestor::subSubSection")
    type <- if (length(section) > 0 && !is.na(section)) plaziValue(xml_attr(section, "type")) else ""
    heading <- plaziRunIn(paragraph)
    text <- plaziText(paste(xml_text(xml_find_all(paragraph, ".//text()[not(ancestor::caption)]")),
                            collapse=" "))
    if (text == "") next

    listed <- grepl(plaziNameSections, type, ignore.case=TRUE) ||
      grepl(plaziNameHeading, heading, ignore.case=TRUE)
    found <- if (listed) {
      plaziNameList(text, heading)
    } else {
      plaziNamesInProse(paragraph, text, taxon)
    }
    for (i in seq_along(found$name)) {
      rows[[length(rows) + 1]] <- data.frame(
        id=paste0(treatment$uuid, "#", id, "#", i), name=found$name[i],
        language=found$language[i], remarks=found$remarks[i], stringsAsFactors=FALSE)
    }
  }
  if (length(rows) == 0) return(empty)
  found <- do.call(rbind, rows)
  found <- found[!duplicated(tolower(paste(found$name, found$language))), , drop=FALSE]

  vernacular <- data.frame(
    source="", id=found$id, vernacularName=found$name, language=found$language,
    locality="", remarks=found$remarks, stringsAsFactors=FALSE)
  vernacular <- vernacular[names(getHeaders("vernacularnames"))]
  rownames(vernacular) <- NULL
  #A vernacular name denotes its taxon, which is what separates it from a
  #rendering of the taxon's call
  return(list(vernacularnames=vernacular,
              links=plaziAboutLinks("vernacularnames", vernacular$id, treatment,
                                    "http://purl.obolibrary.org/obo/IAO_0000219")))
}

#The names a section given over to them sets out, as name, language and
#remarks. A treatment writes them as a name of its own ("Ethiopian Wolf"), as
#"French: Loup d'Abyssinie / German: Athiopien-Wolf", or as a comma-separated
#list under a label.
plaziNameList <- function(text, heading) {
  body <- text
  if (heading != "") body <- substring(body, nchar(heading) + 1)
  body <- sub(plaziNameLabels, "", body, ignore.case=TRUE, perl=TRUE)
  #A section can carry a labelled list after the name it opens with
  parts <- unlist(strsplit(body, "\\b(Other common names|Other names)\\s*[.:]", perl=TRUE))
  names <- character(0)
  languages <- character(0)
  for (part in parts) {
    #A label marks where its name starts; the next label marks where it ends.
    #Reading up to the next colon instead would keep the label welded to it
    #wherever the scanner lost the slash between them.
    at <- gregexpr(paste0("(?<![A-Za-z])(", paste(names(plaziLanguages), collapse="|"),
                          ")\\s*:"), part, ignore.case=TRUE, perl=TRUE)[[1]]
    if (at[1] != -1) {
      starts <- as.integer(at)
      ends <- starts + attr(at, "match.length")
      stops <- c(starts[-1] - 1, nchar(part))
      for (i in seq_along(starts)) {
        label <- tolower(trimws(sub(":\\s*$", "", substr(part, starts[i], ends[i] - 1))))
        for (name in plaziNamePieces(substr(part, ends[i], stops[i]))) {
          names <- c(names, name)
          languages <- c(languages, unname(plaziLanguages[label]))
        }
      }
      next
    }
    for (name in plaziNamePieces(part)) {
      names <- c(names, name)
      languages <- c(languages, NA_character_)
    }
  }
  keep <- !is.na(names) & nzchar(names)
  return(list(name=names[keep], language=languages[keep],
              remarks=rep_len("", sum(keep))))
}

#The names in a comma-separated piece of a list, trimmed of the punctuation a
#treatment sets them with. Anything too long to be a name is a sentence about
#one and is left.
plaziNamePieces <- function(piece) {
  #What a treatment puts in brackets beside a name is the specimens and
  #papers that vouch it, or a gloss of it, and not another name. Taking it
  #out before splitting keeps the name it was attached to, which would
  #otherwise be thrown away with the citation it shares a comma with.
  repeat {
    without <- gsub("\\([^()]*\\)", " ", piece, perl=TRUE)
    if (identical(without, piece)) break
    piece <- without
  }
  names <- character(0)
  for (name in unlist(strsplit(piece, "[,/;]", perl=TRUE))) {
    name <- trimws(gsub("\\s+", " ", name))
    #A label before a name says where it is used or who uses it. It is not
    #read as either: a word before a colon that is not one of the languages
    #plaziLanguages knows may be a locality, a people or a note, and which
    #it is cannot be told from the shape.
    name <- sub("^[A-Za-z][A-Za-z \u00c0-\u024f]{0,24}\\s*:\\s*", "", name, perl=TRUE)
    name <- trimws(gsub("^[[:punct:][:space:]]+|[[:punct:][:space:]]+$", "", name))
    if (!plaziIsName(name)) next
    names <- c(names, name)
  }
  return(names)
}

#Words that make a piece of text a clause rather than a name. A vernacular
#name is a noun phrase, so a finite verb or a relative pronoun in it means a
#sentence about names has been split on its commas.
plaziClauseWords <- paste0("\\b(is|are|was|were|has|have|had|been|being|be|",
                           "which|who|whose|whom|does|do|did|can|could|may|",
                           "might|will|would|shall|should|must)\\b")

#Whether a piece of text is plausibly a name: short, made of letters, and not a
#sentence. Plazi reads a treatment off the page, so a section can also hold
#what the scanner made of it ("H\u57c3\u514be"), which is not a name either.
plaziIsName <- function(name) {
  if (nchar(name) < 2 || nchar(name) > 80) return(FALSE)
  #Latin letters and Han in one word is what the scanner made of a name
  #("H\u57c3\u514be"), not a name. A name written wholly in another script is
  #a name, so only the mixture is refused.
  if (grepl("[A-Za-z]", name) &&
      grepl("[\u3040-\u30ff\u3400-\u9fff]", name)) return(FALSE)
  words <- lengths(regmatches(name, gregexpr("\\S+", name)))
  if (words < 1 || words > plaziNameWords) return(FALSE)
  #A citation, not a name: a names section can hold the specimens and papers
  #a name is vouched by, and those carry numbers where a name does not
  if (grepl("[0-9]", name)) return(FALSE)
  #A herbarium or museum code standing alone (CAS, MO), left behind when a
  #citation was split on its commas
  if (words == 1 && grepl("^[A-Z]{2,4}$", name)) return(FALSE)
  if (grepl(plaziClauseWords, name, ignore.case=TRUE, perl=TRUE)) return(FALSE)
  letters <- nchar(gsub("[^[:alpha:]]", "", name))
  return(letters >= 2 && letters / nchar(name) > 0.6)
}

#The names a treatment gives in prose, where it says both what an animal is
#called and that the name imitates the sound it makes. Only a name the
#treatment quotes is taken: "Tamarins are called sipi (probably of
#onomatopoetic origin)" names one without marking where it begins and ends, and
#guessing at that would invent names.
#
#A sentence naming another taxon is left alone, as the name may be that one's.
plaziNamesInProse <- function(paragraph, text, taxon) {
  none <- list(name=character(0), language=character(0), remarks=character(0))
  if (!grepl(plaziRenders, text, ignore.case=TRUE, perl=TRUE)) return(none)
  others <- plaziOtherTaxa(paragraph, taxon)
  names <- character(0)
  for (sentence in plaziSentences(text)) {
    if (!grepl(plaziRenders, sentence, ignore.case=TRUE, perl=TRUE) ||
        !grepl(plaziNamed, sentence, ignore.case=TRUE, perl=TRUE)) next
    if (grepl(plaziWordOrigin, sentence, ignore.case=TRUE, perl=TRUE)) next
    if (any(vapply(others, function(name) grepl(name, sentence, fixed=TRUE), logical(1)))) next
    for (name in plaziQuoted(sentence)) {
      if (plaziIsName(name)) names <- c(names, name)
    }
  }
  if (length(names) == 0) return(none)
  return(list(name=names, language=rep_len(NA_character_, length(names)),
              remarks=rep_len(paste("The treatment says this name imitates the sound",
                                    "the taxon makes."), length(names))))
}

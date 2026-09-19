#Linking audioBlast! data to the terms of the vocabulary at vocab.audioblast.org.
#Names are matched against the terms' names and synonyms, so the mapping is
#curated in the vocabulary rather than here.

#The terms of a vocabulary at vocab.audioblast.org ("cv/callType" for a
#controlled vocabulary, or "" for the site's own terms), as their IRIs named by
#the keys (see labelKey()) of their names and synonyms
#' @importFrom curl new_handle handle_setheaders curl_fetch_memory
#' @importFrom rjson fromJSON
vocabTerms <- function(vocabulary="", site="https://vocab.audioblast.org/") {
  handle <- new_handle(useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
                       connecttimeout=30, timeout=120)
  handle_setheaders(handle, Accept="application/ld+json")
  response <- curl_fetch_memory(paste0(site, vocabulary), handle=handle)
  if (response$status_code != 200) {
    stop(paste0(site, vocabulary, " could not be read (HTTP ", response$status_code, ")"))
  }
  body <- rawToChar(response$content)
  Encoding(body) <- "UTF-8"
  return(vocabTermsJSONLD(fromJSON(body)))
}

#The terms of a vocabulary given as JSON-LD (see vocabTerms()). Deprecated terms,
#such as synonyms, are left out: their names are given as their replacements'
#synonyms (skos:altLabel). Where terms share a name, the first keeps it.
vocabTermsJSONLD <- function(json) {
  terms <- character(0)
  for (node in json[["@graph"]]) {
    if (!("skos:Concept" %in% unlist(node[["@type"]])) || isTRUE(node[["owl:deprecated"]])) next
    labels <- c(labelValues(node[["skos:prefLabel"]]), labelValues(node[["skos:altLabel"]]))
    ids <- rep_len(node[["@id"]], length(labels))
    names(ids) <- labelKey(labels)
    terms <- c(terms, ids)
  }
  return(terms[!duplicated(names(terms))])
}

#The text of a JSON-LD value, or of a list of them, each a string or an object
#with @value
labelValues <- function(values) {
  if (is.null(values)) return(character(0))
  if (!is.null(names(values))) values <- list(values)
  return(vapply(values, function(v) as.character(if (is.list(v)) v[["@value"]] else v), character(1)))
}

#The key a name is matched on: lower case, with single spaces
labelKey <- function(x) {
  return(gsub("\\s+", " ", trimws(tolower(x))))
}

#The key a call type is matched on: its label key, with a final "call" or "song"
#taken as the same word, as sources use both (e.g. Calling Call, Calling Song)
callTypeKey <- function(x) {
  return(sub("(^| )(call|song)$", "\\1song", labelKey(x)))
}

#The call types of traits as sources write them, e.g. "Courtship Call B" or
#"Calling Call (Night)", split into the type of call ("Courtship Call"), the part
#of the call a trait describes and anything else that qualifies it ("Night"),
#as a data frame with columns type, part and qualifier (NA where there is none).
#A part is a capital letter after the type ("Courtship Call B"), where anything
#after it qualifies the part ("Courtship Call B Echeme A"), or a section in
#brackets ("(section a)"). Numbered calls such as "Territorial II" and "Calling
#Song Type I" are kinds of call rather than parts, so their numbers are
#qualifiers.
parseCallType <- function(x) {
  x <- trimws(ifelse(is.na(x), "", as.character(x)))
  n <- length(x)
  part <- qualifier <- rep(NA_character_, n)

  brackets <- lapply(regmatches(x, gregexpr("\\([^()]*\\)", x)), function(b) trimws(substr(b, 2, nchar(b) - 1)))
  type <- trimws(gsub(" {2,}", " ", gsub("\\([^()]*\\)", "", x)))
  for (i in seq_len(n)) {
    section <- grepl("^section ", brackets[[i]], ignore.case=TRUE)
    if (any(section)) part[i] <- sub("^section +", "", brackets[[i]][section][1], ignore.case=TRUE)
    qualifier[i] <- joinQualifiers(qualifier[i], brackets[[i]][!section])
  }

  lettered <- regmatches(type, regexec("^(.*?) ([A-H])(?: (.+))?$", type, perl=TRUE))
  for (i in which(lengths(lettered) == 4)) {
    type[i] <- lettered[[i]][2]
    part[i] <- lettered[[i]][3]
    qualifier[i] <- joinQualifiers(lettered[[i]][4], qualifier[i])
  }

  numbered <- regmatches(type, regexec("^(.+?) ((?:Type )?(?:I{1,3}|IV))$", type, perl=TRUE))
  for (i in which(lengths(numbered) == 3)) {
    type[i] <- numbered[[i]][2]
    qualifier[i] <- joinQualifiers(numbered[[i]][3], qualifier[i])
  }

  type[type == ""] <- NA
  return(data.frame(type=type, part=part, qualifier=qualifier, stringsAsFactors=FALSE))
}

#Qualifiers joined with "; ", leaving out missing and empty ones, or NA if none
joinQualifiers <- function(...) {
  qualifiers <- unlist(list(...))
  qualifiers <- qualifiers[!is.na(qualifiers) & qualifiers != ""]
  return(if (length(qualifiers) == 0) NA_character_ else paste(qualifiers, collapse="; "))
}

#Links traits to the vocabulary at vocab.audioblast.org:
#
#* Each trait's call type is split into the type, part and qualifiers (see
#  parseCallType()), given as Call.Part and Call.Qualifier, and the type is linked
#  to its term in the Type of Call vocabulary (cv/callType) as Call.Type.Link.
#* A trait without a link to the term for what it measures (Ontology.Link) is
#  linked to the term with its name, if there is one.
#* Call patterns written without separators, e.g. "AB", are written "A:B", as the
#  Call Pattern term describes.
#
#callTypes and terms are the vocabularies' terms, as vocabTerms() gives them.
linkTraits <- function(traits, callTypes=vocabTerms("cv/callType"), terms=vocabTerms()) {
  parsed <- parseCallType(traits$Call.Type)
  traits$Call.Part <- parsed$part
  traits$Call.Qualifier <- parsed$qualifier
  names(callTypes) <- callTypeKey(names(callTypes))
  traits$Call.Type.Link <- unname(callTypes[callTypeKey(parsed$type)])

  unlinked <- is.na(traits$Ontology.Link) | trimws(traits$Ontology.Link) == ""
  byName <- unname(terms[labelKey(traits$Trait)])
  linked <- unlinked & !is.na(byName)
  traits$Ontology.Link[linked] <- byName[linked]

  pattern <- traits$Ontology.Link %in% "https://vocab.audioblast.org/CallPattern" & grepl("^[A-Z]{2,}$", traits$Value)
  traits$Value[pattern] <- vapply(strsplit(traits$Value[pattern], ""), paste, character(1), collapse=":")
  return(traits)
}

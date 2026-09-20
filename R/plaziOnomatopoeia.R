#Reading the words a treatment renders a sound with (see plaziR()). A treatment
#sometimes writes a call out: the calling song of Aglaothorax ovatus "was
#onomatopoeically described as \u201czic-zic-zic, zic-zic-zic-zic\u201d (Tinkham
#1944)". That is an onomatopoeia and not a vernacular name: it renders the
#sound the animal makes rather than naming the animal, so it is about the taxon
#and does not denote it.
#
#Only a rendering the prose marks as one is taken. Searching for the word
#onomatopoeia alone does not work: of 37 matches in 35 treatments only about
#seven render an animal's sound, and the rest are the etymology of a genus
#(gekoq, Tungara, ziriguidum) or an ethnobiological name of imitative origin
#(Tururu, kekek). Those are names, and belong to a taxon's names rather than to
#its sounds. Unmarked renderings are missed, which is the cost of not guessing.

#The phrases that mark a rendering. "Described as" on its own is not one: a
#call is described as a harsh series of double chirps without any word of it
#being quoted.
plaziRenders <- paste0("onomatopoe\\w*|(transcrib|render|writ)(ed|ten)\\s+as|",
                       "\\bspell(ed|t)\\s+as")

#A rendering renders a sound, so the sentence has to be about one. Without
#this, the etymology of a plant named for the pop of its calyx reads as a call.
plaziSounded <- paste0("\\b(call|song|sing|sung|vocali[sz]|cry|cries|chirp|note|",
                       "stridulat|sound|voice|utter|whistl)")

#The quotation marks a treatment renders a word inside, which are the
#typesetter's rather than the keyboard's
plaziQuoteMarks <- "\u201c\u201d\u2018\u2019\"'\u00ab\u00bb"

#Sections that explain a name rather than record a sound. An etymology is
#where a treatment says that a genus is named for the noise something makes
#-- gekoq for the Tokay, Tungara for a frog, "Wu Yue Ni Chang" for a catfish
#named after two states that once bordered its river. Those are the origins
#of words, and a taxon's names belong to the taxon rather than to its calls.
plaziNotRenderings <- c("etymology", "nomenclature", "vernacular_names",
                        "vernacular names", "materials_examined", "material examined",
                        "type_taxon", "reference_group", "synonymic_list")

#The same, as a treatment writes it in a run-in heading rather than a type
plaziNotRenderingHeading <- "^(etymolog|nomenclat|vernacular|common name|derivation|type material)"

#A sentence about what something is called is about a name, wherever it sits
plaziNaming <- "\\b(epithet|etymolog|named (for|after)|name (of|is|comes)|is named|toponym|means )"

#The renderings a treatment's XML gives, with the links that say what each is
#about and where it came from, as list(onomatopoeia=, links=).
#
#A rendering is attributed to the taxon the treatment treats, so a sentence
#that names another taxon is left alone: the treatment of Arthroleptis
#nyungwensis renders the call of A. schubotzi, and reading that as nyungwensis
#would put a real rendering on the wrong animal.
#' @importFrom xml2 xml_attr xml_find_all xml_find_first xml_text
plaziOnomatopoeia <- function(document, treatment) {
  empty <- list(onomatopoeia=getHeaders("onomatopoeia"), links=getHeaders("links"))
  taxon <- plaziTaxonName(document)
  if (taxon == "") return(empty)
  rows <- list()
  for (paragraph in xml_find_all(document, "//paragraph")) {
    id <- plaziValue(xml_attr(paragraph, "id"))
    if (id == "") next
    section <- xml_find_first(paragraph, "ancestor::subSubSection")
    if (length(section) > 0 && !is.na(section) &&
        tolower(plaziValue(xml_attr(section, "type"))) %in% plaziNotRenderings) next
    if (grepl(plaziNotRenderingHeading, plaziRunIn(paragraph), ignore.case=TRUE)) next
    text <- plaziText(paste(xml_text(xml_find_all(paragraph, ".//text()[not(ancestor::caption)]")),
                            collapse=" "))
    if (text == "" || !grepl(plaziRenders, text, ignore.case=TRUE, perl=TRUE)) next
    others <- plaziOtherTaxa(paragraph, taxon)
    cited <- plaziCitations(paragraph)
    n <- 0
    for (sentence in plaziSentences(text)) {
      if (!grepl(plaziRenders, sentence, ignore.case=TRUE, perl=TRUE) ||
          !grepl(plaziSounded, sentence, ignore.case=TRUE, perl=TRUE)) next
      #A sentence saying what something is called is about a name
      if (grepl(plaziNaming, sentence, ignore.case=TRUE, perl=TRUE)) next
      #A sentence naming another taxon may be rendering that one's call
      if (any(vapply(others, function(name) grepl(name, sentence, fixed=TRUE), logical(1)))) next
      for (word in plaziRenderings(sentence)) {
        n <- n + 1
        rows[[length(rows) + 1]] <- data.frame(
          id=paste0(treatment$uuid, "#", id, "#", n), word=word,
          remarks=plaziAttributed(sentence, cited), stringsAsFactors=FALSE)
      }
    }
  }
  if (length(rows) == 0) return(empty)
  found <- do.call(rbind, rows)
  found <- found[!duplicated(found$word), , drop=FALSE]

  onomatopoeia <- data.frame(
    source="", id=found$id, word=found$word, kind="onomatopoeia",
    #A treatment does not say what language it renders a call in, and a
    #rendering in an unrecorded language is not a rendering in none
    language="", sex="", lifeStage="", locality="", remarks=found$remarks,
    info_url=paste0("https://treatment.plazi.org/id/", treatment$uuid),
    kind_link="", stringsAsFactors=FALSE)
  onomatopoeia <- onomatopoeia[names(getHeaders("onomatopoeia"))]
  rownames(onomatopoeia) <- NULL
  return(list(onomatopoeia=onomatopoeia,
              links=plaziAboutLinks("onomatopoeia", onomatopoeia$id, treatment)))
}

#The words a sentence quotes. A treatment sets them in the typesetter's
#quotation marks rather than the keyboard's, and leaves the comma or full
#stop that followed the quotation inside it.
plaziQuoted <- function(text) {
  pieces <- regmatches(text, gregexpr(
    paste0("[", plaziQuoteMarks, "][^", plaziQuoteMarks, "]{2,80}[", plaziQuoteMarks, "]"),
    text, perl=TRUE))[[1]]
  words <- character(0)
  for (piece in pieces) {
    word <- trimws(substr(piece, 2, nchar(piece) - 1))
    word <- trimws(gsub("[[:space:],.;:]+$", "", word))
    if (word != "") words <- c(words, word)
  }
  return(unique(words))
}

#The words a sentence renders a sound with: what is quoted after the phrase
#that marks the rendering. What is quoted before it is being quoted for some
#other reason.
plaziRenderings <- function(sentence) {
  marker <- regexpr(plaziRenders, sentence, ignore.case=TRUE, perl=TRUE)
  if (marker == -1) return(character(0))
  #What is quoted before the marker is quoted for some other reason
  words <- plaziQuoted(substr(sentence, marker, nchar(sentence)))
  #A rendering is the sound written out, not a sentence about it
  return(words[lengths(regmatches(words, gregexpr("\\S+", words))) <= 8])
}

#The names of taxa a paragraph mentions that are not the one it treats, both as
#printed and as the markup composes them, so a sentence naming one can be found
#' @importFrom xml2 xml_attr xml_find_all xml_text
plaziOtherTaxa <- function(paragraph, taxon) {
  names <- character(0)
  for (name in xml_find_all(paragraph, ".//taxonomicName")) {
    genus <- plaziValue(xml_attr(name, "genus"))
    species <- plaziValue(xml_attr(name, "species"))
    printed <- plaziText(paste(xml_text(xml_find_all(name, ".//text()")), collapse=" "))
    full <- trimws(paste(genus, species))
    if (species == "" || full == taxon) next
    #A treatment abbreviates a genus it has already named, so both forms count
    names <- c(names, full, printed)
    if (genus != "") {
      names <- c(names, paste0(substr(genus, 1, 1), ". ", species))
    }
  }
  return(unique(names[nzchar(names) & names != taxon]))
}

#The works a paragraph cites, as a treatment prints them. A rendering is often
#someone else's, quoted from the paper that made it.
#' @importFrom xml2 xml_find_all xml_text
plaziCitations <- function(paragraph) {
  cited <- character(0)
  for (citation in xml_find_all(paragraph, ".//bibRefCitation")) {
    cited <- c(cited, plaziText(paste(xml_text(xml_find_all(citation, ".//text()")), collapse=" ")))
  }
  return(unique(cited[nzchar(cited)]))
}

#Who a sentence credits a rendering to, where it credits anyone. The reference
#itself is not audioBlast!'s to hold - the treatment is what was harvested, and
#the work it quotes was not - so the credit is kept as the treatment prints it.
plaziAttributed <- function(sentence, cited) {
  named <- cited[vapply(cited, function(work) grepl(work, sentence, fixed=TRUE), logical(1))]
  if (length(named) == 0) return("")
  return(paste0("Rendered by ", paste(named, collapse="; "),
                ", as quoted by the treatment."))
}

#The links a record of a treatment gives: it stands in some relation to the
#taxon the treatment treats -- a measurement and a rendering are about it, a
#name denotes it -- and it came from the treatment, which is a reference of
#its own
plaziAboutLinks <- function(type, ids, treatment,
                            predicate="http://purl.obolibrary.org/obo/IAO_0000136") {
  links <- getHeaders("links")
  if (length(ids) == 0) return(links)
  links <- rbind(links, data.frame(
    source="", subject_type=type, subject_source="", subject_id=ids,
    predicate=predicate,
    object_type="iri", object_source="", object_id=treatment$taxon,
    qualifier="", remarks="", reference="", stringsAsFactors=FALSE))
  links <- rbind(links, data.frame(
    source="", subject_type=type, subject_source="", subject_id=ids,
    predicate="http://purl.org/dc/terms/source",
    object_type="references", object_source="", object_id=treatment$uuid,
    qualifier="", remarks="", reference="", stringsAsFactors=FALSE))
  rownames(links) <- NULL
  return(links)
}

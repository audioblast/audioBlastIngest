#Reading the measured acoustic parameters out of a Plazi treatment (see
#plaziR()). A treatment describes a song in prose, and the numbers in that
#prose are traits: the peak frequency of a call, how long an echeme lasts, how
#many teeth a stridulatory file carries.
#
#The numbers are read from the printed text and never from Plazi's own markup.
#Plazi tags 111 of every 117 numbers in an acoustic paragraph as a
#<geoCoordinate>, so a pulse rate of 9.16 \u00b1 0.96 s-1 is asserted to be
#latitude 0.96 south, and it mangles an undescribed species into
#"Gryllus undefined-4" while printing "Gryllus sp.4" correctly. The printed
#text is right in every case measured. See docs/plazi-problems.md in
#api.audioblast.org.

#The run-in heading of a paragraph that describes a sound. A treatment types
#every section "description", so the section type does not find the acoustics;
#the bold heading that opens a paragraph does (Song, Calling song,
#Bioacoustics, Stridulatory file). A heading naming a figure or a table opens a
#caption, not a description.
plaziAcousticHeading <- "(song|call|acoustic|stridulat|bioacoustic|echeme|chirp|sound (pattern|behaviou?r))"
plaziNotHeading <- "^(fig|table|plate)"

#What a treatment measures, as the phrase that names it and the trait it is.
#Each trait is one audioBlast! already holds and one the vocabulary at
#vocab.audioblast.org defines, so linkTraits() finds its term by name; a
#parameter that has neither is left alone rather than given an invented name.
#Notably the pulse train, which is the commonest unit of structure in these
#treatments, has no term and so is not read.
#
#kind is what the parameter measures, which says both what units are credible
#for it and what to convert them to: audioBlast! holds durations in seconds and
#frequencies in kHz, whichever way a treatment writes them. A measurement whose
#unit does not suit its phrase is not that parameter and is skipped.
plaziTraitTerms <- data.frame(
  rbind(
    c("tooth density", "Stridulatory file tooth density (per mm)", "density"),
    c("(teeth|tooth) ?/ ?mm", "Stridulatory file tooth density (per mm)", "density"),
    c("(number of |n )?teeth", "Number Of Teeth On Stridulatory File", "count"),
    c("(stridulatory )?file.{0,20}length|length of.{0,20}file", "Length Of Stridulatory File", "length"),
    c("peak frequency", "Peak Frequency (kHz)", "frequency"),
    c("fundamental frequency", "Fundamental Frequency", "frequency"),
    c("echeme (repetition )?rate|echemes? (are )?(repeat|produc)", "Echeme Repetition Rate (Hz)", "rate"),
    c("(interval|silence|gap|silent).{0,30}between.{0,20}echemes|echemes?.{0,20}separated", "Echeme Interval", "time"),
    c("echemes? ?period", "Echeme Period", "time"),
    c("echeme.{0,20}(duration|last)|duration.{0,20}echeme", "Echeme Duration", "time"),
    c("syllables? per echeme|echeme.{0,30}(made up of|compris|contain|of)", "Syllables per Echeme", "count"),
    c("(interval|silence|gap|silent).{0,30}between.{0,20}syllables|syllables?.{0,20}separated", "Syllable Interval", "time"),
    c("syllable period", "Syllable Period", "time"),
    c("syllable.{0,20}duration|duration.{0,20}syllable", "Syllable Duration", "time"),
    c("duty cycle", "Duty Cycle (%)", "percent")),
  stringsAsFactors=FALSE)
names(plaziTraitTerms) <- c("phrase", "trait", "kind")

#The units each kind of parameter can be written in, and what one value in that
#unit is in the unit audioBlast! holds the parameter in
#Words that make a number a bound rather than a measurement. "peak
#frequencies as high as 23.75 kHz" says what the peak frequency did not
#exceed, which is a different claim from what it was, and the traits table
#has nowhere to record the difference. Reading it as a peak frequency would
#put a maximum and a mean in the same column with nothing to tell them
#apart, so it is left alone.
plaziBounds <- "(as (high|low|much|many|little|few) as|up to|no (more|less|fewer) than|exceed\\w*|at least|at most|maximum of|minimum of)"

#Parameters a treatment measures that audioBlast! has no trait name and the
#vocabulary no term for. They are listed so that a measurement of one is left
#alone instead of being read as whichever mapped parameter happens to sit
#nearest it in the clause (see plaziPhrase()). The pulse train is the most
#wanted of them: it is the commonest unit of call structure in these
#treatments and nothing names it.
plaziUntermed <- c("dominant frequency", "carrier frequency", "pulse[ -]?trains?",
                   "harmonics?", "duty ?cycle of", "bandwidth", "amplitude",
                   "sound pressure", "wing ?stroke")

#Units that name the parameter they measure, whatever the sentence around
#them says. "(n=6) length 3.20-3.90 mm , 81-106 teeth" gives the file length
#and then the number of teeth, and only the unit says which is which.
plaziUnitTraits <- list(
  teeth=list(trait="Number Of Teeth On Stridulatory File", kind="count"),
  "teeth/mm"=list(trait="Stridulatory file tooth density (per mm)", kind="density"),
  teethpermm=list(trait="Stridulatory file tooth density (per mm)", kind="density"))

#Units are keyed in lower case with their spaces taken out and any dash written
#as a hyphen, as plaziUnitKey() writes them, because a treatment sets the
#exponent of a rate as a superscript and may use any of the printer's dashes.
plaziUnits <- list(
  time=c(s=1, sec=1, ms=0.001, msec=0.001),
  frequency=c(khz=1, hz=0.001),
  rate=c("s-1"=1, hz=1, "min-1"=1/60),
  #A pulse train is deliberately absent: "echemes contain 38 pulse trains"
  #counts pulse trains, not syllables, and audioBlast! has no trait for it,
  #so the measurement is left rather than filed under Syllables per Echeme.
  count=c(teeth=1, syllables=1, syllable=1, notes=1, note=1,
          echemes=1, echeme=1),
  density=c("teeth/mm"=1, "tooth/mm"=1, "teethpermm"=1, "toothpermm"=1),
  length=c(mm=1, cm=10),
  percent=c("%"=1))

#The key a unit is looked up by
plaziUnitKey <- function(unit) {
  return(tolower(gsub("[\u2010-\u2015\u2212]", "-", gsub("\\s+", "", unit))))
}

#Numbers as a treatment writes them, and the units they are written in. A
#superscript sets the exponent of a rate apart from its unit, so "s-1" is
#printed "s- 1" once the markup is flattened, and the space is real.
plaziNumber <- "[0-9]+(?:[.][0-9]+)?"
plaziDash <- "[-\u2010-\u2015\u2212]"
plaziUnitNames <- paste0("kHz|Hz|ms|msec|s ?[-\u2010-\u2015\u2212] ?1|min ?[-\u2010-\u2015\u2212] ?1|s|sec|mm|cm|",
                         "teeth ?(?:/|per) ?mm|teeth|syllables?|notes?|",
                         "pulse[ -]?trains?|pulses?|echemes?|%")

#The acoustic traits a treatment's XML gives, with the links that say what each
#is about and where it came from, as list(traits=, links=).
#
#A measurement written as a mean with a spread and an observed range, e.g.
#"18.5\u00b11.9 (14.4\u201321.4) teeth/mm", is two traits and not one: the
#spread and the range are different claims about the same parameter, and
#seperatoR() can only read one of them into a value's min and max. They are
#given consecutive ids so neither is lost to the other.
#
#Sample size is not carried. The treatments give it, consistently and often
#("Stridulatory file. (n=21)"), but the traits table has no column for it and
#one was not added.
#' @importFrom xml2 xml_attr xml_find_all xml_find_first xml_text
plaziTraits <- function(document, treatment) {
  empty <- list(traits=getHeaders("traits"), links=getHeaders("links"))
  taxon <- plaziTaxonName(document)
  rows <- list()
  for (paragraph in xml_find_all(document, "//paragraph")) {
    label <- plaziRunIn(paragraph)
    if (label == "" || grepl(plaziNotHeading, label, ignore.case=TRUE) ||
        !grepl(plaziAcousticHeading, label, ignore.case=TRUE)) next
    #A caption describes a figure rather than the taxon, wherever it falls
    text <- plaziText(paste(xml_text(xml_find_all(paragraph, ".//text()[not(ancestor::caption)]")),
                            collapse=" "))
    id <- plaziValue(xml_attr(paragraph, "id"))
    if (id == "" || text == "") next
    measured <- plaziMeasurements(text, label)
    if (nrow(measured) == 0) next
    conditions <- plaziConditions(label)
    measured$id <- paste0(treatment$uuid, "#", id, "#", seq_len(nrow(measured)))
    measured$call <- conditions$call
    measured$sex <- conditions$sex
    measured$temperature <- conditions$temperature
    rows[[length(rows) + 1]] <- measured
  }
  if (length(rows) == 0) return(empty)
  measured <- do.call(rbind, rows)
  rownames(measured) <- NULL

  traits <- data.frame(
    source="", traitID=measured$id, taxonID="", Taxonomic.name=taxon,
    Trait=measured$trait, Ontology.Link="", Value=measured$value,
    Call.Type=measured$call, Sex=measured$sex, Temperature=measured$temperature,
    Reference="", Cascade="", Annotation.ID="", Call.Part="", Call.Type.Link="",
    Call.Qualifier="", min="", max="", stringsAsFactors=FALSE)
  traits <- traits[names(getHeaders("traits"))]
  rownames(traits) <- NULL
  return(list(traits=traits, links=plaziAboutLinks("traits", traits$traitID, treatment)))
}

#The run-in heading a paragraph opens with, or "" where it opens with prose. A
#heading is bold and set at the start, so a bold phrase further in (a taxon
#name, a figure number) is not one. The paragraph's own text has to begin with
#it, which a mid-paragraph emphasis does not.
#' @importFrom xml2 xml_find_all xml_find_first xml_text
plaziRunIn <- function(paragraph) {
  heading <- xml_find_first(paragraph, "./emphasis[@bold='true']")
  if (length(heading) == 0 || is.na(heading)) return("")
  label <- plaziText(paste(xml_text(xml_find_all(heading, ".//text()")), collapse=" "))
  #Only the punctuation that ends a heading is taken off. A closing bracket is
  #not: a heading gives the conditions a song was recorded under in brackets,
  #and cutting one leaves the pair unbalanced.
  label <- trimws(gsub("[-.,:;[:space:]\u2010-\u2015\u2212]+$", "", label))
  if (label == "") return("")
  opening <- plaziText(paste(xml_text(xml_find_all(paragraph, ".//text()")), collapse=" "))
  if (!startsWith(tolower(opening), tolower(label))) return("")
  return(label)
}

#The name of the taxon a treatment treats, without the authority that a
#treatment prints after it.
#
#This one is built from the markup rather than the printed text, which is the
#opposite of how the measurements are read, and deliberately. A name is printed
#however its sentence needed it, abbreviated to "A. ovatus" in running text and
#welded to its authority where Plazi lost a space, while genus and species hold
#it in full. The attributes are right except in the one case that can be
#spotted from the attribute itself -- Plazi writes an undescribed species
#printed "sp.4" as species="undefined-4" -- and there the printed text is taken
#instead, as plaziName() does for a treatment's title.
#' @importFrom xml2 xml_attr xml_find_all xml_find_first xml_text
plaziTaxonName <- function(document) {
  name <- xml_find_first(document, "//subSubSection[@type='nomenclature']//taxonomicName")
  if (length(name) == 0 || is.na(name)) name <- xml_find_first(document, "//taxonomicName")
  if (length(name) == 0 || is.na(name)) return("")
  at <- function(rank) plaziValue(xml_attr(name, rank))
  if (grepl("undefined", paste(at("species"), at("subSpecies")), fixed=TRUE)) {
    #The printed name of an undescribed species, which the attributes mangle
    return(plaziText(paste(xml_text(xml_find_all(name, ".//text()")), collapse=" ")))
  }
  #The lowest rank the name reaches, with the ranks above it that make it up
  for (ranks in list(c("genus", "species", "subSpecies"), c("genus", "species"),
                     "genus", "family", "order", "class", "phylum", "kingdom")) {
    parts <- vapply(ranks, at, character(1))
    if (all(parts != "")) return(paste(parts, collapse=" "))
  }
  return("")
}

#What a measurement is of, what it is worth and what it was measured in, for
#every measurement a paragraph holds, as a data frame of trait and value.
#
#A treatment writes several measurements into one sentence, each in a clause of
#its own ("Average echeme duration is 0.10 s, average silent interval between
#consecutive echemes is 0.20 s and average echeme period is 0.03 s"), so a
#measurement is attributed within its clause and never across one. A number
#that no clause names a trait for is not read: a stray number in a sentence
#about a song is not a measurement of it.
plaziMeasurements <- function(text, context="") {
  found <- data.frame(trait=character(0), value=character(0), sentence=character(0),
                      stringsAsFactors=FALSE)
  for (sentence in plaziSentences(text)) {
    for (clause in plaziClauses(sentence)) {
      for (measurement in plaziParse(clause)) {
        #The heading names what the paragraph measures, so it is read as if it
        #stood at the front of the clause: under "Stridulatory file." a bare
        #"length" is the file's
        phrase <- plaziUnitTraits[[measurement$unit]]
        if (is.null(phrase)) {
          phrase <- plaziPhrase(paste0(context, " ", substr(clause, 1, measurement$at - 1)))
        }
        if (is.null(phrase)) next
        scale <- plaziUnits[[phrase$kind]][measurement$unit]
        if (is.na(scale)) next
        for (value in measurement$values) {
          found[nrow(found) + 1, ] <- list(phrase$trait, plaziScale(value, scale), sentence)
        }
      }
    }
  }
  return(found)
}

#Sentences, split on the full stops that end them. A number is not the end of a
#sentence, and neither is an abbreviated genus or a figure reference.
plaziSentences <- function(text) {
  parts <- unlist(strsplit(text, "(?<=[.;])\\s+(?=[A-Z(])", perl=TRUE))
  return(parts[nzchar(trimws(parts))])
}

#The clauses of a sentence, each of which names at most one parameter. A
#treatment lists measurements with commas and "and", so those separate them;
#a comma inside brackets is part of a measurement rather than between two.
plaziClauses <- function(sentence) {
  masked <- sentence
  repeat {
    hidden <- gsub("(\\([^()]*),([^()]*\\))", "\\1\u0001\\2", masked, perl=TRUE)
    if (identical(hidden, masked)) break
    masked <- hidden
  }
  #"between 1.4 and 4.3 mm" is one measurement, so its "and" does not separate
  masked <- gsub(paste0("(", plaziNumber, ")\\s+and\\s+(?=", plaziNumber, ")"),
                 "\\1\u0001", masked, perl=TRUE)
  cuts <- gregexpr(",|;|\\band\\b", masked, perl=TRUE)[[1]]
  starts <- c(1, cuts[cuts > 0] + attr(cuts, "match.length")[cuts > 0])
  ends <- c(cuts[cuts > 0] - 1, nchar(sentence))
  clauses <- character(0)
  for (i in seq_along(starts)) {
    if (ends[i] >= starts[i]) clauses <- c(clauses, substr(sentence, starts[i], ends[i]))
  }
  return(clauses[nzchar(trimws(clauses))])
}

#The trait that the words before a number name, or NULL where they name none.
#
#The phrase nearest the number wins, because a clause names its parameter just
#before giving it. Two rules keep that from going wrong:
#
#* Where two phrases overlap, the one that starts earlier wins, as it is the
#  fuller reading: "Silent intervals between echemes last" is an interval
#  between echemes, not the duration of one, though "echemes last" sits nearer
#  the number.
#* A parameter that audioBlast! has no term for (see plaziUntermed) blocks
#  attribution rather than being ignored. Without that, "Fundamental frequency
#  was 1450-1650 Hz, dominant frequency was 2950-3600 Hz" reads the dominant
#  frequency as a second fundamental one, which is the worst kind of error: a
#  real measurement filed under the wrong parameter.
#
#A phrase further from the number than plaziReach characters names something
#else in the same clause, not this measurement.
plaziReach <- 70

plaziPhrase <- function(before) {
  found <- list()
  terms <- rbind(plaziTraitTerms,
                 data.frame(phrase=plaziUntermed, trait=NA_character_, kind=NA_character_,
                            stringsAsFactors=FALSE))
  for (i in seq_len(nrow(terms))) {
    at <- gregexpr(terms$phrase[i], before, ignore.case=TRUE, perl=TRUE)[[1]]
    if (at[1] == -1) next
    last <- length(at)
    found[[length(found) + 1]] <- list(
      at=at[last], to=at[last] + attr(at, "match.length")[last],
      trait=terms$trait[i], kind=terms$kind[i])
  }
  if (length(found) == 0) return(NULL)
  #An overlapping pair is one reading of the same words, so the fuller one wins
  keep <- list()
  for (phrase in found[order(vapply(found, function(p) p$at, numeric(1)))]) {
    if (!any(vapply(keep, function(k) phrase$at < k$to && k$at < phrase$to, logical(1)))) {
      keep[[length(keep) + 1]] <- phrase
    }
  }
  nearest <- keep[[which.max(vapply(keep, function(p) p$at, numeric(1)))]]
  if (nchar(before) - nearest$to > plaziReach) return(NULL)
  #A bound between the phrase and the number is about this measurement
  if (grepl(plaziBounds, substr(before, nearest$to, nchar(before)), ignore.case=TRUE)) {
    return(NULL)
  }
  if (is.na(nearest$trait)) return(NULL)
  return(list(trait=nearest$trait, kind=nearest$kind))
}

#The measurements in a clause: for each, where it starts, the unit it is
#written in, and the values it gives. A mean with a spread and a range beside
#it gives both, in the order they are written, as they are two claims.
plaziParse <- function(clause) {
  pattern <- paste0(
    "(?<mean>", plaziNumber, ")\\s*(?:\u00b1\\s*(?<spread>", plaziNumber, "))?",
    "\\s*(?:\\(\\s*(?:range\\s+|mean\\s+)?(?<low>", plaziNumber, ")\\s*", plaziDash,
    "\\s*(?<high>", plaziNumber, ")[^)]*\\)\\s*)?",
    "\\s*(?<unit>", plaziUnitNames, ")\\b")
  ranged <- paste0(
    "(?:between|from)?\\s*(?<low>", plaziNumber, ")\\s*(?:", plaziDash, "|to|and)\\s*",
    "(?<high>", plaziNumber, ")",
    "\\s*(?:\\(\\s*mean\\s+(?<mean>", plaziNumber, ")\\s*(?:\u00b1\\s*(?<spread>",
    plaziNumber, "))?\\s*\\))?\\s*(?<unit>", plaziUnitNames, ")\\b")
  found <- c(plaziMatches(clause, ranged), plaziMatches(clause, pattern))
  #A range and a plain number can match the same text, so the range wins and
  #anything overlapping it is dropped
  keep <- list()
  for (match in found[order(vapply(found, function(m) m$at, numeric(1)))]) {
    if (!any(vapply(keep, function(k) match$at < k$to && k$at < match$to, logical(1)))) {
      keep[[length(keep) + 1]] <- match
    }
  }
  return(keep)
}

#Every match of a pattern in a clause, as where it sits, its unit and its
#values written the way audioBlast! writes them
plaziMatches <- function(clause, pattern) {
  found <- gregexpr(pattern, clause, ignore.case=TRUE, perl=TRUE)[[1]]
  if (found[1] == -1) return(list())
  starts <- attr(found, "capture.start")
  lengths <- attr(found, "capture.length")
  names <- attr(found, "capture.names")
  matches <- list()
  for (i in seq_along(found)) {
    part <- function(name) {
      j <- match(name, names)
      if (is.na(j) || lengths[i, j] == 0) return("")
      return(substr(clause, starts[i, j], starts[i, j] + lengths[i, j] - 1))
    }
    values <- character(0)
    if (part("mean") != "") {
      values <- c(values, if (part("spread") != "") paste0(part("mean"), "\u00b1", part("spread"))
                  else part("mean"))
    }
    if (part("low") != "" && part("high") != "") {
      values <- c(values, paste0(part("low"), "-", part("high")))
    }
    if (length(values) == 0) next
    matches[[length(matches) + 1]] <- list(
      at=found[i], to=found[i] + attr(found, "match.length")[i],
      unit=plaziUnitKey(part("unit")), values=values)
  }
  return(matches)
}

#A value in the unit audioBlast! holds its parameter in. The numbers are read
#out, scaled and written back in order, rather than substituted into the text:
#replacing "1" with "0.0167" in "1-6" also rewrites what it has just written.
plaziScale <- function(value, scale) {
  if (scale == 1) return(value)
  numbers <- regmatches(value, gregexpr(plaziNumber, value))[[1]]
  scaled <- vapply(as.numeric(numbers) * scale, function(x) {
    #Enough places to keep a millisecond, without the noise of binary rounding
    format(signif(x, 6), scientific=FALSE, trim=TRUE)
  }, character(1))
  pieces <- strsplit(value, plaziNumber)[[1]]
  rebuilt <- ""
  for (i in seq_along(scaled)) {
    rebuilt <- paste0(rebuilt, if (i <= length(pieces)) pieces[i] else "", scaled[i])
  }
  if (length(pieces) > length(scaled)) {
    rebuilt <- paste0(rebuilt, paste(pieces[(length(scaled) + 1):length(pieces)], collapse=""))
  }
  return(rebuilt)
}

#The conditions a run-in heading gives beside the kind of call it names, which
#treatments set in brackets after it: "Calling song ( 2\u2642 , 30.0\u00b0C)".
#The sex is the symbol printed, the temperature the degrees, and what is left is
#the call type, which linkTraits() splits into its part and qualifiers.
plaziConditions <- function(label) {
  #The degrees are the number the C follows, with its spread if it has one,
  #and not any other number in the brackets: "(1J, 29.2\u00b11.2\u00b0C)" was
  #recorded at 29.2 degrees, not at 1
  temperature <- ""
  degrees <- regmatches(label, regexpr(
    paste0(plaziNumber, "(?:\\s*\u00b1\\s*", plaziNumber, ")?\\s*\u00b0\\s*C"),
    label, perl=TRUE))
  if (length(degrees) == 1) {
    temperature <- regmatches(degrees, regexpr(plaziNumber, degrees))
  }
  sex <- ""
  if (grepl("\u2642", label) && grepl("\u2640", label)) sex <- "Male; Female"
  else if (grepl("\u2642", label)) sex <- "Male"
  else if (grepl("\u2640", label)) sex <- "Female"
  call <- gsub("\\([^()]*\\)", "", label)
  #A heading can set the kind of call under a broader one ("Behavior. Calling
  #song"), and it is the call that the measurements are of
  call <- sub("^.*\\bbehaviou?r[.:]\\s*", "", call, ignore.case=TRUE)
  call <- trimws(gsub("[-.,:;[:space:]\u2010-\u2015\u2212]+$", "",
                      trimws(gsub("\\s{2,}", " ", call))))
  #A heading that is only the word for a sound is the kind of call it describes;
  #one that says nothing more names no type
  if (grepl("^(bioacoustics|stridulation|stridulatory file|sound)$", call, ignore.case=TRUE)) {
    call <- ""
  }
  return(list(call=call, sex=sex, temperature=temperature))
}

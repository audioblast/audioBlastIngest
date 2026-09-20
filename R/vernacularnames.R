#Language tags (BCP 47) written as the standard writes them: the language in
#lower case, a script in title case and a region in upper case, e.g. pt-br
#becomes pt-BR. A tag that isn't a language, or a script or region, is NA.
languageTag <- function(x) {
  subtags <- strsplit(trimws(as.character(x)), "-", fixed=TRUE)
  return(vapply(subtags, function(parts) {
    if (length(parts) == 0 || length(parts) > 3) {return(NA_character_)}
    if (!grepl("^[A-Za-z]{2,3}$", parts[1])) {return(NA_character_)}
    out <- tolower(parts[1])
    for (part in parts[-1]) {
      if (grepl("^[A-Za-z]{4}$", part)) {
        part <- paste0(toupper(substr(part, 1, 1)), tolower(substring(part, 2)))
      } else if (grepl("^([A-Za-z]{2}|[0-9]{3})$", part)) {
        part <- toupper(part)
      } else {
        return(NA_character_)
      }
      out <- paste(out, part, sep="-")
    }
    return(out)
  }, character(1), USE.NAMES=FALSE))
}

#Normalises a data frame of vernacular names (see uploadVernacularNames()), so
#that each column holds one form of value whichever source a name came from.
#The columns are named after the Darwin Core terms they hold. Values are
#trimmed, and:
#
#* vernacularName, locality and remarks are plain text, as sources often hold
#  them as HTML, and the whitespace within a name is a single space.
#* language is a BCP 47 language tag, written as the standard writes it (see
#  languageTag()). One that can't be read is set to NA, which is uploaded as
#  NULL, as is one a source doesn't give: many names at bio.acousti.ca never
#  had their language filled in, and a name in an unrecorded language is not a
#  name in none.
#
#A name is as its source gives it, with the article that a reference wrote it
#with (e.g. le Criquet des pins) kept, as no name at bio.acousti.ca is given
#both with one and without.
#
#Names that have no id, or nothing to name a taxon with, are left out with a
#warning.
#
#Normalising vernacular names that are already normalised leaves them
#unchanged.
normaliseVernacularNames <- function(table) {
  columns <- names(getHeaders("vernacularnames"))
  vernacular <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE)

  vernacular$vernacularName <- gsub("\\s+", " ", html2text(vernacular$vernacularName))
  for (column in c("locality", "remarks")) {
    vernacular[[column]] <- html2text(vernacular[[column]])
  }

  usable <- vernacular$id != "" & vernacular$vernacularName != ""
  if (!all(usable)) {
    warning(sum(!usable), " vernacular names have no id or no name, so they are left out",
            call.=FALSE)
  }
  vernacular <- vernacular[usable, , drop=FALSE]

  language <- languageTag(vernacular$language)
  warnUnread("vernacular names", "language", vernacular$language, language)
  vernacular$language <- language

  for (column in columns[-(1:2)]) {
    vernacular[[column]][!is.na(vernacular[[column]]) & vernacular[[column]] == ""] <- NA
  }
  rownames(vernacular) <- NULL
  return(vernacular)
}

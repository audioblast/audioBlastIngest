#' Upload Links
#'
#' Replaces the links that each source gives in the database links table with
#' those in a data frame. A link is a relationship (predicate) between a
#' subject and an object, which are each identified by their type (a data
#' module, e.g. references or taxa, term for a vocabulary term, or iri for
#' anything else with an IRI), the source that holds them and their id there.
#' A source can link records held by other sources. Where the source of a
#' record of a data module is not given, it is the source giving the link.
#'
#' Links whose types or predicate are not known, or that lack an id, are
#' skipped with a warning. Each link's id is the SHA-1 hash of its subject,
#' predicate, object, qualifier and remarks, so a link keeps its id from one
#' ingest to the next, and links that differ only in their remarks (e.g.
#' spectrograms of two call types of a taxon) are different links. Repeats of
#' a link are left out. The links of each source in the data
#' frame are deleted and the new ones inserted in one transaction, so links
#' that a source no longer gives are removed. Empty qualifiers and remarks are
#' uploaded as NULL.
#'
#' A link that gives a reference is the relationship that reference
#' establishes, so a second link says so: the first link is its subject and the
#' reference its object, by dcterms:source. A source needn't know how a link's
#' id is made to cite one, and a citation is a link like any other, found and
#' served the same way. A link with no reference gets none.
#'
#' @param db database connector
#' @param table dataframe of links to upload, with the columns of
#'   getHeaders("links").
#' @export
#' @importFrom DBI dbExecute
#' @importFrom digest digest
uploadLinks <- function(db, table) {
  links <- normaliseLinks(table)
  if (nrow(links) == 0) return(invisible(NULL))
  links <- rbind(links, citedBy(links))
  links <- links[!duplicated(links[c("source", "id")]), , drop=FALSE]
  for (column in c("qualifier", "remarks")) {
    links[which(links[[column]] == ""), column] <- NA
  }

  columns <- c("source", "id", setdiff(names(getHeaders("links"))[-1], "reference"))
  DBI::dbWithTransaction(db, {
    for (source in unique(links$source)) {
      dbExecute(db, "DELETE FROM `links` WHERE `source` = ?", params=list(source))
    }
    uploadRows(db, "links", columns, links[columns], update=columns[-(1:2)], transaction=FALSE)
  })
}

#The data modules that audioBLAST! holds records in, which links join and
#details belong to. Links are among them: a link is a record with an id of its
#own, so what established one can be said of it.
recordTypes <- c("recordings", "specimens", "traits", "taxa", "references",
                 "locations", "descriptions", "deployments", "annomate",
                 "vernacularnames", "links")

#Types of record that links can join: data modules, vocabulary terms (with
#their IRI as id) and anything else with an IRI
linkTypes <- c(recordTypes, "term", "iri")

#Relationships that links can give, beside any term of the vocabulary's
#interactions (see interactionPredicates)
linkPredicates <- c(
  "http://purl.obolibrary.org/obo/IAO_0000136", #is about
  "http://purl.obolibrary.org/obo/IAO_0000219", #denotes, a subproperty of is about
  "http://purl.org/dc/terms/isReferencedBy",
  "http://purl.org/dc/terms/source",
  "http://purl.org/dc/terms/relation",
  "http://rs.tdwg.org/ac/terms/associatedSpecimenReference",
  "http://rs.tdwg.org/dwc/iri/inDescribedPlace",
  "http://rs.tdwg.org/dwc/iri/toTaxon",
  "http://rs.tdwg.org/dwc/terms/namePublishedInID",
  "http://rs.tdwg.org/dwc/terms/nameAccordingToID",
  "http://www.w3.org/2004/02/skos/core#exactMatch"
)

#The vocabulary's interactions, which are relationships by definition: one
#taxon eats, parasitises or listens for another. They are named here by their
#namespace rather than one by one, as the vocabulary grows a term at a time and
#the list above is for the relationships that other standards name.
interactionPredicates <- "https://vocab.audioblast.org/cv/interaction#"

#The links saying which reference established each of the links that give one,
#with their own ids. The reference is the linking source's own, as a record of
#a data module is.
citedBy <- function(links) {
  cited <- links[links$reference != "", , drop=FALSE]
  if (nrow(cited) == 0) {
    return(links[0, , drop=FALSE])
  }
  citations <- data.frame(
    source=cited$source, subject_type="links", subject_source=cited$source,
    subject_id=cited$id, predicate="http://purl.org/dc/terms/source",
    object_type="references", object_source=cited$source, object_id=cited$reference,
    qualifier="", remarks="", reference="", stringsAsFactors=FALSE)
  return(normaliseLinks(citations))
}

#Trims links, fills in the sources of records that the linking source holds,
#skips links that can't be used, and gives each link its id
normaliseLinks <- function(table) {
  columns <- names(getHeaders("links"))
  #A source needn't say what established a link, so the column is added empty
  for (column in setdiff(columns, names(table))) {
    table[[column]] <- rep_len("", nrow(table))
  }
  links <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE)

  for (end in c("subject", "object")) {
    source <- paste0(end, "_source")
    own <- links[[source]] == "" & !links[[paste0(end, "_type")]] %in% c("term", "iri")
    links[[source]][own] <- links$source[own]
  }

  related <- links$predicate %in% linkPredicates |
    startsWith(links$predicate, interactionPredicates)
  usable <- links$subject_type %in% linkTypes & links$object_type %in% linkTypes &
    related & links$subject_id != "" & links$object_id != ""
  if (!all(usable)) {
    warning(paste0("Skipping ", sum(!usable), " links with unknown types or predicates, or no ids, e.g. ",
                   paste(unlist(links[which(!usable)[1], ]), collapse=" | ")))
  }
  links <- links[usable, , drop=FALSE]

  key <- do.call(paste, c(links[c("subject_type", "subject_source", "subject_id", "predicate",
                                  "object_type", "object_source", "object_id", "qualifier",
                                  "remarks")], sep="\n"))
  links$id <- vapply(key, digest, character(1), algo="sha1", serialize=FALSE, USE.NAMES=FALSE)
  links <- links[!duplicated(links[c("source", "id")]), , drop=FALSE]
  rownames(links) <- NULL
  return(links)
}

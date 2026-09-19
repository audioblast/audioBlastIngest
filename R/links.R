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
#' @param db database connector
#' @param table dataframe of links to upload, with the columns of
#'   getHeaders("links").
#' @export
#' @importFrom DBI dbExecute
#' @importFrom digest digest
uploadLinks <- function(db, table) {
  links <- normaliseLinks(table)
  if (nrow(links) == 0) return(invisible(NULL))
  for (column in c("qualifier", "remarks")) {
    links[which(links[[column]] == ""), column] <- NA
  }

  columns <- c("source", "id", names(getHeaders("links"))[-1])
  DBI::dbWithTransaction(db, {
    for (source in unique(links$source)) {
      dbExecute(db, "DELETE FROM `links` WHERE `source` = ?", params=list(source))
    }
    uploadRows(db, "links", columns, links[columns], update=columns[-(1:2)], transaction=FALSE)
  })
}

#Types of record that links can join: data modules, vocabulary terms (with
#their IRI as id) and anything else with an IRI
linkTypes <- c("recordings", "traits", "taxa", "references", "deployments", "annomate", "term", "iri")

#Relationships that links can give
linkPredicates <- c(
  "http://purl.obolibrary.org/obo/IAO_0000136", #is about
  "http://purl.org/dc/terms/isReferencedBy",
  "http://purl.org/dc/terms/source",
  "http://purl.org/dc/terms/relation",
  "http://rs.tdwg.org/dwc/terms/namePublishedInID",
  "http://rs.tdwg.org/dwc/terms/nameAccordingToID",
  "http://www.w3.org/2004/02/skos/core#exactMatch"
)

#Trims links, fills in the sources of records that the linking source holds,
#skips links that can't be used, and gives each link its id
normaliseLinks <- function(table) {
  columns <- names(getHeaders("links"))
  links <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE)

  for (end in c("subject", "object")) {
    source <- paste0(end, "_source")
    own <- links[[source]] == "" & !links[[paste0(end, "_type")]] %in% c("term", "iri")
    links[[source]][own] <- links$source[own]
  }

  usable <- links$subject_type %in% linkTypes & links$object_type %in% linkTypes &
    links$predicate %in% linkPredicates & links$subject_id != "" & links$object_id != ""
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

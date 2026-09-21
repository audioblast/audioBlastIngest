# Imports the Catalogue of Life taxa that audioBLAST!'s taxa are, and the links
# saying which of its rows each of them is.
#
# audioBLAST! holds a taxon once for every source that knows it, and a name is
# all that two such rows share. A name is not an identity: bio.acousti.ca and
# iNaturalist both hold Aepyceros, one putting it in Aepycerotinae and the other
# in Antilopinae, and nothing said they were about the same animal. Each row is
# looked up in the Catalogue of Life here and linked to the taxon it is, so rows
# that reach the same taxon are known to be one without audioBLAST! having to
# say which source is right about where it belongs.
#
# The taxa and the links are one import because they are one fact. taxonBot was
# a set of Catalogue of Life rows that nothing refreshed, and its ids have been
# rotting quietly ever since: of 25 sampled in September 2026, six had become
# synonyms of names under other genera. Rows and links written by the same
# import, against the same release, cannot drift apart like that.

#The relationship that says a row and a taxon are the same taxon
COL_EXACT_MATCH <- "http://www.w3.org/2004/02/skos/core#exactMatch"

#The ranks the Catalogue of Life ranks taxa by, as it names them. A rank is sent
#with a name because the matcher needs it to tell a genus from the family of the
#same name, and it is a filter rather than a hint: a name sent at a rank the
#taxonomy does not put it at matches nothing.
#
#A rank audioBLAST! has that is not here is not looked up at all, because a
#match would not mean what a match means for the rest. complex is iNaturalist's
#for a group of species too alike to tell apart, and the Catalogue of Life has
#no such taxon: the species of the same name is a narrower thing, and calling
#them the same would lose the difference. hybrid is not a taxon either, and
#stateofmatter is iNaturalist's root above every kingdom.
COL_RANKS <- c("domain", "kingdom", "subkingdom", "infrakingdom", "superphylum",
               "phylum", "subphylum", "infraphylum", "superclass", "class",
               "subclass", "infraclass", "subterclass", "superorder", "order",
               "suborder", "infraorder", "parvorder", "superfamily", "epifamily",
               "family", "subfamily", "infrafamily", "supertribe", "tribe",
               "subtribe", "infratribe", "genus", "subgenus", "species",
               "subspecies", "variety", "form")

#How the matcher can answer that the name it was given is the name of the taxon
#it found. Everything else is a refusal or a near miss, and two of them answer
#with match TRUE while meaning nothing of the kind:
#
#  ambiguous   the name is a homonym and it cannot tell which was meant, so it
#              returns one of them. Morus on its own comes back as the mulberry,
#              and a gannet linked to a tree is worse than a gannet linked to
#              nothing.
#  higherrank  it found nothing of the name and fell back to something the name
#              was sent under, so Cryptospiza reichenovii sent with the family
#              Cicadidae comes back as the family Cicadidae.
#  none        it found nothing.
COL_MATCHED <- c("exact", "variant", "canonical")

#The statuses a name can have and still say which taxon a row is of. A synonym
#says so, and is followed to the taxon it is a synonym of, so that a source
#calling a whale Eschrichtius gibbosus and one calling it Eschrichtius robustus
#reach the same taxon. An ambiguous synonym is a synonym of more than one taxon
#and a misapplied name is a name used for a taxon it does not belong to, and
#neither says which taxon a row is of.
COL_ACCEPTED <- c("accepted", "provisionally accepted")

#The ranks of the classification audioBLAST! gives with a taxon, narrowest
#first. The narrowest one a row has is sent with its name, which is how the
#matcher tells a homonym apart: Morus is a mulberry and a gannet, and only the
#classification says which one a row means. A classification that disagrees with
#the taxonomy's costs a request, not a match, because the name is sent again
#without it.
COL_HINTS <- c("family", "order", "class", "kingdom")

#' The taxa of the Catalogue of Life that audioBLAST!'s taxa are
#'
#' Looks each taxon audioBLAST! holds up in the Catalogue of Life and gives the
#' taxa it found, their ancestors, and the links saying which row is which
#' taxon. Two rows linked to the same taxon are the same taxon, whichever
#' sources hold them and whatever those sources make of its classification.
#'
#' Nothing a source gives is changed or chosen between. Each row keeps its own
#' name and its own classification; the links say only which rows are about the
#' same animal.
#'
#' The taxa given are the taxa matched and every taxon above them, so the
#' classification is a tree that can be walked rather than the nine ranks the
#' taxa table has columns for. Acridoidea is a superfamily and has no column,
#' and iNaturalist and bio.acousti.ca cannot place it; its parent_id can.
#'
#' A match is taken only where the Catalogue of Life is saying which taxon a
#' name is of. The taxa audioBLAST! holds that it has no name for, chiefly
#' recordists' undescribed species, are left alone, which is the right outcome
#' for them: they are real taxa that no catalogue has caught up with, and
#' forcing them onto the nearest described species would say something false.
#' A name is not looked up at all where it is not a determination:
#'
#'   Albanycada "sp. 01"            an undescribed species, named where it was found
#'   Ancylecha sp.                  identified no further than its genus
#'   Ephippiger ?ephippiger         a determination its author doubted
#'   Anaxyrus americanus x fowleri  a hybrid, which is not one taxon
#'
#' The doubted ones matter most. The matcher takes the question mark out and
#' answers with the taxon, and taking that would turn a doubt into a statement
#' that the two are the same.
#'
#' These rules hold for the sources audited when they were written
#' (bio.acousti.ca, iNaturalist and taxonBot, September 2026), not for all
#' sources for ever: each of those three writes open nomenclature its own way
#' and hybrids already arrive written two ways between them. A source added
#' later is matched by running this again and needs no change here, but what it
#' reached is worth reading in the outcomes before it is trusted.
#'
#' @param taxa Data frame of the taxa audioBLAST! holds, as `/data/taxa/` gives
#'   them, with source, id, taxon, rank and the classification columns. The
#'   default reads them from the API.
#' @param dataset Dataset key of the taxonomy at ChecklistBank. The default is
#'   the Catalogue of Life's latest release, so a run always matches against the
#'   current one; each link says in its remarks which release that was.
#' @param colids Sources whose taxa were read out of the Catalogue of Life, so
#'   that the id of a row is the id of the taxon and there is no name to match.
#'   Their rows are looked up all the same, because an id that was accepted when
#'   it was read in can be a synonym by now.
#' @param pause Seconds between requests.
#' @param verbose If TRUE reports progress and what each source came to.
#' @return Named list of the data frames the import gives: the `taxa`, with an
#'   empty source column (see sourceR()), and the `links`. The outcome of every
#'   row looked up is in `attr(x, "outcomes")` of the links.
#' @examples
#' \dontrun{
#' harvest <- colR(verbose=TRUE)
#' #replace, because this gives its taxa whole every time: a taxon the release
#' #no longer holds is one that nothing points at any more, and its links are
#' #replaced whether the taxa are or not
#' uploadTaxa(db, taxonomiseR(sourceR("CoL", harvest$taxa)), replace=TRUE)
#' uploadLinks(db, sourceR("CoL", harvest$links))
#' }
#' @importFrom curl new_handle curl_escape curl_fetch_memory
#' @importFrom rjson fromJSON
#' @export
colR <- function(taxa=audioblastTaxa(), dataset="3LR", colids=c("taxonBot"),
                 pause=0.1, verbose=FALSE) {
  if (!is.character(dataset) || length(dataset) != 1 || is.na(dataset) || !nzchar(dataset)) {
    stop("dataset must be a ChecklistBank dataset key.")
  }
  if (!is.character(colids) || any(is.na(colids))) {
    stop("colids must be the sources whose ids are Catalogue of Life ids.")
  }
  if (!is.numeric(pause) || length(pause) != 1 || !is.finite(pause) || pause < 0) {
    stop("pause must be a non-negative number.")
  }
  handle <- new_handle(
    useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
    connecttimeout=30, timeout=300)
  first <- TRUE
  fetch <- function(path) {
    if (!first) Sys.sleep(pause)
    first <<- FALSE
    colFetch(path, dataset, handle)
  }
  return(colHarvest(taxa, fetch, colids=colids, verbose=verbose))
}

#' The taxa audioBLAST! holds
#'
#' Reads every taxon of every source from the audioBLAST! API, a page at a time.
#' What the Catalogue of Life is asked for is the taxa audioBLAST! holds, so
#' this is what colR() matches unless it is given them.
#'
#' @param url Address of the taxa endpoint.
#' @param page_size Taxa per request.
#' @return Data frame of taxa, with the columns the endpoint gives.
#' @importFrom curl new_handle curl_fetch_memory
#' @importFrom rjson fromJSON
#' @export
audioblastTaxa <- function(url="https://api.audioblast.org/data/taxa/", page_size=1000) {
  handle <- new_handle(
    useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
    connecttimeout=30, timeout=300)
  rows <- list()
  page <- 1
  repeat {
    body <- rawToChar(curl_fetch_memory(
      paste0(url, "?page=", page, "&page_size=", page_size), handle=handle)$content)
    Encoding(body) <- "UTF-8"
    answer <- fromJSON(body)
    rows <- c(rows, answer$data)
    if (page >= (answer$last_page %||% 0)) break
    page <- page + 1
  }
  columns <- unique(unlist(lapply(rows, names)))
  taxa <- as.data.frame(lapply(setNames(columns, columns), function(column) {
    vapply(rows, function(row) {
      value <- row[[column]]
      if (is.null(value)) "" else as.character(value)
    }, character(1))
  }), stringsAsFactors=FALSE, check.names=FALSE)
  return(taxa)
}

#What a source did not say, where something has to stand in for it
`%||%` <- function(a, b) if (is.null(a)) b else a

#What ChecklistBank answers for a path, NULL where it holds nothing there, and
#an error where it could not be asked. One failed request is not worth losing a
#run of twenty thousand to, so each is tried again.
colFetch <- function(path, dataset, handle, backoff=c(1, 5, 20)) {
  url <- paste0("https://api.checklistbank.org/dataset/", curl_escape(dataset), "/", path)
  for (wait in c(backoff, NA)) {
    response <- tryCatch(curl_fetch_memory(url, handle=handle), error=function(e) e)
    if (!inherits(response, "error")) {
      #A name usage the release does not hold is an answer, not a failure
      if (response$status_code == 404) return(NULL)
      if (response$status_code == 200) {
        body <- rawToChar(response$content)
        Encoding(body) <- "UTF-8"
        parsed <- tryCatch(fromJSON(body), error=function(e) NULL)
        if (!is.null(parsed)) return(parsed)
      }
      problem <- paste0("HTTP ", response$status_code)
      #A request that was refused rather than dropped will be refused again
      if (response$status_code >= 400 && response$status_code < 500 &&
          response$status_code != 429) break
    } else {
      problem <- conditionMessage(response)
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  stop("ChecklistBank did not answer for ", url, ": ", problem)
}

#Text of a name to compare by: what the Catalogue of Life gives back is the name
#it holds rather than the name it was sent, so the two are compared with the
#case and the spacing taken out, and with a subgenus in brackets dropped, as
#audioBLAST! writes Mus (Mus) musculus where the Catalogue of Life writes Mus
#musculus.
colCanonical <- function(name) {
  name <- gsub("\\([^()]*\\)", " ", as.character(name))
  return(tolower(trimws(gsub("[[:space:]]+", " ", name))))
}

#Whether a name is a determination: a name of a taxon, rather than a recordist
#saying which taxon they could not name
colDetermination <- function(name) {
  name <- as.character(name)
  if (length(name) != 1 || is.na(name) || !nzchar(trimws(name))) return(FALSE)
  #An undescribed or informal name, written in quotes
  if (grepl("\"", name, fixed=TRUE)) return(FALSE)
  #A determination its author was not sure of
  if (grepl("?", name, fixed=TRUE)) return(FALSE)
  #A hybrid, whether written with the sign or with an x between two epithets.
  #Both are in use, and in these three sources the x is the commoner of the two.
  if (grepl("×", name, fixed=TRUE)) return(FALSE)
  if (grepl("[[:alpha:]] +x +[[:alpha:]]", name)) return(FALSE)
  #Identified no further than the taxon named, or only compared to one
  if (grepl("\\b(sp|spp|ssp|cf|aff|nr|indet)\\b\\.?", name, ignore.case=TRUE)) return(FALSE)
  return(TRUE)
}

#Whether a match says which taxon a name is of, rather than failing to or
#answering about something else. The name is compared because a matcher that
#answers about a taxon of another name has not found the one asked for, and it
#is what catches an ambiguous or a higher-rank answer whatever the type says.
colUsable <- function(found, name, rank="") {
  if (is.null(found) || !isTRUE(found$match)) return(FALSE)
  if (!isTRUE(found$type %in% COL_MATCHED)) return(FALSE)
  usage <- found$usage
  if (is.null(usage) || is.null(usage$id) || is.null(usage$name)) return(FALSE)
  if (colCanonical(usage$name) != colCanonical(name)) return(FALSE)
  if (nzchar(rank) && !identical(tolower(usage$rank %||% ""), rank)) return(FALSE)
  return(TRUE)
}

#A taxon of the taxonomy as a row of it: its id, name, rank and, where it is
#one of a chain, the id of the taxon above it
colNode <- function(entry, parent="") {
  return(list(id=as.character(entry$id %||% ""),
              taxon=as.character(entry$name %||% entry$label %||% ""),
              rank=as.character(entry$rank %||% ""),
              parent=as.character(parent)))
}

#The taxon a match is of and every taxon above it, nearest first. The match
#already carries the whole classification, so the ancestors cost no request:
#getting the tree is the same job as getting the identity, not a second one.
#
#Where the name matched is a synonym, the taxon it is a synonym of heads its
#classification, and the chain starts there: a source calling a warbler
#Vermivora celata and one calling it Leiothlypis celata must reach one taxon,
#not two. A name that says which taxon it is of no more clearly than that gives
#no chain.
colChain <- function(usage) {
  above <- usage$classification %||% list()
  status <- tolower(usage$status %||% "")
  if (status %in% COL_ACCEPTED) {
    chain <- c(list(usage), above)
    via <- NULL
  } else if (identical(status, "synonym") && length(above) > 0 &&
             tolower(above[[1]]$status %||% "") %in% COL_ACCEPTED) {
    chain <- above
    via <- as.character(above[[1]]$name %||% above[[1]]$label %||% "")
  } else {
    return(NULL)
  }
  nodes <- colNodes(chain)
  if (is.null(nodes)) return(NULL)
  return(list(nodes=nodes, via=via))
}

#The classification of a taxon looked up by its id, which the taxonomy gives
#root first and audioBLAST! reads nearest first, as a chain headed by the taxon
#itself
colChainByID <- function(id, fetch) {
  above <- fetch(paste0("taxon/", curl_escape(id), "/classification"))
  if (is.null(above)) return(NULL)
  above <- rev(above)
  #The classification of a taxon does not include the taxon, but the last of it
  #is the taxon's parent
  return(above)
}

#The taxon of the taxonomy that a row of audioBLAST!'s taxa is, found by its
#name. The name is asked for three ways, each giving up something the one
#before it held on to, and the first that answers is taken:
#
#  1. at its rank and under the taxon its source puts it in, which is the only
#     way a homonym can be told apart
#  2. at its rank alone, for where a source's classification disagrees with the
#     taxonomy's and so tells it nothing
#  3. by name alone, for where the two disagree about the rank as well:
#     bio.acousti.ca has Hyracoidea as a family and the taxonomy as an order,
#     and they are the same animals either way. What the taxonomy ranks it is
#     put in the link's remarks rather than changed anywhere, since which of
#     them is right is not audioBLAST!'s to say.
colByName <- function(name, rank, hint, fetch) {
  ask <- function(rank, hint) {
    path <- paste0("match/nameusage?q=", curl_escape(name))
    if (nzchar(rank)) path <- paste0(path, "&rank=", curl_escape(rank))
    if (!is.null(hint)) path <- paste0(path, "&", names(hint), "=", curl_escape(hint[[1]]))
    return(fetch(path))
  }
  if (nzchar(rank)) {
    if (!is.null(hint)) {
      found <- ask(rank, hint)
      if (colUsable(found, name, rank)) return(list(usage=found$usage, note=NULL))
    }
    found <- ask(rank, NULL)
    if (colUsable(found, name, rank)) return(list(usage=found$usage, note=NULL))
  }
  found <- ask("", NULL)
  if (!colUsable(found, name)) return(NULL)
  ranked <- tolower(found$usage$rank %||% "")
  #A rank that is not a rank at all is no disagreement worth recording
  if (!ranked %in% COL_RANKS) return(NULL)
  note <- if (nzchar(rank) && ranked != rank) {
    paste0("the Catalogue of Life ranks it ", ranked, ", the source ", rank)
  } else NULL
  return(list(usage=found$usage, note=note))
}

#The classification hint to send with a name: the narrowest rank above it that
#its source gives, and nothing where the source gives none or names the taxon
#itself at that rank
colHint <- function(row, name) {
  for (at in COL_HINTS) {
    value <- as.character(row[[at]] %||% "")
    if (length(value) == 1 && !is.na(value) && nzchar(value) &&
        colCanonical(value) != colCanonical(name)) {
      return(setNames(list(value), at))
    }
  }
  return(NULL)
}

#The release of the taxonomy as it names itself, e.g. COL26.9. A dataset key
#can be an alias for whichever release is current, so a link made today and one
#made after the next release can point at the same taxon and not quite mean the
#same thing by it. Each link says which release it was made against, so a later
#import can tell a taxon that has moved from a match that was wrong.
colRelease <- function(fetch) {
  about <- fetch("")
  return(as.character(about$alias %||% about$title %||% "an unnamed release"))
}

#A chain of taxa, nearest first, as rows of the taxonomy: each is given the id
#of the one after it as its parent, and the last of them has none. Nothing is
#taken from a chain with a taxon in it that has no id.
colNodes <- function(entries) {
  nodes <- list()
  for (i in seq_along(entries)) {
    parent <- if (i < length(entries)) entries[[i + 1]]$id %||% "" else ""
    node <- colNode(entries[[i]], parent)
    if (!nzchar(node$id)) return(NULL)
    nodes[[i]] <- node
  }
  return(nodes)
}

#A name usage read as an entry of a chain
colEntry <- function(usage) {
  return(list(id=usage$id, name=usage$name$scientificName %||% usage$label,
              rank=usage$name$rank %||% usage$rank))
}

#The taxon a row of a source that kept the taxonomy's ids is, by that id, and
#the chain above it. An id is only that source's taxon while the taxonomy still
#holds the same name under it: ids are not promised to mean the same thing from
#one release to the next, and one that has come to be another taxon's would link
#the row to an animal it is not. Where the id no longer says which taxon the row
#is of, NULL asks for the name to be matched instead, as for any other row.
colByID <- function(id, name, fetch) {
  usage <- fetch(paste0("nameusage/", curl_escape(id)))
  if (is.null(usage) || is.null(usage$id)) return(NULL)
  held <- usage$name$scientificName %||% usage$label %||% ""
  if (colCanonical(held) != colCanonical(name)) return(NULL)
  status <- tolower(usage$status %||% "")
  via <- NULL
  if (!status %in% COL_ACCEPTED) {
    if (!identical(status, "synonym")) return(NULL)
    accepted <- usage$accepted
    if (is.null(accepted) || is.null(accepted$id)) return(NULL)
    if (!tolower(accepted$status %||% "accepted") %in% COL_ACCEPTED) return(NULL)
    via <- as.character(accepted$name$scientificName %||% accepted$label %||% "")
    usage <- accepted
  }
  above <- colChainByID(usage$id, fetch)
  if (is.null(above)) return(NULL)
  nodes <- colNodes(c(list(colEntry(usage)), above))
  if (is.null(nodes)) return(NULL)
  return(list(nodes=nodes, via=via))
}

#Imports the taxa and writes the links, given something that answers for a path
#of the taxonomy. The fetching is a parameter so that this can be run over
#recorded answers as well as over the taxonomy itself.
colHarvest <- function(taxa, fetch, colids=c("taxonBot"), verbose=FALSE) {
  release <- colRelease(fetch)
  if (verbose) message("Matching ", nrow(taxa), " taxa against ", release)

  #Every taxon reached, and every taxon above one, by its id. A taxon reached
  #twice is one taxon: the whole point is that sources meet at it.
  found <- new.env(hash=TRUE, parent=emptyenv())
  matched <- character(nrow(taxa))
  remarks <- character(nrow(taxa))
  outcomes <- character(nrow(taxa))

  for (i in seq_len(nrow(taxa))) {
    if (verbose && i %% 250 == 0) {
      message("  ", i, " of ", nrow(taxa), " looked up, ", sum(nzchar(matched)), " matched")
    }
    row <- lapply(as.list(taxa[i, , drop=FALSE]), function(value) {
      if (length(value) == 1 && !is.na(value)) as.character(value) else ""
    })
    name <- trimws(row$taxon %||% "")
    rank <- tolower(trimws(row$rank %||% ""))
    said <- name

    chain <- NULL
    #A row of a source that read its taxa out of the taxonomy says which taxon
    #it is already, so its id is looked up rather than its name
    if ((row$source %||% "") %in% colids && grepl("^[0-9A-Za-z]+$", row$id %||% "")) {
      chain <- colByID(row$id, name, fetch)
    }

    if (is.null(chain)) {
      if (!colDetermination(name)) {
        outcomes[i] <- "not a determination"
        next
      }
      if (nzchar(rank) && !rank %in% COL_RANKS) {
        outcomes[i] <- paste0("rank ", rank, " is not one the Catalogue of Life ranks taxa by")
        next
      }
      byName <- colByName(name, rank, colHint(row, name), fetch)
      if (is.null(byName)) {
        outcomes[i] <- "no match in the Catalogue of Life"
        next
      }
      chain <- colChain(byName$usage)
      if (is.null(chain)) {
        outcomes[i] <- paste0("the Catalogue of Life holds the name as ",
                              tolower(byName$usage$status %||% "something it does not say"))
        next
      }
      if (!is.null(byName$note)) said <- paste0(said, "; ", byName$note)
    }

    if (!is.null(chain$via)) {
      said <- paste0(said, "; a synonym in the Catalogue of Life of ", chain$via)
      outcomes[i] <- "matched through a synonym"
    } else {
      outcomes[i] <- "matched"
    }
    for (node in chain$nodes) {
      if (is.null(found[[node$id]])) assign(node$id, node, envir=found)
    }
    matched[i] <- chain$nodes[[1]]$id
    remarks[i] <- paste0(said, "; ", release)
  }

  if (verbose) {
    for (source in sort(unique(as.character(taxa$source)))) {
      of <- as.character(taxa$source) == source
      message("  ", source, ": ", sum(nzchar(matched) & of), " of ", sum(of), " matched (",
              sprintf("%.1f%%", 100 * sum(nzchar(matched) & of) / max(1, sum(of))), ")")
    }
  }

  imported <- colTaxa(found)
  links <- colLinks(taxa, matched, remarks, imported, release)
  attr(links, "outcomes") <- data.frame(
    source=as.character(taxa$source), id=as.character(taxa$id),
    taxon=as.character(taxa$taxon), rank=as.character(taxa$rank),
    matched=matched, outcome=outcomes, stringsAsFactors=FALSE)
  if (verbose) message(nrow(imported), " taxa imported, ", nrow(links), " links written")
  return(list(taxa=imported, links=links))
}

#The taxa reached, as rows of the taxa table. The source is left empty for
#sourceR() to fill, as every harvest does, and parent_id is the tree: the ranks
#the taxa table has no column for, a superfamily or an infraorder, are held by
#it and by nothing else.
colTaxa <- function(found) {
  taxa <- getHeaders("taxa")
  ids <- ls(found)
  if (length(ids) == 0) return(taxa)
  nodes <- mget(ids, envir=found)
  rows <- data.frame(
    source=rep_len("", length(ids)),
    id=vapply(nodes, function(node) node$id, character(1)),
    taxon=vapply(nodes, function(node) node$taxon, character(1)),
    u1="", u2="", u3="", u4="",
    Rank=str_to_title(vapply(nodes, function(node) node$rank, character(1))),
    parent_id=vapply(nodes, function(node) node$parent, character(1)),
    parent_taxon="", stringsAsFactors=FALSE)
  names(rows) <- names(taxa)
  rows <- rows[order(rows$id), , drop=FALSE]
  rownames(rows) <- NULL
  return(rows)
}

#The links. A row of another source is linked to the taxon it is, which is a
#row of this import, so a client reaches it without leaving audioBLAST!. Each
#imported taxon is linked to the taxonomy's own address for it as well, so that
#a client can go on to the catalogue, and so that what audioBLAST! holds can be
#joined to anything else citing the same taxon.
#
#The object of a link to an imported taxon names no source, so that the source
#this import is uploaded under fills it in (see normaliseLinks); the subject
#names the source holding the row, which is another source's.
colLinks <- function(taxa, matched, remarks, imported, release) {
  links <- getHeaders("links")
  rows <- which(nzchar(matched))
  if (length(rows) > 0) {
    links <- rbind(links, data.frame(
      source="", subject_type="taxa", subject_source=as.character(taxa$source)[rows],
      subject_id=as.character(taxa$id)[rows], predicate=COL_EXACT_MATCH,
      object_type="taxa", object_source="", object_id=matched[rows],
      qualifier="", remarks=remarks[rows], reference="", stringsAsFactors=FALSE))
  }
  if (nrow(imported) > 0) {
    links <- rbind(links, data.frame(
      source="", subject_type="taxa", subject_source="",
      subject_id=imported$id, predicate=COL_EXACT_MATCH,
      object_type="iri", object_source="", object_id=colIRI(imported$id),
      qualifier="", remarks=paste0(imported$taxon, "; ", release),
      reference="", stringsAsFactors=FALSE))
  }
  rownames(links) <- NULL
  return(links)
}

#The taxonomy's own address for a taxon, which resolves to what it holds
colIRI <- function(id) {
  return(paste0("https://api.checklistbank.org/dataset/3LR/taxon/", id))
}

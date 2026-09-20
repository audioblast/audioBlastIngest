#' Harvest descriptions from Plazi TreatmentBank
#'
#' Harvests what taxonomic treatments say in prose from Plazi TreatmentBank and
#' converts it to the audioBlast! descriptions format.
#'
#' Treatments are found through the Biodiversity Literature Repository on
#' Zenodo rather than through TreatmentBank's own search, which caps every
#' query at 100 results and cannot page past them. Zenodo pages properly, says
#' what licence each treatment carries, and names the treatment at Plazi, so a
#' harvest is discovered there and read from Plazi. Its result window is
#' 10,000 records, so a query that matches more than that is refused rather
#' than silently truncated: narrow it and harvest in parts.
#'
#' A treatment's prose is only in its XML. The JSON gives metadata and
#' citations with no text at all, and the RDF flattens every section to an
#' untyped `spm:InfoItem`, losing what each one is of. The XML keeps that as
#' the type of a `subSubSection`, which is a description's topic, and
#' normaliseDescriptions() reads the Species Profile Model info item it names.
#'
#' Sections that are data rather than prose are left out: nomenclature, the
#' material examined, the reference group and the type taxon belong to the
#' specimens, references and taxa a treatment gives, not to what it says.
#' Figure captions are dropped too, as a caption describes a figure and is set
#' inside whichever section the figure falls in, not in the one it belongs to.
#'
#' Each description is about the taxon the treatment treats and rests on the
#' treatment that says it, which are links. The treatment is harvested as a
#' reference of its own: it is deposited as a publication and has its own DOI,
#' and an article holds many treatments (61 in one of the papers sampled), so
#' citing the article instead would not say which treatment spoke. The
#' article is in turn what the treatment rests on. audioBlast! does not hold
#' Plazi's taxa, so a taxon is named by GBIF where Zenodo gives it and by
#' Plazi's taxon concept otherwise.
#'
#' @param query Zenodo search over the treatments, e.g. `"stridulation"`. The
#'   default finds the treatments that say something about sound.
#' @param licenses Licence ids to accept, as Zenodo gives them. Treatments
#'   under any other licence are left out with a warning rather than
#'   republished.
#' @param max Most treatments to read, for trying a query out. `Inf` reads
#'   every treatment the query matches.
#' @param token Zenodo access token, by default read from the ZENODO_TOKEN
#'   environment variable. It is not needed; it only raises the page size from
#'   25 to 100, so a large harvest makes a quarter as many requests. Keep
#'   tokens out of code and version control.
#' @param pause Seconds between requests.
#' @param dir Directory to stream the harvest to, a CSV of each type of table,
#'   instead of holding it all. A harvest of every acoustic treatment is some
#'   hours of requests, and streaming it means a failed upload does not throw
#'   those hours away (see uploadStreamed()).
#' @param verbose If TRUE reports harvest progress.
#' @return Named list of the data frames a harvest gives: the descriptions, the
#'   acoustic parameters the same treatments measure as traits, the words they
#'   render a call with as onomatopoeia, the treatments
#'   they were read from as references, and the links. Each has an empty source
#'   column (see sourceR()). With dir, the paths they were written to instead.
#' @examples
#' \dontrun{
#' harvest <- plaziR()
#' uploadReferences(db, sourceR("Plazi", harvest$references))
#' uploadDescriptions(db, sourceR("Plazi", harvest$descriptions))
#' uploadLinks(db, sourceR("Plazi", harvest$links))
#' }
#' @importFrom curl new_handle curl_escape curl_fetch_memory
#' @importFrom rjson fromJSON
#' @importFrom stats setNames
#' @export
plaziR <- function(query=plaziAcoustic, licenses=plaziLicenses, max=Inf,
                   token=Sys.getenv("ZENODO_TOKEN"), pause=1, verbose=FALSE,
                   dir=NULL) {
  if (!is.character(query) || length(query) != 1 || is.na(query) || !nzchar(query)) {
    stop("query must be a Zenodo search over the treatments.")
  }
  if (!is.character(licenses) || length(licenses) == 0 || any(is.na(licenses))) {
    stop("licenses must be one or more licence ids.")
  }
  if (!is.numeric(max) || length(max) != 1 || is.na(max) || max < 1) {
    stop("max must be the most treatments to read, or Inf for all of them.")
  }
  if (!is.numeric(pause) || length(pause) != 1 || !is.finite(pause) || pause < 0) {
    stop("pause must be a non-negative number.")
  }

  handle <- new_handle(useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
                       connecttimeout=30, timeout=300)
  first <- TRUE
  pacing <- function() {
    if (!first) Sys.sleep(pause)
    first <<- FALSE
  }

  found <- plaziFound(query, licenses, max, token, handle, pacing, verbose)

  #Each treatment is converted as it arrives and the document let go of. With
  #a dir the tables go to files as well and nothing is held from one treatment
  #to the next, which is what a harvest of every acoustic treatment needs:
  #those are some hours of requests, and a harvest lost to a failed upload is
  #those hours thrown away.
  pages <- lapply(plaziTables, function(type) list())
  for (i in seq_along(found)) {
    treatment <- found[[i]]
    pacing()
    document <- plaziRead(plaziFetch(
      paste0("https://tb.plazi.org/GgServer/xml/", treatment$uuid), handle), treatment$uuid)
    tables <- plaziHarvested(document, treatment)
    #A treatment that says nothing, measures nothing and renders nothing is
    #not a treatment this harvest wanted, and its reference cites nothing
    if (all(vapply(tables[setdiff(plaziTables, c("references", "links"))],
                   nrow, integer(1)) == 0)) next
    for (type in plaziTables) {
      if (is.null(dir)) {
        pages[[type]][[length(pages[[type]]) + 1]] <- tables[[type]]
      } else {
        streamTable(dir, type, tables[[type]])
      }
    }
    if (verbose && i %% 100 == 0) message("  Plazi: ", i, " of ", length(found), " treatments")
  }

  if (!is.null(dir)) {
    paths <- lapply(plaziTables, function(type) streamPath(dir, type))
    names(paths) <- plaziTables
    if (verbose) message("  Plazi harvested to ", dir)
    return(paths)
  }

  data <- lapply(plaziTables, function(type) plaziCombine(pages[[type]], type))
  names(data) <- plaziTables
  if (verbose) {
    for (type in plaziTables) message("  Plazi ", type, ": ", nrow(data[[type]]))
  }
  return(data)
}

#The tables a Plazi harvest gives, in the order a reference is needed before
#the records that cite it. Naming them once is what lets a table be added
#without the harvest, the streaming or the combining being told about it.
plaziTables <- c("references", "descriptions", "traits", "onomatopoeia", "links")

#What a treatment gives, as one table of each type. The measured parameters
#and the renderings are read before the sections, because plaziSections()
#takes the captions out of the document as it goes.
plaziHarvested <- function(document, treatment) {
  measured <- plaziTraits(document, treatment)
  renderings <- plaziOnomatopoeia(document, treatment)
  sections <- plaziSections(document, treatment$uuid)
  links <- rbind(plaziLinks(sections$id, treatment), measured$links, renderings$links)
  tables <- list(
    #A treatment is read once: its sections are the descriptions and its
    #heading is the reference they cite
    references=plaziReference(document, treatment),
    descriptions=sections,
    traits=measured$traits,
    onomatopoeia=renderings$onomatopoeia,
    links=links)
  #A link whose subject this treatment did not give has nothing to join. The
  #ids are checked against the treatment's own tables rather than the whole
  #harvest's, which is the same answer and is the one a streamed harvest can
  #give, having let every other treatment go.
  held <- unlist(lapply(setdiff(plaziTables, "links"),
                        function(type) plaziIds(tables[[type]], type)), use.names=FALSE)
  tables$links <- tables$links[tables$links$subject_id %in% held, , drop=FALSE]
  rownames(tables$links) <- NULL
  return(tables[plaziTables])
}

#The ids of a table, which traits call traitID
plaziIds <- function(table, type) {
  if (type == "traits") return(table$traitID)
  return(table$id)
}

#The tables of every treatment as one, with the repeats left out. Two
#treatments should not give a record twice, as an id carries the UUID of the
#treatment it came from, so this is the belt to the braces.
plaziCombine <- function(tables, type) {
  data <- do.call(rbind, c(list(getHeaders(type)), tables))
  keep <- if (type == "links") !duplicated(data) else !duplicated(plaziIds(data, type))
  data <- data[keep, , drop=FALSE]
  rownames(data) <- NULL
  return(data)
}

#The treatments that say something about sound. Plazi holds over a million
#treatments and audioBlast! wants the ones that describe a call, a song or the
#organ that makes it, so the harvest is named by what it is looking for rather
#than by a taxon: a treatment of any group can describe a sound.
plaziAcoustic <- paste("acoustic OR song OR stridulation OR stridulatory OR",
                       "bioacoustics OR spectrogram OR sonogram OR oscillogram")

#Licences that let a description be republished. Every treatment sampled from
#the Biodiversity Literature Repository was CC0, but Plazi deposits under the
#licence of the article a treatment came from, so the others are named too
#rather than assumed. Plazi's own zenodo-license-* fields are not used: one
#reports UNSPECIFIED for a treatment that Zenodo records as CC0.
plaziLicenses <- c("cc-zero", "cc-by-4.0", "cc-by-3.0", "cc-by-sa-4.0", "public-domain")

#Sections that are data rather than what a treatment says. The material
#examined, the nomenclature, the references cited and the type taxon are the
#specimens, taxa and references a treatment gives, which have modules of their
#own; a description holds prose.
plaziNotDescriptions <- c("nomenclature", "materials_examined", "materials examined",
                          "material_examined", "material examined", "material",
                          "type material", "type_material", "reference_group",
                          "type_taxon", "synonymic_list", "type-specimen")

#The shortest section worth keeping. A heading that a treatment left empty, or
#a section holding only a figure number, says nothing about the taxon.
plaziShortest <- 60

#The backoff of the package's other harvesters (see xenocantoFetch()): a failed
#request stops the harvest, so it is worth waiting out an outage
plaziBackoff <- c(1, 1, 2, 3, 5, 10, 30, 60)

#The treatments a Zenodo query matches, each with the licence it carries, the
#article it was published in and the taxon it treats. Zenodo's result window is
#10,000 records, which a harvest cannot page past, so a wider query is refused.
plaziFound <- function(query, licenses, max, token, handle, pacing, verbose) {
  #A token is not needed to read the repository; it only raises the page size
  size <- if (is.character(token) && length(token) == 1 && !is.na(token) && nzchar(token)) 100 else 25
  found <- list()
  refused <- 0
  page <- 1
  repeat {
    pacing()
    url <- paste0("https://zenodo.org/api/records",
                  "?communities=biosyslit&type=publication&subtype=taxonomictreatment",
                  "&size=", size, "&page=", page,
                  "&q=", curl_escape(query))
    response <- plaziFetch(url, handle, token=token)
    json <- tryCatch(fromJSON(response), error=function(e) NULL)
    hits <- json[["hits"]][["hits"]]
    if (!is.list(json) || !is.list(hits)) stop("Unexpected Zenodo response.")
    total <- suppressWarnings(as.numeric(json[["hits"]][["total"]]))
    if (page == 1) {
      if (length(total) != 1 || !is.finite(total)) stop("Missing Zenodo result count.")
      if (verbose) message("  Plazi: ", total, " treatments match")
      if (total > 10000 && is.infinite(max)) {
        stop("Zenodo pages only 10,000 records and the query matches ", total,
             ". Narrow the query and harvest in parts.")
      }
    }
    for (hit in hits) {
      treatment <- plaziTreatment(hit)
      if (is.null(treatment)) next
      if (!treatment$license %in% licenses) {
        refused <- refused + 1
        next
      }
      found[[length(found) + 1]] <- treatment
      if (length(found) >= max) break
    }
    #Zenodo pages 10,000 records and refuses the page after them, so a harvest
    #that has asked for a set number stops there rather than failing
    if (length(found) >= max || length(hits) < size ||
        page * size >= total || page * size >= 10000) break
    page <- page + 1
  }
  if (refused > 0) {
    warning(paste0("Skipping ", refused,
                   " Plazi treatments whose licence does not allow republishing."))
  }
  return(found)
}

#What a Zenodo record says about a treatment, or NULL where it does not name
#one at Plazi and so cannot be read
plaziTreatment <- function(hit) {
  metadata <- hit[["metadata"]]
  if (!is.list(metadata)) return(NULL)
  uuid <- ""
  for (identifier in plaziIdentifiers(metadata, "alternate_identifiers")) {
    match <- regmatches(identifier, regexpr("treatment\\.plazi\\.org/id/[0-9A-Fa-f-]+", identifier))
    if (length(match) == 1) uuid <- toupper(gsub("-", "", sub(".*/", "", match)))
  }
  if (!grepl("^[0-9A-F]{32}$", uuid)) return(NULL)

  #The article a treatment is part of, which the treatment rests on in turn
  article <- ""
  taxon <- ""
  for (related in metadata[["related_identifiers"]]) {
    identifier <- plaziValue(related[["identifier"]])
    if (identical(plaziValue(related[["relation"]]), "isPartOf") &&
        identical(plaziValue(related[["scheme"]]), "doi") && article == "") {
      article <- paste0("https://doi.org/", sub("^https?://(dx\\.)?doi\\.org/", "", identifier))
    }
    if (grepl("^https?://(www\\.)?gbif\\.org/species/[0-9]+$", identifier) && taxon == "") {
      taxon <- identifier
    }
  }
  #Plazi names the concept a treatment defines where GBIF has not matched it
  if (taxon == "") taxon <- paste0("http://taxon-concept.plazi.org/id/", uuid)

  #A treatment is deposited as a publication of its own, so it has a DOI that
  #is not the article's. It is the treatment that says what a description
  #says, so it is the treatment that a description cites.
  return(list(uuid=uuid, article=article, taxon=taxon,
              doi=plaziValue(hit[["doi"]]),
              license=plaziValue(metadata[["license"]][["id"]])))
}

plaziIdentifiers <- function(metadata, field) {
  values <- vapply(metadata[[field]], function(x) plaziValue(x[["identifier"]]), character(1))
  return(values[values != ""])
}

plaziValue <- function(x) {
  if (is.null(x) || !is.atomic(x) || length(x) != 1 || is.na(x)) return("")
  return(trimws(enc2utf8(as.character(x))))
}

plaziFetch <- function(url, handle, token="", backoff=plaziBackoff) {
  request <- url
  if (is.character(token) && length(token) == 1 && !is.na(token) && nzchar(token)) {
    request <- paste0(url, "&access_token=", curl_escape(token))
  }
  for (wait in c(backoff, NA)) {
    response <- tryCatch(curl_fetch_memory(request, handle=handle), error=function(e) e)
    if (inherits(response, "error")) {
      problem <- conditionMessage(response)
    } else {
      status <- response$status_code
      body <- rawToChar(response$content)
      Encoding(body) <- "UTF-8"
      if (status == 200 && nzchar(body)) return(body)
      problem <- paste0("HTTP ", status)
      #Other client errors, such as a bad query or token, will not succeed on retry
      if (status >= 400 && status < 500 && status != 429) break
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  #A token is only ever in request, never in url, so this message does not carry it
  stop("Plazi request for ", url, " failed: ", problem)
}

#A treatment's XML, read once: its sections are the descriptions and its
#heading is the reference they cite
#' @importFrom xml2 read_xml
plaziRead <- function(xml, uuid) {
  document <- tryCatch(read_xml(xml), error=function(e) NULL)
  if (is.null(document)) stop("Plazi treatment ", uuid, " is not valid XML.")
  return(document)
}

#The treatment a harvest read, as the reference its descriptions cite. A
#treatment is deposited as a publication of its own and has its own DOI, so it
#is citable; an article holds many of them (61 in one of the papers sampled),
#so citing the article instead would not say which treatment spoke. It is an
#incollection because it is a titled part of a larger work, and the work it is
#part of is a link (see plaziLinks()).
#' @importFrom xml2 xml_attr xml_find_first
plaziReference <- function(document, treatment) {
  #The document element is the root of a treatment's XML, so it is matched from
  #anywhere in the tree rather than below the context node
  heading <- xml_find_first(document, "//document")
  at <- function(name) plaziValue(xml_attr(heading, name))
  uri <- paste0("https://treatment.plazi.org/id/", treatment$uuid)
  #A treatment's title is the name it treats, which is what it is a treatment of
  named <- plaziName(document, at("docTitle"))
  reference <- data.frame(
    source="", id=treatment$uuid, type="incollection",
    title=named$title, author=at("docAuthor"), year=at("docDate"),
    journal=at("docOrigin"), booktitle=at("masterDocTitle"),
    doi=sub("^https?://(dx\\.)?doi\\.org/", "", treatment$doi),
    note=named$note, url=uri, info_url=uri, stringsAsFactors=FALSE)
  columns <- names(getHeaders("references"))
  for (column in setdiff(columns, names(reference))) reference[[column]] <- ""
  return(reference[columns])
}

#The name a treatment treats, which is its title, and what Plazi called it
#where that had to be corrected.
#
#Plazi's parser cannot read open nomenclature: a treatment of Gryllus sp.4 is
#marked up as species="undefined-4" and titled "Gryllus undefined-4". The
#mistake is systematic rather than occasional, it follows the printed
#designator (sp. B becomes undefined-B), and it has travelled: Zenodo titles
#the treatment the same way, and GBIF's backbone holds the mangled name as an
#accepted species. Only the printed text of the name is still right, so a
#mangled title is taken from there. The name is corrected rather than kept
#because a reference's title is read by people, and nothing is lost by it:
#what Plazi gave stays in note, and doi, url and info_url resolve whatever the
#title says.
#
#A title that is not mangled is left alone: the printed name carries its
#authority and Plazi's spacing, which a title should not gain.
#' @importFrom xml2 xml_attr xml_find_all xml_text
plaziName <- function(document, title) {
  if (!grepl("undefined", title, fixed=TRUE)) return(list(title=title, note=""))
  printed <- ""
  for (name in xml_find_all(document, "//subSubSection[@type='nomenclature']//taxonomicName")) {
    if (grepl("undefined", plaziValue(xml_attr(name, "species")), fixed=TRUE)) {
      printed <- plaziText(paste(xml_text(xml_find_all(name, ".//text()")), collapse=" "))
      break
    }
  }
  #A mangled title with no printed name to put in its place is left as it is,
  #rather than guessed at
  if (printed == "") return(list(title=title, note=""))
  return(list(title=printed,
              note=paste0("Plazi titles this treatment \"", title,
                          "\"; its parser reads open nomenclature as undefined. ",
                          "The name here is the treatment's own printed text.")))
}

#The descriptions a treatment's XML gives, one for each section that holds
#prose. A section is identified within the treatment, so a treatment that is
#reprocessed and gains a section does not renumber the others.
#' @importFrom xml2 xml_attr xml_find_all xml_remove xml_text
plaziSections <- function(document, uuid) {
  sections <- xml_find_all(document, ".//subSubSection")
  descriptions <- getHeaders("descriptions")
  for (section in sections) {
    topic <- plaziValue(xml_attr(section, "type"))
    if (tolower(topic) %in% plaziNotDescriptions) next
    id <- plaziValue(xml_attr(section, "id"))
    if (id == "") next
    #A caption describes a figure and sits in whichever section the figure
    #falls in, so it is not part of what that section says
    xml_remove(xml_find_all(section, ".//caption"))
    #Plazi marks up a treatment without always keeping the space that was
    #between two marked up words, so running the text together welds them
    #(Plazi's own RDF says nigripesMeigen for this reason). The pieces are
    #joined with a space and the spacing then tidied, which is safe because
    #Plazi's text is loosely spaced throughout.
    value <- plaziText(paste(xml_text(xml_find_all(section, ".//text()")), collapse=" "))
    if (nchar(value) < plaziShortest) next
    descriptions[nrow(descriptions) + 1, ] <- list(
      source="", id=paste0(uuid, "#", id), topic=topic, value=value,
      info_url=paste0("https://treatment.plazi.org/id/", uuid), topic_link="")
  }
  rownames(descriptions) <- NULL
  return(descriptions)
}

#Plazi reads a treatment from the page it was printed on, so its text is
#broken where the typesetting broke it: a URL or a DOI can be split on the
#spaces and hyphens that fitted it to the column. Those are put back together,
#as they are the same string however it was set, and the rest of the text is
#left as the treatment gives it.
plaziText <- function(x) {
  x <- gsub("\\s+", " ", x, perl=TRUE)
  #A scheme set with spaces around its punctuation
  x <- gsub("(https?)\\s*:\\s*/\\s*/\\s*", "\\1://", x, perl=TRUE)
  #A URL carries on across the spaces the typesetting put in it. A space next
  #to punctuation is one of those; a space between two words is not, and a
  #capital after a full stop begins a sentence rather than a path, so the URL
  #ends there. Joining one space can reveal the next, so this runs to a stop.
  repeat {
    joined <- gsub("(https?://\\S*[./?=&#_-])\\s+(?=[a-z0-9])", "\\1", x, perl=TRUE)
    joined <- gsub("(https?://\\S*)\\s+(?=[/?=&#_-])", "\\1", joined, perl=TRUE)
    if (identical(joined, x)) break
    x <- joined
  }
  #A DOI, which is set the same way and always begins with a directory number
  x <- gsub("\\b(10)\\s*\\.\\s*(\\d{4,9})\\s*/\\s*", "\\1.\\2/", x, perl=TRUE)
  return(trimws(x))
}

#The links a treatment's descriptions give: each is about the taxon the
#treatment treats and rests on the article it was published in. The taxon is
#named by GBIF or by Plazi, as audioBlast! does not hold Plazi's taxa, and is
#about is what a description of a taxon supports.
plaziLinks <- function(ids, treatment) {
  links <- getHeaders("links")
  if (length(ids) == 0) return(links)
  link <- function(subject_type, subject_id, predicate, object_type, object_id) {
    data.frame(source="", subject_type=subject_type, subject_source="",
               subject_id=subject_id, predicate=predicate, object_type=object_type,
               object_source="", object_id=object_id, qualifier="", remarks="",
               reference="", stringsAsFactors=FALSE)
  }
  links <- rbind(links, link("descriptions", ids,
                             "http://purl.obolibrary.org/obo/IAO_0000136",
                             "iri", treatment$taxon))
  #A description rests on the treatment that says it, which audioBlast! holds
  #as a reference of its own, so the citation reaches a record rather than a
  #bare IRI: the links table's reference column is resolved to a references
  #id, so nothing could cite a treatment that was only a URL
  links <- rbind(links, link("descriptions", ids,
                             "http://purl.org/dc/terms/source",
                             "references", treatment$uuid))
  #And the treatment rests on the article it is part of, once for the
  #treatment rather than once for each of its descriptions
  if (treatment$article != "") {
    links <- rbind(links, link("references", treatment$uuid,
                               "http://purl.org/dc/terms/source",
                               "iri", treatment$article))
  }
  rownames(links) <- NULL
  return(links)
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

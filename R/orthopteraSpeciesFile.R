#' Harvest recordings from Orthoptera Species File
#'
#' Harvests recording metadata from the Orthoptera Species File's public
#' TaxonWorks API and converts it to the audioBlast! recordings format.
#'
#' A sound is conveyed on a taxon (an OTU), a specimen, a field observation or
#' a collecting event. Specimens and field observations are read as Darwin Core
#' records, which give both the taxon of the accepted determination and where
#' and when the recording was made. A collecting event is read through the
#' Darwin Core records of its occurrences, which say what was recorded only
#' where they all agree.
#'
#' Where an indirect link gives no taxon, a binomial that is the whole title or
#' precedes a numbered recording label is matched to a unique accepted OSF
#' taxon. The Orthoptera Species File does not identify those recordings, so
#' the name is not put in taxon, which holds the scientific name the source
#' gives: it is given as a link saying the identification was read from the
#' title. A recording of more than one taxon is given links too, as a
#' scientific name is one name. The links are `attr(x, "links")`, which
#' ingestR() uploads.
#'
#' The MIME type and size of each recording are read with one HEAD request for
#' its audio; audio is never downloaded. The API gives no time of day,
#' recording device or number of channels, so those are left empty, as is
#' info_url: the Orthoptera Species File has no page for a sound.
#'
#' @param token Public project token. The Orthoptera Species File's is the
#'   default. It is not a credential: <https://sfg.taxonworks.org/api/v1/>
#'   needs no authentication and publishes the token of every open TaxonWorks
#'   project. Override it if the site's token changes.
#' @param per_page Number of sounds per request, from 1 to 1000.
#' @param pause Seconds between requests.
#' @param verbose If TRUE reports harvest progress.
#' @return Data frame in the recordings format, with an empty source column
#'   (see sourceR()), and the links it gives in `attr(x, "links")`.
#' @examples
#' \dontrun{
#' recordings <- sourceR("orthoptera-speciesfile", orthopteraSpeciesFileR())
#' uploadRecordings(db, recordings)
#' }
#' @importFrom curl new_handle curl_escape curl_fetch_memory
#' @importFrom rjson fromJSON
#' @importFrom stats setNames
#' @importFrom utils tail
#' @export
orthopteraSpeciesFileR <- function(token="3oerVKf82_196cIECvHYNg", per_page=100, pause=1, verbose=FALSE) {
  if (!is.character(token) || length(token) != 1 || is.na(token) || !nzchar(token)) {
    stop("token must be a non-empty project token.")
  }
  if (!is.numeric(per_page) || length(per_page) != 1 || !is.finite(per_page) ||
      per_page < 1 || per_page > 1000 || per_page != floor(per_page)) {
    stop("per_page must be an integer from 1 to 1000.")
  }
  if (!is.numeric(pause) || length(pause) != 1 || !is.finite(pause) || pause < 0) {
    stop("pause must be a non-negative number.")
  }
  useragent <- "audioBlastIngest (https://github.com/audioblast/audioBlastIngest)"
  handle <- new_handle(useragent=useragent, connecttimeout=30, timeout=300)
  #nobody makes a HEAD request, so a recording's type and size are read without
  #downloading its audio, and followlocation follows the short link to the file
  files <- new_handle(useragent=useragent, connecttimeout=30, timeout=300,
                      nobody=TRUE, followlocation=TRUE)

  first <- TRUE
  pacing <- function() {
    if (!first) Sys.sleep(pause)
    first <<- FALSE
  }
  fetch <- function(path) {
    pacing()
    orthopteraFetch(path, token, handle)
  }
  fileInfo <- function(url) {
    pacing()
    orthopteraFile(url, files)
  }

  taxa <- new.env(parent=emptyenv())
  lookup <- function(id) {
    if (!exists(id, envir=taxa, inherits=FALSE)) {
      otu <- fetch(paste0("otus/", id, "?extend[]=taxon_name"))$data
      if (!is.list(otu) || orthopteraValue(otu$id) != id) stop("Unexpected Orthoptera OTU response.")
      name <- orthopteraValue(otu$taxon_name$cached)
      if (name == "") name <- orthopteraValue(otu$name)
      assign(id, name, envir=taxa)
    }
    get(id, envir=taxa, inherits=FALSE)
  }
  occurrences <- orthopteraOccurrences(fetch, lookup)

  pages <- list()
  linked <- list()
  page <- 1
  repeat {
    response <- fetch(paste0("sounds?extend[]=conveyances&extend[]=attribution&per=",
                             per_page, "&page=", page))
    sounds <- response$data
    if (!is.list(sounds) || !is.null(names(sounds)) ||
        !all(vapply(sounds, is.list, logical(1)))) stop("Unexpected Orthoptera sounds response.")
    rows <- orthopteraRecordings(sounds, lookup, occurrences, fileInfo)
    pages[[length(pages) + 1]] <- rows
    linked[[length(linked) + 1]] <- attr(rows, "links")
    if (verbose) message("  Orthoptera Species File: page ", page)
    total <- suppressWarnings(as.numeric(response$total_pages))
    if (length(total) != 1 || !is.finite(total) || total < 0 || total != floor(total)) {
      stop("Missing or invalid Orthoptera pagination headers.")
    }
    if (page >= total) break
    if (length(sounds) == 0) stop("Unexpected empty Orthoptera page before harvest completion.")
    page <- page + 1
  }

  data <- do.call(rbind, pages)
  #Results that change while paging can repeat a sound on two pages
  data <- data[!duplicated(data$id), , drop=FALSE]
  rownames(data) <- NULL
  links <- do.call(rbind, linked)
  links <- links[links$subject_id %in% data$id, , drop=FALSE]
  links <- links[!duplicated(links), , drop=FALSE]
  rownames(links) <- NULL
  attr(data, "links") <- links
  if (verbose) message("  Orthoptera Species File recordings: ", nrow(data),
                       ", links: ", nrow(links))
  return(data)
}

#The backoff of the package's other harvesters (see xenocantoFetch()): a failed
#request stops the harvest, so it is worth waiting out an outage
orthopteraBackoff <- c(1, 1, 2, 3, 5, 10, 30, 60)

orthopteraFetch <- function(path, token, handle, backoff=orthopteraBackoff) {
  url <- paste0("https://sfg.taxonworks.org/api/v1/", path,
                if (grepl("?", path, fixed=TRUE)) "&" else "?",
                "project_token=", curl_escape(token))
  for (wait in c(backoff, NA)) {
    response <- tryCatch(curl_fetch_memory(url, handle=handle), error=function(e) e)
    if (inherits(response, "error")) {
      problem <- conditionMessage(response)
    } else {
      status <- response$status_code
      body <- rawToChar(response$content)
      Encoding(body) <- "UTF-8"
      data <- tryCatch(fromJSON(body), error=function(e) NULL)
      if (status == 200 && !is.null(data)) {
        return(list(data=data,
                    total_pages=orthopteraHeader(response$headers, "pagination-total-pages")))
      }
      #Errors are {"success": false, "message": message}
      reason <- if (is.list(data)) data[["message"]] else NULL
      if (length(reason) != 1 || !is.character(reason)) {
        reason <- if (is.null(data)) "invalid JSON" else "unexpected response"
      }
      problem <- paste0(reason, " (HTTP ", status, ")")
      #Other client errors, such as a bad path or token, will not succeed on retry
      if (status >= 400 && status < 500 && status != 429) break
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  #The token is in the URL, never in path, so this message does not carry it
  stop("Orthoptera Species File request for ", path, " failed: ", problem)
}

#The MIME type and size of a recording's audio, which the API does not give as
#sound fields. A HEAD request reads the headers of the file the sound's short
#link leads to, without downloading it. Audio that can't be reached leaves both
#empty rather than stopping the harvest: the recording is still a recording.
#Retries are short because there is one of these requests for every recording.
orthopteraFile <- function(url, handle, backoff=c(1, 2, 5)) {
  for (wait in c(backoff, NA)) {
    response <- tryCatch(curl_fetch_memory(url, handle=handle), error=function(e) e)
    if (!inherits(response, "error") && response$status_code == 200) {
      #A Content-Type can carry parameters, e.g. audio/x-wav; charset=binary
      type <- sub(";.*$", "", orthopteraHeader(response$headers, "content-type"))
      size <- orthopteraHeader(response$headers, "content-length")
      return(list(type=orthopteraValue(mimeType(type)),
                  size=if (grepl("^[0-9]+$", size)) size else ""))
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  return(list(type="", size=""))
}

#The last value of a response header, which redirects give more than once
orthopteraHeader <- function(headers, name) {
  lines <- strsplit(rawToChar(headers), "\r\n", fixed=TRUE)[[1]]
  found <- grep(paste0("^", name, ":"), lines, ignore.case=TRUE, value=TRUE)
  if (length(found) == 0) return("")
  return(trimws(sub("^[^:]+:", "", tail(found, 1))))
}

orthopteraValue <- function(x) {
  if (is.null(x) || !is.atomic(x) || length(x) != 1 || is.na(x)) return("")
  trimws(enc2utf8(as.character(x)))
}

orthopteraRecordings <- function(sounds, lookup,
                                 occurrences=function(sound) list(records=list(), taxa=character(), inferred=""),
                                 fileInfo=function(url) list(type="", size="")) {
  columns <- names(getHeaders("recordings"))
  data <- as.data.frame(setNames(rep(list(rep("", length(sounds))), length(columns)), columns),
                        stringsAsFactors=FALSE)
  links <- getHeaders("links")
  for (i in seq_along(sounds)) {
    sound <- sounds[[i]]
    id <- orthopteraValue(sound$id)
    if (!grepl("^[0-9]+$", id)) stop("Orthoptera sound is missing a valid id.")
    data$id[i] <- id
    data$file[i] <- orthopteraValue(httpURL(orthopteraValue(sound$sound_file)))
    #Without audio there is nothing to listen to or analyse
    if (data$file[i] == "" || identical(sound$metadata$error, "Missing sound file")) next
    data$Title[i] <- orthopteraValue(sound$name)
    if (data$Title[i] == "") data$Title[i] <- paste("OSF", id)
    data$post_date[i] <- orthopteraValue(isoDate(orthopteraValue(sound$created_at)))
    data$Duration[i] <- orthopteraValue(positiveNumber(orthopteraValue(sound$metadata$duration)))
    data$sample_rate[i] <- orthopteraValue(wholeNumber(orthopteraValue(sound$metadata$sample_rate)))

    attribution <- orthopteraAttribution(orthopteraValue(sound$attribution$label))
    data$author[i] <- attribution$author
    data$rights_holder[i] <- attribution$rights_holder
    data$license[i] <- attribution$license

    audio <- fileInfo(data$file[i])
    data$type[i] <- audio$type
    data$size_raw[i] <- audio$size

    #A recording was made where and when the specimen or observation it conveys
    #was collected; the occurrences of a collecting event share both
    found <- occurrences(sound)
    data$Date[i] <- orthopteraDate(orthopteraAgreed(found$records, "eventDate"))
    data$lat[i] <- orthopteraAgreed(found$records, "decimalLatitude")
    data$lon[i] <- orthopteraAgreed(found$records, "decimalLongitude")
    #Darwin Core names a country rather than coding it, which countryCode()
    #reads, as it does the dates and coordinates above
    data$country[i] <- orthopteraAgreed(found$records, "country")
    data$locality[i] <- orthopteraAgreed(found$records, "verbatimLocality")
    #Attribution says who created the sound; a Darwin Core record says who
    #recorded the animal, which is the author where there is no attribution
    if (data$author[i] == "") data$author[i] <- orthopteraAgreed(found$records, "recordedBy")

    otus <- unique(c(orthopteraConveyed(sound, "Otu"), found$taxa))
    taxonNames <- unique(vapply(otus, lookup, character(1), USE.NAMES=FALSE))
    taxonNames <- taxonNames[taxonNames != ""]
    if (length(taxonNames) == 1) {
      data$taxon[i] <- taxonNames
    } else if (length(taxonNames) > 1) {
      #taxon holds one dwc:scientificName, so a recording of several taxa says
      #what it is about as a relationship to each of them
      links <- rbind(links, orthopteraTaxonLinks(id, otus))
    }
    if (found$inferred != "") {
      links <- rbind(links, orthopteraTaxonLinks(
        id, found$inferred,
        qualifier="https://vocab.audioblast.org/cv/identificationBasis#RecordingTitle",
        remarks="Name matched in the recording's title; the Orthoptera Species File does not identify this recording."))
    }
  }

  keep <- data$file != "" &
    !vapply(sounds, function(s) identical(s$metadata$error, "Missing sound file"), logical(1))
  data <- data[keep, , drop=FALSE]
  rownames(data) <- NULL
  links <- links[links$subject_id %in% data$id, , drop=FALSE]
  rownames(links) <- NULL
  attr(data, "links") <- links
  return(data)
}

#The ids of the records of one type that a sound is conveyed on
orthopteraConveyed <- function(sound, type) {
  conveyed <- Filter(function(link) identical(link$conveyance_object_type, type), sound$conveyances)
  ids <- vapply(conveyed, function(link) orthopteraValue(link$conveyance_object_id), character(1))
  return(unique(ids[grepl("^[0-9]+$", ids)]))
}

#The links saying that a recording is about a taxon, which the taxon column
#cannot hold. The taxon is named by the Orthoptera Species File's page for the
#OTU, as audioBlast! does not hold its taxa. is about, rather than an
#identification, is what a recording of an animal supports.
orthopteraTaxonLinks <- function(id, otus, qualifier="", remarks="") {
  return(data.frame(
    source="", subject_type="recordings", subject_source="", subject_id=id,
    predicate="http://purl.obolibrary.org/obo/IAO_0000136",
    object_type="iri", object_source="",
    object_id=paste0("https://orthoptera.speciesfile.org/otus/", otus),
    qualifier=qualifier, remarks=remarks, reference="", stringsAsFactors=FALSE))
}

#The one value the Darwin Core records of a sound give for a field, which is
#empty where they give none or disagree: the occurrences of a collecting event
#share where and when it happened, but two specimens needn't
orthopteraAgreed <- function(records, field) {
  values <- unique(vapply(records, function(r) orthopteraValue(r[[field]]), character(1)))
  values <- values[values != ""]
  if (length(values) == 1) return(values)
  return("")
}

#TaxonWorks writes a date known only to the year as the whole of that year
#(1971-01-01/1971-12-31). Other dates are passed on as they are given, for
#normaliseRecordings() to read and to warn about if it cannot.
orthopteraDate <- function(x) {
  parts <- strsplit(x, "/", fixed=TRUE)[[1]]
  if (length(parts) == 2 && nchar(parts[1]) == 10 &&
      parts[1] == paste0(substr(parts[1], 1, 4), "-01-01") &&
      parts[2] == paste0(substr(parts[1], 1, 4), "-12-31")) {
    return(substr(parts[1], 1, 4))
  }
  return(x)
}

#The parts of an attribution label, which is the only form the API gives
#attribution in, e.g. "(c)2020. Created by Holger Braun. License: CC BY 4.0".
#"Created by" names who made the recording; a copyright sign and year name who
#holds the rights, who is the named creator where there is one. A label that
#only claims copyright gives no author.
orthopteraAttribution <- function(label) {
  license <- ""
  parts <- regmatches(label, regexec("^(.*?)[.]?[[:space:]]*License:[[:space:]]*(.*)$", label))[[1]]
  if (length(parts) == 3) {
    label <- trimws(parts[2])
    license <- orthopteraLicense(trimws(parts[3]))
  }
  rights <- ""
  #A copyright sign is the run of non-alphanumeric characters before the year
  claim <- regmatches(label, regexec("^[^[:alnum:]]+[0-9]{4}[.]?[[:space:]]*(.*)$", label))[[1]]
  if (length(claim) == 2) {
    label <- trimws(claim[2])
    rights <- label
  }
  author <- ""
  created <- regmatches(label, regexec("^Created by[[:space:]]+(.*)$", label))[[1]]
  if (length(created) == 2) author <- trimws(created[2])
  if (rights != "" && author != "") rights <- author
  return(list(author=author, rights_holder=rights, license=license))
}

#The URLs of the licences the labels name; anything else is left empty rather
#than guessed
orthopteraLicense <- function(text) {
  licenses <- c(
    "CC BY 4.0"="https://creativecommons.org/licenses/by/4.0/",
    "CC BY-SA 4.0"="https://creativecommons.org/licenses/by-sa/4.0/",
    "CC BY-ND 4.0"="https://creativecommons.org/licenses/by-nd/4.0/",
    "CC BY-NC 4.0"="https://creativecommons.org/licenses/by-nc/4.0/",
    "CC BY-NC-SA 4.0"="https://creativecommons.org/licenses/by-nc-sa/4.0/",
    "CC BY-NC-ND 4.0"="https://creativecommons.org/licenses/by-nc-nd/4.0/",
    "CC0 1.0"="https://creativecommons.org/publicdomain/zero/1.0/")
  url <- unname(licenses[toupper(gsub("[[:space:]]+", " ", trimws(text)))])
  if (is.na(url)) return("")
  return(url)
}

#Reads the specimens, field observations and collecting events a sound is
#conveyed on as Darwin Core records, which give its taxon and where and when it
#was recorded. Reading is cached by record, so a specimen that two recordings
#share is read once, and a collecting event's occurrences are paged once.
orthopteraOccurrences <- function(fetch, lookup) {
  cache <- new.env(parent=emptyenv())
  cached <- function(key, read) {
    if (!exists(key, envir=cache, inherits=FALSE)) assign(key, read(), envir=cache)
    get(key, envir=cache, inherits=FALSE)
  }
  otuIds <- function(records) {
    ids <- vapply(records, function(r) orthopteraValue(r$otu_id), character(1))
    unique(ids[grepl("^[0-9]+$", ids)])
  }
  function(sound) {
    records <- list()
    taxa <- character()
    indirect <- FALSE
    for (link in sound$conveyances) {
      id <- orthopteraValue(link$conveyance_object_id)
      type <- orthopteraValue(link$conveyance_object_type)
      if (!grepl("^[0-9]+$", id)) next
      if (type %in% c("CollectionObject", "FieldOccurrence")) {
        #A specimen or field observation is one Darwin Core record, whose
        #otu_id is its accepted determination
        path <- if (type == "CollectionObject") "collection_objects/" else "field_occurrences/"
        found <- cached(paste0(type, id), function() {
          record <- fetch(paste0(path, id, "/dwc"))$data
          if (!is.list(record) || is.null(names(record))) {
            stop("Unexpected Orthoptera occurrence response.")
          }
          list(records=list(record), taxa=otuIds(list(record)))
        })
        if (type == "FieldOccurrence") indirect <- TRUE
      } else if (type == "CollectingEvent") {
        indirect <- TRUE
        found <- cached(paste0(type, id), function() {
          occurrences <- unlist(lapply(c("collection_object", "field_occurrence"), function(kind) {
            orthopteraPages(paste0("dwc_occurrences?", kind, "_query[collecting_event_id]=", id), fetch)
          }), recursive=FALSE)
          #An event says what was recorded only where all of its records are
          #identified and agree on one taxon
          candidates <- otuIds(occurrences)
          unidentified <- vapply(occurrences,
                                 function(r) !grepl("^[0-9]+$", orthopteraValue(r$otu_id)),
                                 logical(1))
          taxon <- character()
          if (length(occurrences) > 0 && !any(unidentified)) {
            agreed <- unique(vapply(candidates, lookup, character(1), USE.NAMES=FALSE))
            if (length(agreed) == 1 && agreed != "") taxon <- candidates[1]
          }
          list(records=occurrences, taxa=taxon)
        })
      } else {
        next
      }
      records <- c(records, found$records)
      taxa <- c(taxa, found$taxa)
    }

    #A binomial that is the whole title or precedes a numbered recording label.
    #Do not infer names from arbitrary prose, or truncate subspecies names.
    inferred <- ""
    if (!length(taxa) && indirect) {
      title <- orthopteraValue(sound$name)
      match <- regmatches(title, regexpr("^[A-Z][a-z]+ [a-z][a-z-]+(?= [0-9]+(?: |$)|$)", title, perl=TRUE))
      if (length(match) && nzchar(match)) {
        inferred <- cached(paste0("title:", match), function() {
          otus <- orthopteraPages(paste0("otus?taxon_name_query[cached]=", curl_escape(match),
                                         "&extend[]=taxon_name"), fetch)
          accepted <- Filter(function(otu) {
            identical(orthopteraValue(otu$taxon_name$cached), match) &&
              isTRUE(otu$taxon_name$cached_is_valid) &&
              grepl("^[0-9]+$", orthopteraValue(otu$id))
          }, otus)
          if (length(accepted) == 1) orthopteraValue(accepted[[1]]$id) else ""
        })
      }
    }
    list(records=records, taxa=unique(taxa), inferred=inferred)
  }
}

orthopteraPages <- function(path, fetch) {
  records <- list()
  page <- 1
  repeat {
    response <- fetch(paste0(path, "&per=100&page=", page))
    data <- response$data
    if (!is.list(data) || !is.null(names(data)) || !all(vapply(data, is.list, logical(1)))) {
      stop("Unexpected Orthoptera lookup response.")
    }
    total <- suppressWarnings(as.numeric(response$total_pages))
    if (length(total) != 1 || !is.finite(total) || total < 0 || total != floor(total)) {
      stop("Missing or invalid Orthoptera lookup pagination headers.")
    }
    records <- c(records, data)
    if (page >= total) break
    if (!length(data)) stop("Unexpected empty Orthoptera lookup page.")
    page <- page + 1
  }
  records
}

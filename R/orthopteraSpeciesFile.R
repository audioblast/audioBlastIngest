#' Harvest recordings from Orthoptera Species File
#'
#' Reads the public TaxonWorks sounds API, including attribution and linked
#' taxa, including accepted identifications of linked specimens and field
#' occurrences. Event links are resolved when their occurrences identify a
#' single taxon. Otherwise, an explicitly named recording can be matched to
#' an exact, unique accepted taxon in OSF. Unresolved taxa remain empty.
#' Audio files are not downloaded. Unknown recording dates, locations,
#' licences and file types are left empty. Attribution labels are preserved
#' verbatim in author. Multiple linked taxa are separated by semicolons.
#'
#' @param token Public project token published by Orthoptera Species File.
#'   Override if the site's token changes.
#' @param per_page Number of sounds per request, from 1 to 1000.
#' @param pause Seconds between requests.
#' @param verbose If TRUE reports harvest progress.
#' @return Data frame in the recordings format, with an empty source column.
#' @examples
#' \dontrun{
#' recordings <- sourceR("orthoptera-speciesfile", orthopteraSpeciesFileR())
#' uploadRecordings(db, recordings)
#' }
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
  handle <- curl::new_handle(
    useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
    connecttimeout=30, timeout=300)
  first <- TRUE
  fetch <- function(path) {
    if (!first) Sys.sleep(pause)
    first <<- FALSE
    orthopteraFetch(path, token, handle)
  }
  taxa <- new.env(parent=emptyenv())
  specimens <- new.env(parent=emptyenv())
  specimenTaxa <- function(id) {
    if (!exists(id, envir=specimens, inherits=FALSE)) {
      object <- fetch(paste0("collection_objects/", id, "?extend[]=taxon_determinations"))$data
      if (!is.list(object) || orthopteraValue(object$id) != id) stop("Unexpected Orthoptera specimen response.")
      #Position 1 is the accepted determination, not necessarily the newest.
      current <- Filter(function(d) identical(orthopteraValue(d$position), "1"), object$taxon_determinations)
      ids <- vapply(current, function(d) orthopteraValue(d$otu_id), character(1))
      assign(id, ids[grepl("^[0-9]+$", ids)], envir=specimens)
    }
    get(id, envir=specimens, inherits=FALSE)
  }
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
  relatedTaxa <- orthopteraRelatedTaxa(fetch, lookup)
  pages <- list()
  page <- 1
  repeat {
    response <- fetch(paste0("sounds?extend[]=conveyances&extend[]=attribution&per=",
                             per_page, "&page=", page))
    sounds <- response$data
    if (!is.list(sounds) || !is.null(names(sounds)) ||
        !all(vapply(sounds, is.list, logical(1)))) stop("Unexpected Orthoptera sounds response.")
    pages[[length(pages) + 1]] <- orthopteraRecordings(sounds, lookup, specimenTaxa, relatedTaxa)
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
  data <- data[!duplicated(data$id), , drop=FALSE]
  rownames(data) <- NULL
  return(data)
}

orthopteraFetch <- function(path, token, handle, backoff=c(1, 2, 5)) {
  url <- paste0("https://sfg.taxonworks.org/api/v1/", path,
                if (grepl("?", path, fixed=TRUE)) "&" else "?",
                "project_token=", curl::curl_escape(token))
  for (wait in c(backoff, NA)) {
    response <- tryCatch(curl_fetch_memory(url, handle=handle), error=function(e) NULL)
    if (is.null(response)) {
      problem <- "network error"
    } else {
      problem <- paste("HTTP", response$status_code)
      if (response$status_code == 200) {
        body <- rawToChar(response$content)
        Encoding(body) <- "UTF-8"
        data <- tryCatch(rjson::fromJSON(body), error=function(e) NULL)
        if (!is.null(data)) {
          headers <- strsplit(rawToChar(response$headers), "\r\n", fixed=TRUE)[[1]]
          total <- grep("^pagination-total-pages:", headers, ignore.case=TRUE, value=TRUE)
          return(list(data=data, total_pages=trimws(sub("^[^:]+:", "", utils::tail(total, 1)))))
        }
        problem <- "invalid JSON"
      }
      if (response$status_code >= 400 && response$status_code < 500 && response$status_code != 429) break
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  stop("Orthoptera Species File request for ", path, " failed: ", problem)
}

orthopteraValue <- function(x) {
  if (is.null(x) || !is.atomic(x) || length(x) != 1 || is.na(x)) return("")
  trimws(enc2utf8(as.character(x)))
}

orthopteraRecordings <- function(sounds, lookup, specimenTaxa=function(id) character(),
                                relatedTaxa=function(sound) character()) {
  columns <- names(getHeaders("recordings"))
  data <- as.data.frame(setNames(rep(list(rep("", length(sounds))), length(columns)), columns),
                        stringsAsFactors=FALSE)
  for (i in seq_along(sounds)) {
    sound <- sounds[[i]]
    id <- orthopteraValue(sound$id)
    if (!grepl("^[0-9]+$", id)) stop("Orthoptera sound is missing a valid id.")
    data$id[i] <- id
    data$file[i] <- orthopteraValue(httpURL(orthopteraValue(sound$sound_file)))
    if (data$file[i] == "" || identical(sound$metadata$error, "Missing sound file")) next
    data$Title[i] <- orthopteraValue(sound$name)
    if (data$Title[i] == "") data$Title[i] <- paste("OSF", id)
    data$author[i] <- orthopteraValue(sound$attribution$label)
    data$post_date[i] <- orthopteraValue(isoDate(orthopteraValue(sound$created_at)))
    data$Duration[i] <- orthopteraValue(positiveNumber(orthopteraValue(sound$metadata$duration)))
    ids <- unique(vapply(Filter(function(c) identical(c$conveyance_object_type, "Otu"),
                               sound$conveyances), function(c) orthopteraValue(c$conveyance_object_id), character(1)))
    ids <- ids[grepl("^[0-9]+$", ids)]
    specimens <- unique(vapply(Filter(function(c) identical(c$conveyance_object_type, "CollectionObject"),
                                     sound$conveyances), function(c) orthopteraValue(c$conveyance_object_id), character(1)))
    specimens <- specimens[grepl("^[0-9]+$", specimens)]
    ids <- unique(c(ids, unlist(lapply(specimens, specimenTaxa), use.names=FALSE)))
    ids <- unique(c(ids, relatedTaxa(sound)))
    names <- unique(vapply(ids, lookup, character(1), USE.NAMES=FALSE))
    data$taxon[i] <- paste(names[names != ""], collapse=";")
    data$info_url[i] <- if (length(ids) == 1) paste0("https://orthoptera.speciesfile.org/otus/", ids) else
      "https://orthoptera.speciesfile.org/"
  }
  data <- data[data$file != "" & !vapply(sounds, function(s) identical(s$metadata$error, "Missing sound file"), logical(1)), , drop=FALSE]
  rownames(data) <- NULL
  data
}

#Resolve indirect links without treating every species at a multi-species
#collecting event as an identification of the recording.
orthopteraRelatedTaxa <- function(fetch, lookup) {
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
    ids <- character()
    indirect <- FALSE
    for (link in sound$conveyances) {
      id <- orthopteraValue(link$conveyance_object_id)
      type <- orthopteraValue(link$conveyance_object_type)
      if (!grepl("^[0-9]+$", id)) next
      if (type == "FieldOccurrence") {
        indirect <- TRUE
        ids <- c(ids, cached(paste0(type, id), function() {
          record <- fetch(paste0("field_occurrences/", id, "/dwc"))$data
          if (!is.list(record) || is.null(names(record))) stop("Unexpected Orthoptera field occurrence response.")
          otuIds(list(record))
        }))
      }
      if (type == "CollectingEvent") {
        indirect <- TRUE
        ids <- c(ids, cached(paste0(type, id), function() {
          records <- unlist(lapply(c("collection_object", "field_occurrence"), function(kind) {
            orthopteraPages(paste0("dwc_occurrences?", kind, "_query[collecting_event_id]=", id), fetch)
          }), recursive=FALSE)
          candidates <- otuIds(records)
          #All event records must be identified, and agree on the taxon.
          if (!length(records) || any(vapply(records, function(r) !grepl("^[0-9]+$", orthopteraValue(r$otu_id)), logical(1)))) return(character())
          names <- unique(vapply(candidates, lookup, character(1)))
          if (length(names) == 1 && names != "") candidates[1] else character()
        }))
      }
    }
    #A binomial that is the whole title or precedes a numbered recording label.
    #Do not infer names from arbitrary prose, or truncate subspecies names.
    if (!length(ids) && indirect) {
      title <- orthopteraValue(sound$name)
      match <- regmatches(title, regexpr("^[A-Z][a-z]+ [a-z][a-z-]+(?= [0-9]+(?: |$)|$)", title, perl=TRUE))
      if (length(match) && nzchar(match)) {
        ids <- cached(paste0("title:", match), function() {
          otus <- orthopteraPages(paste0("otus?taxon_name_query[cached]=", curl::curl_escape(match), "&extend[]=taxon_name"), fetch)
          accepted <- Filter(function(otu) {
            identical(orthopteraValue(otu$taxon_name$cached), match) &&
              isTRUE(otu$taxon_name$cached_is_valid) &&
              grepl("^[0-9]+$", orthopteraValue(otu$id))
          }, otus)
          if (length(accepted) == 1) orthopteraValue(accepted[[1]]$id) else character()
        })
      }
    }
    unique(ids)
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

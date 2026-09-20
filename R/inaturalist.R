#' Harvest recordings from iNaturalist
#'
#' Harvests recording metadata from the iNaturalist API (version 2) and
#' converts it to the audioBlast! recordings format.
#'
#' Harvests are limited to the taxa given, as iNaturalist taxon ids
#' (<https://www.inaturalist.org/taxa>), e.g. 47651 for Orthoptera and 50186
#' for Cicadidae. Each is harvested in turn, and "" is every taxon.
#'
#' An observation is not a recording: it can carry several sounds, and each of
#' them is a recording of its own, identified by the sound's id rather than the
#' observation's. Sounds that have been taken down are left out, as are sounds
#' that are All Rights Reserved, whose licence this API could not state (see
#' inaturalistLicenses).
#'
#' iNaturalist caps a search at 10000 results however it is paged, so pages are
#' taken as a sliding window of ids rather than by number. Reading needs no API
#' key; iNaturalist asks that requests identify themselves with a user agent
#' and that there are no more than 60 of them a minute.
#'
#' @param taxon_id Character vector of iNaturalist taxon ids, harvested in
#'   turn. An element can name several taxa at once, separated by commas, and
#'   "" is every taxon.
#' @param quality_grade Quality grades of the observations to harvest, as a
#'   comma separated list. "research" is those whose identification the
#'   community has agreed.
#' @param per_page Number of observations per API request, from 1 to 200.
#' @param pause Seconds to wait between API requests.
#' @param verbose If TRUE says more about what's going on.
#' @return Data frame of recordings, with an empty source column (see
#'   sourceR()).
#' @examples
#' \dontrun{
#' recordings <- sourceR("iNaturalist", inaturalistR(c("47651", "50186")))
#' uploadRecordings(db, recordings)
#' }
#' @importFrom curl new_handle
#' @export
inaturalistR <- function(taxon_id, quality_grade="research", per_page=200, pause=1, verbose=FALSE) {
  if (!is.character(taxon_id) || length(taxon_id) == 0 || any(is.na(taxon_id)) ||
      !all(grepl("^([0-9]+(,[0-9]+)*)?$", taxon_id))) {
    stop("taxon_id must be one or more iNaturalist taxon ids, or \"\" for every taxon.")
  }
  if (!is.character(quality_grade) || length(quality_grade) != 1 || is.na(quality_grade) ||
      !grepl("^[a-z_]+(,[a-z_]+)*$", quality_grade)) {
    stop("quality_grade must be one or more iNaturalist quality grades.")
  }
  if (!is.numeric(per_page) || length(per_page) != 1 || is.na(per_page) ||
      per_page < 1 || per_page > 200) {
    stop("per_page must be between 1 and 200.")
  }

  handle <- new_handle(
    useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
    connecttimeout=30,
    timeout=300)

  pages <- list()
  for (t in taxon_id) {
    #A search is capped at 10000 results however it is paged, so each request
    #asks for the observations above the last id of the one before
    id_above <- "0"
    total <- NA_real_
    harvested <- 0
    repeat {
      if (length(pages) > 0) Sys.sleep(pause)
      response <- inaturalistFetch(t, quality_grade, id_above, as.integer(per_page), handle)
      observations <- response[["results"]]
      pages[[length(pages) + 1]] <- inaturalistSounds(observations)
      #total_results counts what is left above id_above, so the first page of a
      #taxon is the only one that says how many there are altogether
      if (is.na(total)) total <- suppressWarnings(as.numeric(response[["total_results"]]))
      harvested <- harvested + length(observations)
      if (verbose) {
        print(paste0("  iNaturalist ", t, ": ", harvested, " of ",
                     ifelse(is.na(total), "?", format(total, scientific=FALSE, trim=TRUE)),
                     " observations"))
      }
      if (length(observations) < per_page) break
      id_above <- inaturalistLastID(observations)
    }
  }

  #Combine column by column, which is far quicker than rbind for many pages
  headers <- names(getHeaders("recordings"))
  data <- lapply(headers, function(h) as.character(unlist(lapply(pages, `[[`, h), use.names=FALSE)))
  names(data) <- headers
  data <- as.data.frame(data, stringsAsFactors=FALSE)

  #An observation edited while a taxon is being paged can be read on two pages
  data <- data[!duplicated(data$id), ]
  rownames(data) <- NULL
  if (verbose) print(paste("  iNaturalist recordings:", nrow(data)))
  return(data)
}

#The licences iNaturalist gives a sound, and the licence URL of each. They are
#Creative Commons 4.0, which is what GBIF's export of the same records gives
#them as.
#
#No-derivatives licences are harvested: audioBlast! links to a recording where
#it lives and never copies it, so it never makes a derivative of one. A sound
#with no licence at all is All Rights Reserved, and is not harvested: its
#licence column would have to be empty, and a recording whose licence this API
#cannot state is worse to a reader than no recording at all.
inaturalistLicenses <- c(
  "cc0"="https://creativecommons.org/publicdomain/zero/1.0/",
  "cc-by"="https://creativecommons.org/licenses/by/4.0/",
  "cc-by-sa"="https://creativecommons.org/licenses/by-sa/4.0/",
  "cc-by-nd"="https://creativecommons.org/licenses/by-nd/4.0/",
  "cc-by-nc"="https://creativecommons.org/licenses/by-nc/4.0/",
  "cc-by-nc-sa"="https://creativecommons.org/licenses/by-nc-sa/4.0/",
  "cc-by-nc-nd"="https://creativecommons.org/licenses/by-nc-nd/4.0/")

#The fields of an observation that a recording is made of. Version 2 of the API
#returns the fields it is asked for and no others, which is a page of 180 KB
#rather than 12 MB.
inaturalistFields <- paste0(
  "(id:!t,observed_on:!t,time_observed_at:!t,created_at:!t,location:!t,place_guess:!t,",
  "taxon:(name:!t,preferred_common_name:!t),",
  "user:(name:!t,login:!t),",
  "sounds:(id:!t,license_code:!t,file_url:!t,file_content_type:!t,hidden:!t))")

#' @importFrom curl curl_escape curl_fetch_memory
#' @importFrom rjson fromJSON
inaturalistFetch <- function(taxon_id, quality_grade, id_above, per_page, handle,
                             backoff=c(1,1,2,3,5,10,30,60)) {
  url <- paste0(
    "https://api.inaturalist.org/v2/observations",
    "?sounds=true",
    "&taxon_id=", taxon_id,
    "&quality_grade=", curl_escape(quality_grade),
    "&sound_license=", paste(names(inaturalistLicenses), collapse=","),
    "&order_by=id&order=asc",
    "&id_above=", id_above,
    "&per_page=", per_page,
    "&fields=", inaturalistFields)

  for (wait in c(backoff, NA)) {
    response <- tryCatch(curl_fetch_memory(url, handle=handle), error=function(e) e)
    if (inherits(response, "error")) {
      problem <- conditionMessage(response)
    } else {
      json <- tryCatch({
        body <- rawToChar(response$content)
        Encoding(body) <- "UTF-8"
        fromJSON(body)
      }, error=function(e) NULL)
      if (!is.list(json)) json <- list()
      status <- response$status_code
      if (status == 200 && is.list(json[["results"]])) {
        return(json)
      }
      #Errors are {"errors": [{"errorCode", "message"}]}, and {"error": message}
      #in version 1 of the API
      reason <- if (is.list(json[["errors"]]) && length(json[["errors"]]) > 0) {
        json[["errors"]][[1]][["message"]]
      } else {
        json[["error"]]
      }
      if (length(reason) != 1 || !is.character(reason)) reason <- "unexpected response"
      problem <- paste0(reason, " (HTTP ", status, ")")
      #Other client errors, such as an unknown taxon, will not succeed on retry
      if (status >= 400 && status < 500 && status != 429) break
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  stop(paste0("iNaturalist request for taxon '", taxon_id, "' above id ", id_above,
              " failed: ", problem))
}

#Converts a page of observations to the recordings format, with a recording for
#each of the sounds an observation carries
inaturalistSounds <- function(observations) {
  #An observation is not a recording: each of the sounds it carries is one
  pairs <- unlist(lapply(observations, function(o) {
    sounds <- o[["sounds"]]
    if (!is.list(sounds)) return(NULL)
    lapply(sounds, function(s) list(observation=o, sound=s))
  }), recursive=FALSE, use.names=FALSE)

  field <- function(record, ...) {
    path <- c(...)
    vapply(pairs, function(p) {
      value <- p[[record]]
      for (key in path) {
        if (!is.list(value)) return("")
        value <- value[[key]]
      }
      if (length(value) != 1 || is.list(value) || is.na(value)) return("")
      if (is.numeric(value)) value <- format(value, scientific=FALSE, digits=15, trim=TRUE)
      value <- as.character(value)
      Encoding(value) <- "UTF-8"
      return(trimws(value))
    }, character(1), USE.NAMES=FALSE)
  }
  observation <- function(...) field("observation", ...)
  sound <- function(...) field("sound", ...)
  empty <- rep_len("", length(pairs))

  observed <- observation("id")
  taxon <- observation("taxon", "name")
  #The observer made the recording and holds the rights in it
  observer <- inaturalistObserver(observation("user", "name"), observation("user", "login"))
  location <- inaturalistCoordinates(observation("location"))

  data <- data.frame(
    source=empty,
    id=sound("id"),
    Title=inaturalistTitle(observed, observation("taxon", "preferred_common_name"), taxon),
    taxon=taxon,
    file=inaturalistFile(sound("file_url")),
    author=observer,
    post_date=inaturalistDate(observation("created_at")),
    size=empty,
    size_raw=empty,
    type=inaturalistMime(sound("file_content_type")),
    NonSpecimen=empty,
    Date=inaturalistDate(observation("observed_on")),
    Time=inaturalistTime(observation("time_observed_at")),
    Duration=empty,
    deployment=empty,
    lat=coordinate(location$lat, 90),
    lon=coordinate(location$lon, 180),
    time_of_day=empty,
    license=inaturalistLicense(sound("license_code")),
    info_url=inaturalistPage(observed),
    device=empty,
    rights_holder=observer,
    #iNaturalist has no country code to read, only a place in the observer's own
    #words, and it gives a sound no duration, sample rate, channels or size
    country=empty,
    locality=observation("place_guess"),
    sample_rate=empty,
    channels=empty,
    stringsAsFactors=FALSE)

  #Without audio there is nothing to listen to or analyse, a sound that has been
  #taken down should not be linked to, and a sound that is All Rights Reserved
  #has no licence to give (see inaturalistLicenses)
  takenDown <- tolower(sound("hidden")) == "true"
  data <- data[data$id != "" & data$file != "" & data$license != "" & !takenDown, ]
  rownames(data) <- NULL
  return(data)
}

#The largest id of a page of observations, which the next request asks for the
#observations above. Ids are beyond what an R integer holds, so they are kept as
#numbers and written without an exponent.
inaturalistLastID <- function(observations) {
  ids <- vapply(observations, function(o) {
    id <- suppressWarnings(as.numeric(o[["id"]]))
    if (length(id) != 1 || is.na(id)) return(NA_real_)
    return(id)
  }, numeric(1), USE.NAMES=FALSE)
  if (all(is.na(ids))) {
    stop("iNaturalist returned a page of observations with no id to page on")
  }
  return(format(max(ids, na.rm=TRUE), scientific=FALSE, trim=TRUE))
}

#The observer's name as iNaturalist shows it, or their login where they have
#given no name. The attribution iNaturalist writes for a sound is not read: it
#is kept as the observer was named when the sound was uploaded, so it is out of
#date for an observer who has since been renamed, and it is written in words
#that change with the language it is asked for.
inaturalistObserver <- function(name, login) {
  return(as.character(ifelse(name != "", name, login)))
}

#The recording's page at iNaturalist, which is its observation's: a sound's own
#URL is the audio file rather than a page about it.
inaturalistPage <- function(id) {
  return(as.character(ifelse(id == "", "",
                             paste0("https://www.inaturalist.org/observations/", id))))
}

#A title of the taxon and the name it is commonly known by, as far as they are
#known, after the observation the recording belongs to. The observation is named
#rather than the sound because it is what the recording's page is of, and what
#iNaturalist itself is searched by.
inaturalistTitle <- function(id, common, taxon) {
  name <- ifelse(common != "" & taxon != "", paste0(common, " (", taxon, ")"),
                 paste0(common, taxon))
  return(as.character(trimws(paste(ifelse(id == "", "", paste0("iNat", id)), name))))
}

#The file a recording is, without the timestamp iNaturalist ends its URL with.
#That timestamp is when the file was last processed rather than when it was
#uploaded, so it changes under a harvest, and the URL serves the same file
#without it. A URL with anything else after its ? is left as it is, in case it
#is ever needed to fetch the file.
inaturalistFile <- function(x) {
  file <- httpURL(sub("\\?[0-9]+$", "", x))
  return(as.character(ifelse(is.na(file), "", file)))
}

#The licence of a sound as a licence URL; empty for All Rights Reserved, which
#is how iNaturalist gives a sound with no licence, and for a licence that is not
#known here
inaturalistLicense <- function(x) {
  url <- unname(inaturalistLicenses[tolower(x)])
  return(as.character(ifelse(is.na(url), "", url)))
}

#The time of day an observation was made at, which iNaturalist gives as part of
#an ISO 8601 timestamp in the observer's own time zone. The clock time is taken
#as it is written and not moved to UTC, as a recording's time is the time of day
#it was made at.
inaturalistTime <- function(x) {
  parts <- regmatches(x, regexec("^[0-9]{4}-[0-9]{2}-[0-9]{2}T([0-9]{2}:[0-9]{2}(:[0-9]{2})?)", x))
  clock <- vapply(parts, function(p) if (length(p) >= 2) p[2] else "", character(1), USE.NAMES=FALSE)
  time <- clockTime(clock)
  return(as.character(ifelse(is.na(time), "", time)))
}

#ISO 8601 dates of the dates iNaturalist gives, which are whole dates and
#timestamps
inaturalistDate <- function(x) {
  date <- isoDate(x)
  return(as.character(ifelse(is.na(date), "", date)))
}

#The MIME type iNaturalist gives a sound, in the names normaliseRecordings()
#uses (audio/x-wav rather than audio/wav)
inaturalistMime <- function(x) {
  mime <- mimeType(x)
  return(as.character(ifelse(is.na(mime), "", mime)))
}

#The latitude and longitude of an observation, which iNaturalist gives as one
#"lat,lon" value. Where a taxon or an observer needs protecting the location is
#obscured: iNaturalist moves it at random within a large cell, and there is no
#truer one to be had.
inaturalistCoordinates <- function(x) {
  parts <- regmatches(x, regexec("^(-?[0-9.]+),(-?[0-9.]+)$", x))
  part <- function(n) {
    vapply(parts, function(p) if (length(p) == 3) p[n] else "", character(1), USE.NAMES=FALSE)
  }
  return(list(lat=part(2), lon=part(3)))
}

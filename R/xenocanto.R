#' Harvest recordings from xeno-canto
#'
#' Harvests recording metadata from the xeno-canto API (version 3) and converts
#' it to the audioBlast! recordings format.
#'
#' Queries are built from xeno-canto search tags
#' (<https://xeno-canto.org/help/search>), e.g. `grp:grasshoppers` or
#' `grp:"land mammals" cnt:brazil`. The `since` tag limits a harvest to recent
#' uploads, e.g. `grp:birds since:31`.
#'
#' xeno-canto recordings are not part of a deployment, so they are located by
#' lat and lon. Recordings of restricted species are omitted, as xeno-canto
#' withholds their audio and location.
#'
#' @param query Character vector of xeno-canto queries, harvested in turn.
#' @param key xeno-canto API key, by default read from the XC_API_KEY
#'   environment variable. Keep keys out of code and version control.
#' @param per_page Number of recordings per API request, from 50 to 500.
#' @param pause Seconds to wait between API requests.
#' @param verbose If TRUE says more about what's going on.
#' @return Data frame of recordings, with an empty source column (see
#'   sourceR()).
#' @examples
#' \dontrun{
#' recordings <- sourceR("xeno-canto", xenocantoR("grp:grasshoppers"))
#' uploadRecordings(db, recordings)
#' }
#' @importFrom curl new_handle
#' @export
xenocantoR <- function(query, key=Sys.getenv("XC_API_KEY"), per_page=500, pause=1, verbose=FALSE) {
  if (!is.character(query) || length(query) == 0 || any(is.na(query) | query == "")) {
    stop("query must be one or more xeno-canto search queries.")
  }
  if (!is.character(key) || length(key) != 1 || is.na(key) || key == "") {
    stop("No xeno-canto API key: set the XC_API_KEY environment variable.")
  }
  if (!is.numeric(per_page) || length(per_page) != 1 || is.na(per_page) || per_page < 50 || per_page > 500) {
    stop("per_page must be between 50 and 500.")
  }

  handle <- new_handle(
    useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
    connecttimeout=30,
    timeout=300)

  pages <- list()
  for (q in query) {
    page <- 1
    repeat {
      if (length(pages) > 0) Sys.sleep(pause)
      response <- xenocantoFetch(q, page, as.integer(per_page), key, handle)
      pages[[length(pages) + 1]] <- xenocantoRecordings(response$recordings)
      if (verbose) print(paste0("  xeno-canto ", q, ": page ", page, " of ", response$numPages))
      if (page >= as.numeric(response$numPages)) break
      page <- page + 1
    }
  }

  #Combine column by column, which is far quicker than rbind for many pages
  headers <- names(getHeaders("recordings"))
  data <- lapply(headers, function(h) unlist(lapply(pages, `[[`, h), use.names=FALSE))
  names(data) <- headers
  data <- as.data.frame(data, stringsAsFactors=FALSE)

  #Results that change while paging can repeat a recording on two pages
  data <- data[!duplicated(data$id), ]
  rownames(data) <- NULL
  if (verbose) print(paste("  xeno-canto recordings:", nrow(data)))
  return(data)
}

#' @importFrom curl curl_escape curl_fetch_memory
#' @importFrom rjson fromJSON
xenocantoFetch <- function(query, page, per_page, key, handle, backoff=c(1,1,2,3,5,10,30,60)) {
  url <- paste0(
    "https://xeno-canto.org/api/3/recordings",
    "?query=", curl_escape(query),
    "&page=", page,
    "&per_page=", per_page,
    "&key=", curl_escape(key))

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
      numPages <- suppressWarnings(as.numeric(json[["numPages"]]))
      if (status == 200 && is.list(json[["recordings"]]) &&
          length(numPages) == 1 && !is.na(numPages)) {
        return(json)
      }
      #Errors are documented as {"error": {"code", "message"}}, but are also
      #returned as {"error": code, "message": message}
      reason <- if (is.list(json[["error"]])) json[["error"]][["message"]] else json[["message"]]
      if (length(reason) != 1) reason <- "unexpected response"
      problem <- paste0(reason, " (HTTP ", status, ")")
      #Other client errors, such as a bad query or key, will not succeed on retry
      if (status >= 400 && status < 500 && status != 429) break
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  stop(paste0("xeno-canto request for '", query, "' page ", page, " failed: ",
              gsub(key, "<key>", problem, fixed=TRUE)))
}

xenocantoRecordings <- function(recordings) {
  field <- function(name) {
    vapply(recordings, function(r) {
      value <- r[[name]]
      #Fields withheld for restricted species are listed in _meta
      if (name %in% names(r[["_meta"]][["redacted_fields"]]) || length(value) != 1 || is.na(value)) {
        return("")
      }
      if (is.numeric(value)) value <- format(value, scientific=FALSE, digits=15, trim=TRUE)
      value <- as.character(value)
      Encoding(value) <- "UTF-8"
      return(trimws(value))
    }, character(1), USE.NAMES=FALSE)
  }
  empty <- rep_len("", length(recordings))

  id <- field("id")
  grp <- field("grp")
  en <- field("en")
  type <- field("type")
  taxon <- xenocantoTaxon(field("gen"), field("sp"), field("ssp"), grp, field("status"))
  name <- ifelse(en != "" & taxon != "", paste0(en, " (", taxon, ")"), paste0(en, taxon))
  title <- trimws(paste0("XC", id, " ", name))
  title <- ifelse(type == "", title, paste(title, "-", type))

  data <- data.frame(
    source=empty,
    id=id,
    Title=title,
    taxon=taxon,
    file=sub("^//", "https://", field("file")),
    author=field("rec"),
    post_date=xenocantoDate(field("uploaded")),
    size=empty,
    size_raw=empty,
    type=xenocantoMime(field("file-name")),
    NonSpecimen=ifelse(grp == "soundscape", "Soundscape", ""),
    Date=xenocantoDate(field("date")),
    Time=xenocantoTime(field("time")),
    Duration=xenocantoDuration(field("length")),
    deployment=empty,
    lat=coordinate(field("lat"), 90),
    lon=coordinate(field("lon"), 180),
    time_of_day=xenocantoTimeOfDay(field("time")),
    license=xenocantoURL(field("lic")),
    info_url=xenocantoURL(field("url")),
    device=xenocantoDevice(field("dvc"), field("mic")),
    #The recordist holds the rights in a xeno-canto recording. Its country is
    #named rather than coded (e.g. Spain), so it is left out, and its channels
    #are not given.
    rights_holder=field("rec"),
    country=empty,
    locality=field("loc"),
    sample_rate=field("smp"),
    channels=empty,
    stringsAsFactors=FALSE)

  #Without audio (e.g. restricted species) there is nothing to listen to or analyse
  data <- data[data$id != "" & data$file != "", ]
  rownames(data) <- NULL
  return(data)
}

xenocantoTaxon <- function(gen, sp, ssp, grp, status) {
  taxon <- ifelse(gen != "" & sp != "", paste(gen, sp), "")
  #Subspecies are only added when given as a plain epithet
  trinomial <- taxon != "" & grepl("^[a-z]+(-[a-z]+)?$", ssp)
  taxon[trinomial] <- paste(taxon[trinomial], ssp[trinomial])
  #Soundscapes and unidentified recordings carry placeholder names, not taxa
  placeholder <- grp == "soundscape" | status == "unidentified" |
    tolower(paste(gen, sp)) %in% c("sonus naturalis", "mystery mystery")
  taxon[placeholder] <- ""
  return(taxon)
}

#xeno-canto uses 00 for an unknown month or day; these become partial dates
xenocantoDate <- function(x) {
  parts <- regmatches(x, regexec("^([0-9]{4})-(0[0-9]|1[0-2])-([0-2][0-9]|3[01])$", x))
  vapply(parts, function(p) {
    if (length(p) != 4 || p[2] == "0000") return("")
    if (p[3] == "00") return(p[2])
    if (p[4] == "00") return(paste(p[2], p[3], sep="-"))
    return(p[1])
  }, character(1), USE.NAMES=FALSE)
}

#Times are given as clock times, but some are words such as "morning" (see
#xenocantoTimeOfDay())
xenocantoTime <- function(x) {
  time <- clockTime(x)
  return(ifelse(is.na(time), "", time))
}

xenocantoTimeOfDay <- function(x) {
  return(ifelse(is.na(clockTime(x)) & !unknownTime(x), x, ""))
}

#The licence and the recording's page are URLs, some of which start //
xenocantoURL <- function(x) {
  url <- httpURL(x)
  return(ifelse(is.na(url), "", url))
}

#The recording device and the microphone, as far as they are known
xenocantoDevice <- function(dvc, mic) {
  return(ifelse(dvc != "" & mic != "", paste(dvc, mic, sep=", "), paste0(dvc, mic)))
}

#Lengths are m:ss or h:mm:ss, converted to seconds
xenocantoDuration <- function(x) {
  vapply(strsplit(x, ":", fixed=TRUE), function(p) {
    if (length(p) < 2 || length(p) > 3 || !all(grepl("^[0-9]+$", p))) return(NA_character_)
    return(sprintf("%.0f", sum(as.numeric(p) * 60^(rev(seq_along(p)) - 1))))
  }, character(1), USE.NAMES=FALSE)
}

xenocantoMime <- function(filename) {
  types <- c(mp3="audio/mpeg", wav="audio/x-wav", flac="audio/flac", ogg="audio/ogg", m4a="audio/mp4")
  extension <- ifelse(grepl("\\.[[:alnum:]]+$", filename), tolower(sub("^.*\\.", "", filename)), "")
  mime <- unname(types[extension])
  mime[is.na(mime)] <- ""
  return(mime)
}

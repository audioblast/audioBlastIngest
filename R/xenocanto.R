#' Harvest recordings from xeno-canto
#'
#' Harvests recording metadata from the xeno-canto API (version 3) and converts
#' it to the audioBlast! recordings format, with the details of each recording
#' that the recordings table has no column for.
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
#' @return Named list of the data frames a harvest gives: the recordings, and
#'   the details of them. Each has an empty source column (see sourceR()).
#' @examples
#' \dontrun{
#' harvest <- xenocantoR("grp:grasshoppers")
#' uploadRecordings(db, sourceR("xeno-canto", harvest$recordings))
#' uploadDetails(db, sourceR("xeno-canto", harvest$details))
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

  #Each page is converted as it arrives and the recordings it came from let
  #go of, as a harvest of every group is over a million recordings
  seen <- new.env(hash=TRUE, parent=emptyenv())
  pages <- list(recordings=list(), details=list())
  for (q in query) {
    page <- 1
    repeat {
      if (length(pages$recordings) > 0) Sys.sleep(pause)
      response <- xenocantoFetch(q, page, as.integer(per_page), key, handle)
      fresh <- xenocantoFresh(response$recordings, seen)
      for (type in names(pages)) {
        pages[[type]][[length(pages[[type]]) + 1]] <-
          if (type == "recordings") xenocantoRecordings(fresh) else xenocantoDetails(fresh)
      }
      if (verbose) print(paste0("  xeno-canto ", q, ": page ", page, " of ", response$numPages))
      if (page >= as.numeric(response$numPages)) break
      page <- page + 1
    }
  }

  data <- lapply(names(pages), function(type) xenocantoCombine(pages[[type]], type))
  names(data) <- names(pages)
  if (verbose) {
    for (type in names(data)) print(paste0("  xeno-canto ", type, ": ", nrow(data[[type]])))
  }
  return(data)
}

#Combines the tables of each page column by column, which is far quicker than
#rbind for many pages
xenocantoCombine <- function(pages, type) {
  headers <- names(getHeaders(type))
  data <- lapply(headers, function(h) {
    values <- unlist(lapply(pages, `[[`, h), use.names=FALSE)
    if (is.null(values)) character(0) else values
  })
  names(data) <- headers
  data <- as.data.frame(data, stringsAsFactors=FALSE)
  rownames(data) <- NULL
  return(data)
}

#The recordings of a page that have not been harvested already, remembering
#the ones that have. Results that change while paging can repeat a recording
#on two pages, which would otherwise be counted twice and give it two of each
#of its details.
xenocantoFresh <- function(recordings, seen) {
  fresh <- vapply(recordings, function(recording) {
    id <- xenocantoText(recording[["id"]])
    if (id == "" || !is.null(seen[[id]])) return(FALSE)
    assign(id, TRUE, envir=seen)
    return(TRUE)
  }, logical(1), USE.NAMES=FALSE)
  return(recordings[fresh])
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

#A field of a recording as text, as xeno-canto gives a number as a number in
#some recordings and as a string in others. Empty for a field it has no single
#value of, such as the sono object or the also array.
xenocantoText <- function(value) {
  if (length(value) != 1 || is.na(value)) return("")
  if (is.numeric(value)) value <- format(value, scientific=FALSE, digits=15, trim=TRUE)
  value <- as.character(value)
  Encoding(value) <- "UTF-8"
  return(trimws(value))
}

#A field of every recording of a page
xenocantoField <- function(recordings, name) {
  vapply(recordings, function(r) {
    #Fields withheld for restricted species are listed in _meta
    if (name %in% names(r[["_meta"]][["redacted_fields"]])) return("")
    return(xenocantoText(r[[name]]))
  }, character(1), USE.NAMES=FALSE)
}

xenocantoRecordings <- function(recordings) {
  field <- function(name) xenocantoField(recordings, name)
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
    #The recordist holds the rights in a xeno-canto recording, and its channels
    #are not given
    rights_holder=field("rec"),
    country=xenocantoCountry(field("cnt")),
    locality=field("loc"),
    sample_rate=field("smp"),
    channels=empty,
    stringsAsFactors=FALSE)

  #Without audio (e.g. restricted species) there is nothing to listen to or analyse
  data <- data[data$id != "" & data$file != "", ]
  rownames(data) <- NULL
  return(data)
}

#The details of recordings: what xeno-canto holds about one that the recordings
#table has no column for. Their names are xeno-canto's own, as each source's
#names for its details are, until they are matched to vocabulary terms.
#
#A value saying the recordist did not know is left out, as it says nothing
#about the recording: a sex or a life stage of "uncertain", an "unknown" for
#whether the animal was seen, playback was used or the recording was automatic,
#and a quality of "no score", which is a recording nobody has rated yet rather
#than a bad one. An altitude or a temperature that is not a number is left out
#as well, as xeno-canto writes "-" and "?" for some of them.
#
#Temperatures are taken to be degrees Celsius. xeno-canto does not say what it
#measures them in, but it gives them for grasshoppers, whose recordists work in
#Celsius, and a temperature with no unit is a number nothing can read.
xenocantoDetails <- function(recordings) {
  #Recordings without audio are left out of the recordings table (see
  #xenocantoRecordings()), so their details would belong to no record
  recordings <- recordings[xenocantoField(recordings, "id") != "" &
                             xenocantoField(recordings, "file") != ""]
  field <- function(name) xenocantoField(recordings, name)
  id <- field("id")
  detail <- function(name, value, unit="") xenocantoDetail(id, name, value, unit)

  return(rbind(
    detail("alt", decimalNumber(field("alt")), "m"),
    detail("temp", decimalNumber(field("temp")), "\u00b0C"),
    detail("q", xenocantoKnown(field("q"), "no score")),
    detail("method", field("method")),
    detail("sex", xenocantoKnown(field("sex"), "uncertain")),
    detail("stage", xenocantoKnown(field("stage"), "uncertain")),
    detail("auto", xenocantoKnown(field("auto"), "unknown")),
    detail("animal-seen", xenocantoKnown(field("animal-seen"), "unknown")),
    detail("playback-used", xenocantoKnown(field("playback-used"), "unknown")),
    detail("rmk", field("rmk")),
    #A registration number is the recordist's own, with nothing saying who holds
    #the specimen, so it is what xeno-canto recorded rather than a specimen
    detail("regnr", field("regnr"))))
}

#The detail of one name of each recording that has a value for it, in the
#columns of getHeaders("details")
xenocantoDetail <- function(id, name, value, unit="") {
  value[is.na(value)] <- ""
  has <- which(id != "" & value != "")
  return(data.frame(
    source=rep_len("", length(has)),
    type=rep_len("recordings", length(has)),
    id=id[has],
    name=rep_len(name, length(has)),
    delta=rep_len("0", length(has)),
    value=value[has],
    unit=rep_len(unit, length(has)),
    stringsAsFactors=FALSE))
}

#Values saying the recordist did not know, which are no detail of a recording
xenocantoKnown <- function(x, unknown) {
  return(ifelse(tolower(x) %in% unknown, "", x))
}

#xeno-canto names the country a recording was made in (Spain, Russian
#Federation) where the recordings table holds an ISO 3166-1 alpha-2 code, so
#the name is read as one. A name that is no country's is left out and said so.
xenocantoCountry <- function(cnt) {
  code <- countryOfName(cnt)
  warnUnread("xeno-canto recordings", "country", cnt, code)
  return(ifelse(is.na(code), "", code))
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

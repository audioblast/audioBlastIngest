#' Harvest recordings from the Tierstimmenarchiv
#'
#' Harvests recording metadata from the Animal Sound Archive of the Museum
#' fuer Naturkunde Berlin (Tierstimmenarchiv) and converts it to the
#' audioBlast! recordings format, with the details of each recording that the
#' recordings table has no column for, the taxa they name, the papers they
#' were used in, and the links saying which taxon each recording is of, which
#' are audible behind it and which paper cites it.
#'
#' Queries are the parameters of the archive's own search, written as they
#' would be typed and joined by &, e.g. `species=Anas acuta` or
#' `country=DE&from_year=2000`. The search takes species, author, collection,
#' country, state, administrative_area, locality, scenic_area, habitat,
#' sound_type, tape, unique_identifier, from_year, to_year, start_date,
#' end_date, sex, age, recording_type, sound_quality, usage_permission and
#' has_coords. Values are escaped here.
#'
#' The default query matches the whole archive. The search has no parameter
#' meaning everything, and a date range would leave out the recordings nobody
#' dated, so it asks instead for the identifiers with a colon in them, which is
#' all of them: an identifier is a collection and a name, e.g.
#' TSA:Anas_acuta_DIG_195_1_0. The collection is not always TSA, as the archive
#' also holds the recordings published with papers (e.g. J_Orn).
#'
#' Records are paged by dbid, the archive's own record number, rather than by
#' its default order, which is by date and moves as records are added.
#'
#' Tierstimmenarchiv recordings are not part of a deployment, so they are
#' located by lat and lon.
#'
#' @param query Character vector of searches, harvested in turn.
#' @param per_page Number of records a request asks for, up to 500.
#' @param pause Seconds to wait between requests. The archive publishes no
#'   rate limit, so requests are paced rather than pressed.
#' @param audio If FALSE, harvests the records with no audio to fetch as well,
#'   whose file is empty. About one record in eight has none.
#' @param verbose If TRUE says more about what's going on.
#' @param dir Directory to stream the harvest to, a CSV of each type of table,
#'   rather than holding it in memory (see xenocantoR()).
#' @return Named list of the data frames a harvest gives: the recordings, the
#'   details of them, the taxa they name, the references they were used in and
#'   the links. Each has an empty source column (see sourceR()). With dir, the
#'   paths they were streamed to instead.
#' @examples
#' \dontrun{
#' harvest <- tierstimmenarchivR("species=Pipistrellus")
#' uploadRecordings(db, sourceR("TSA", harvest$recordings))
#' uploadDetails(db, sourceR("TSA", harvest$details))
#' uploadTaxa(db, taxonomiseR(sourceR("TSA", harvest$taxa)))
#' uploadReferences(db, sourceR("TSA", harvest$references))
#' uploadLinks(db, sourceR("TSA", harvest$links))
#' }
#' @importFrom curl new_handle
#' @export
tierstimmenarchivR <- function(query="unique_identifier=:", per_page=500, pause=1,
                               audio=TRUE, verbose=FALSE, dir=NULL) {
  if (!is.character(query) || length(query) == 0 || any(is.na(query) | query == "")) {
    stop("query must be one or more Tierstimmenarchiv searches.")
  }
  if (!is.numeric(per_page) || length(per_page) != 1 || is.na(per_page) ||
      per_page < 1 || per_page > 500) {
    stop("per_page must be between 1 and 500.")
  }

  handle <- new_handle(
    useragent="audioBlastIngest (https://github.com/audioblast/audioBlastIngest)",
    connecttimeout=30,
    timeout=300)

  #The tables a harvest gives, and what makes each of them from a page
  make <- list(recordings=tsaRecordings, details=tsaDetails, taxa=tsaTaxa,
               references=tsaReferences, links=tsaLinks)

  #Each page is converted as it arrives and the records it came from let go of,
  #as xenocantoR() does, so that a harvest of the whole archive costs a page of
  #memory rather than all of it
  seen <- new.env(hash=TRUE, parent=emptyenv())
  #A taxon or a paper is one record however many recordings name it, so each is
  #written once rather than once for every page that names it
  once <- list(taxa=new.env(hash=TRUE, parent=emptyenv()),
               references=new.env(hash=TRUE, parent=emptyenv()))
  pages <- lapply(make, function(from) list())
  fetched <- 0
  for (q in query) {
    page <- 1
    repeat {
      if (fetched > 0) Sys.sleep(pause)
      records <- tsaFetch(q, page, as.integer(per_page), audio, handle)
      fetched <- fetched + 1
      fresh <- tsaFresh(records, seen)
      for (type in names(make)) {
        table <- make[[type]](tsaUsable(fresh, audio))
        if (type %in% names(once)) table <- tsaOnce(table, once[[type]])
        if (is.null(dir)) {
          pages[[type]][[length(pages[[type]]) + 1]] <- table
        } else {
          streamTable(dir, type, table)
        }
      }
      if (verbose) {
        print(paste0("  Tierstimmenarchiv ", q, ": page ", page, ", ",
                     length(fresh), " of ", length(records), " records new"))
      }
      #The search answers with the records themselves and no count of them, and
      #it pages past the end of a result set: asked for a page after the last
      #one it answers with the last page's records again, and goes on answering
      #with them for ever. So a harvest stops when a page brings nothing it has
      #not already seen, as well as when a page is not full.
      if (length(records) < per_page || length(fresh) == 0) break
      page <- page + 1
    }
  }

  if (!is.null(dir)) {
    paths <- lapply(names(make), function(type) streamPath(dir, type))
    names(paths) <- names(make)
    if (verbose) print(paste("  Tierstimmenarchiv harvested to", dir))
    return(paths)
  }

  data <- lapply(names(pages), function(type) tsaCombine(pages[[type]], type))
  names(data) <- names(pages)
  if (verbose) {
    for (type in names(data)) print(paste0("  Tierstimmenarchiv ", type, ": ", nrow(data[[type]])))
  }
  return(data)
}

#The archive's search, which serves both the pages and the JSON behind them
tsaSite <- "https://suche.tierstimmenarchiv.de/"

#Combines the tables of each page column by column, which is far quicker than
#rbind for many pages
tsaCombine <- function(pages, type) {
  headers <- names(getHeaders(type))
  data <- lapply(headers, function(h) {
    values <- unlist(lapply(pages, `[[`, h), use.names=FALSE)
    if (is.null(values)) character(0) else values
  })
  names(data) <- headers
  #Taxa have columns whose names have spaces in them, which are theirs to keep
  data <- as.data.frame(data, stringsAsFactors=FALSE, check.names=FALSE)
  rownames(data) <- NULL
  return(data)
}

#The rows of a page whose id has not been written already, remembering the ones
#that have, so that a taxon or a paper is one record however many recordings
#name it
tsaOnce <- function(table, written) {
  if (nrow(table) == 0) return(table)
  new <- vapply(table$id, function(id) {
    if (!is.null(written[[id]])) return(FALSE)
    assign(id, TRUE, envir=written)
    return(TRUE)
  }, logical(1), USE.NAMES=FALSE)
  table <- table[new, , drop=FALSE]
  rownames(table) <- NULL
  return(table)
}

#The records of a page that have not been harvested already, remembering the
#ones that have. This is what tells a harvest it has reached the end, as the
#archive goes on answering with the last page for ever, and it also keeps a
#record that repeats on two pages from being counted twice and given two of
#each of its details.
tsaFresh <- function(records, seen) {
  fresh <- vapply(records, function(record) {
    id <- tsaText(record[["unique_identifier"]])
    if (id == "" || !is.null(seen[[id]])) return(FALSE)
    assign(id, TRUE, envir=seen)
    return(TRUE)
  }, logical(1), USE.NAMES=FALSE)
  return(records[fresh])
}

#The records a harvest keeps. A record whose filename is empty has nothing to
#fetch: the archive holds the card for it but not the audio. With audio, which
#is the default, those are left out, as there is nothing in them to listen to
#or analyse.
tsaUsable <- function(records, audio) {
  if (!audio) return(records)
  return(records[tsaField(records, "filename") != ""])
}

#' @importFrom curl curl_escape curl_fetch_memory
#' @importFrom rjson fromJSON
tsaFetch <- function(query, page, per_page, audio, handle, backoff=c(1,1,2,3,5,10,30,60)) {
  url <- paste0(
    tsaSite, "search/query.json?", tsaParameters(query),
    if (audio) "" else "&show_not_downloadable=1",
    "&order_by=dbid&sort=asc",
    "&page=", page,
    "&results_per_page=", per_page)

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
      status <- response$status_code
      #A page of records is a JSON array, which has no names; a search that was
      #not understood answers with an object saying so, e.g. {"Warning": "no
      #search parameters found"}, and will not succeed on retry
      if (status == 200 && is.list(json) && is.null(names(json))) {
        return(json)
      }
      reason <- if (is.list(json)) paste(names(json), unlist(json), collapse="; ") else ""
      if (length(reason) != 1 || reason == "") reason <- "unexpected response"
      problem <- paste0(reason, " (HTTP ", status, ")")
      if (status == 200 || (status >= 400 && status < 500 && status != 429)) break
    }
    if (is.na(wait)) break
    Sys.sleep(wait)
  }
  stop(paste0("Tierstimmenarchiv request for '", query, "' page ", page, " failed: ", problem))
}

#A search as a query string, with its values escaped, so that a search can be
#written as it would be typed: "species=Anas acuta" rather than
#"species=Anas%20acuta"
#' @importFrom curl curl_escape
tsaParameters <- function(query) {
  pairs <- trimws(strsplit(query, "&", fixed=TRUE)[[1]])
  pairs <- pairs[pairs != ""]
  if (length(pairs) == 0) stop("A Tierstimmenarchiv search must have a parameter in it.")
  parameters <- vapply(pairs, function(pair) {
    name <- sub("=.*$", "", pair)
    value <- if (grepl("=", pair, fixed=TRUE)) sub("^[^=]*=", "", pair) else ""
    return(paste0(curl_escape(trimws(name)), "=", curl_escape(value)))
  }, character(1), USE.NAMES=FALSE)
  return(paste(parameters, collapse="&"))
}

#A field of a record as text, as the archive gives a number as a number (a
#coordinate, a sample rate, whether the animal was seen) and leaves out what it
#has no value for as null
tsaText <- function(value) {
  if (length(value) != 1 || is.na(value)) return("")
  if (is.numeric(value)) value <- format(value, scientific=FALSE, digits=15, trim=TRUE)
  value <- as.character(value)
  Encoding(value) <- "UTF-8"
  return(trimws(value))
}

#A field of every record of a page
tsaField <- function(records, name) {
  vapply(records, function(r) tsaText(r[[name]]), character(1), USE.NAMES=FALSE)
}

#One of two values for each record, as text. ifelse() gives logical(0) rather
#than character(0) where there are no records at all, which would make a column
#of a page with nothing on it the wrong type.
tsaEither <- function(test, yes, no) {
  return(as.character(ifelse(test, yes, no)))
}

tsaRecordings <- function(records) {
  field <- function(name) tsaField(records, name)
  empty <- rep_len("", length(records))

  id <- field("unique_identifier")
  taxon <- tsaTaxon(field("species"), field("subspecies"))
  file <- tsaFile(id, field("filename"))

  data <- data.frame(
    source=empty,
    id=id,
    Title=tsaTitle(id, tsaWritten(field("species"), field("subspecies")),
                   field("sound_type")),
    taxon=taxon,
    file=file,
    author=field("author"),
    post_date=tsaDate(field("submission")),
    #The size is only in the headers of the audio itself, so reading it would
    #cost a request for every record in the archive
    size=empty,
    size_raw=empty,
    type=tsaEither(file == "", "", "audio/x-wav"),
    NonSpecimen=empty,
    Date=tsaDate(field("recording_date")),
    Time=tsaTime(field("recording_time")),
    Duration=tsaDuration(field("duration"), field("durationinsec")),
    deployment=empty,
    lat=tsaCoordinate(field("latitude"), 90),
    lon=tsaCoordinate(field("longitude"), 180),
    #Times are clock times, so none of them describe a time of day in words
    time_of_day=empty,
    license=tsaLicense(field("usage_permission")),
    info_url=tsaRecordURL(id),
    device=field("recording_equipment"),
    #The recordist is who a CC BY licence asks to be credited, and the archive
    #names no other rights holder
    rights_holder=field("author"),
    country=field("country"),
    locality=field("locality"),
    sample_rate=tsaSampleRate(field("sample_rate")),
    #Channels are not given. They could be worked out from the size of the
    #audio, its length and its sample rate, but that would be a guess at what
    #the archive does not say, and a request for every record to make it.
    channels=empty,
    stringsAsFactors=FALSE)

  rownames(data) <- NULL
  return(data)
}

#The details of recordings: what the archive holds about one that the
#recordings table has no column for. Their names are the archive's own, as each
#source's names for its details are, until they are matched to vocabulary
#terms.
#
#The place a recording was made is partly here. Its country, locality and
#coordinates are columns of the recordings table, but the archive also names a
#state, a district, a scenic area and a habitat, and it gives its places no
#identifiers of their own, so making locations records of them would mean
#inventing ids for places the archive does not hold as records. NOTE: if these
#are to become locations, the ids have to be settled first.
tsaDetails <- function(records) {
  field <- function(name) tsaField(records, name)
  id <- field("unique_identifier")
  detail <- function(name, value, unit="") tsaDetail(id, name, value, unit)
  sounds <- tsaSplit(field("sound_type"))

  return(rbind(
    #What was recorded, and of what animal. A record can name more than one
    #sound type, e.g. "social call, echolocation", and each is a detail of its
    #own; uploadDetails() numbers them by delta.
    tsaDetail(rep(id, lengths(sounds)), "sound_type",
              unlist(sounds, use.names=FALSE)),
    detail("sex", field("sex")),
    detail("age", field("age")),
    detail("visual_identification", field("visual_identification")),
    #The archive's label for the individual recorded is sometimes a collection's
    #number (ZFMK 89560) and more often the recordist's own (2, 3, Bird05,
    #ID107), with nothing saying which it is or who holds anything, so it is
    #kept as the archive wrote it rather than made a specimens record
    detail("specimen", field("specimen")),

    #Where, and in what conditions
    detail("habitat", field("habitat")),
    detail("scenic_area", field("scenic_area")),
    detail("administrative_area", field("administrative_area")),
    detail("state", field("state")),
    detail("altitude", decimalNumber(field("altitude")), "m"),
    #The weather is a sentence of its own making, e.g. "calm, 14 C , RH 99%",
    #which is kept as it is rather than read for a temperature and a humidity
    detail("weather", field("weather")),

    #How it was made, and where it sits in the collection
    detail("recording_type", field("recording_type")),
    detail("sound_quality", field("sound_quality")),
    detail("bit_depth", wholeNumber(field("bit_depth")), "bit"),
    detail("collection", field("collection")),
    detail("tape_identifier", field("tape_identifier")),
    detail("tape_number", field("tape_number")),
    detail("tape_speed", field("tape_speed")),
    detail("track", field("track")),
    detail("recording_number", field("recording_number")),
    detail("start_position", field("start_position")),
    detail("end_position", field("end_position")),
    detail("filename", field("filename")),
    detail("alternate_filename", field("alternate_filename")),

    #What the archive says about a recording in prose: in German for every
    #record, and in English for most of them. The descriptions table has no
    #column for the language a description is in, so they are kept here, where
    #their names say which is which, rather than uploaded as two descriptions
    #that nothing tells apart. NOTE: a language column on descriptions, as
    #vernacularnames has, would move them there.
    detail("description", field("description")),
    detail("description_en", field("description_en")),

    #What is left of a note once the paper it cites has been read out of it
    detail("notes", tsaRemarks(field("notes")))))
}

#The detail of one name of each record that has a value for it, in the columns
#of getHeaders("details")
tsaDetail <- function(id, name, value, unit="") {
  #A record can have no value of a name at all, which unlist() gives as NULL,
  #and a NULL column would be dropped from the data frame rather than empty
  id <- as.character(id)
  value <- as.character(value)
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

#The taxa a page of records names, so that the links to them reach a record
#rather than dangling. A taxon is identified by its name, as it is for
#xeno-canto: the archive browses a classification of its own, but it gives that
#only as a tree of its own pages, not with the records.
#
#A name implies the taxa it sits in, so a species reaches its genus whether or
#not anything was recorded of the genus alone.
tsaTaxa <- function(records) {
  named <- c(tsaTaxon(tsaField(records, "species"), tsaField(records, "subspecies")),
             unlist(tsaBackground(tsaField(records, "background_species")), use.names=FALSE))
  named <- unique(named[!is.na(named) & named != ""])

  taxa <- unique(unlist(lapply(strsplit(named, " ", fixed=TRUE), function(parts) {
    vapply(seq_along(parts), function(n) paste(parts[seq_len(n)], collapse=" "),
           character(1))
  }), use.names=FALSE))
  if (is.null(taxa)) taxa <- character(0)

  words <- lengths(strsplit(taxa, " ", fixed=TRUE))
  parent <- tsaEither(words > 1, sub(" [^ ]+$", "", taxa), "")
  empty <- rep_len("", length(taxa))
  data <- data.frame(empty, taxa, taxa, empty, empty, empty, empty,
                     c("Genus", "Species", "Subspecies")[words], parent, parent,
                     stringsAsFactors=FALSE)
  names(data) <- names(getHeaders("taxa"))
  return(data)
}

#The papers the archive says a recording was used in, as references. A note
#that does not read as a citation gives none, and stays a note (see
#tsaRemarks()).
tsaReferences <- function(records) {
  citations <- tsaCitations(tsaField(records, "notes"))
  cited <- citations[!is.na(citations$id), , drop=FALSE]
  cited <- cited[!duplicated(cited$id), , drop=FALSE]

  columns <- names(getHeaders("references"))
  data <- as.data.frame(
    matrix("", nrow=nrow(cited), ncol=length(columns), dimnames=list(NULL, columns)),
    stringsAsFactors=FALSE)
  #Every citation the archive gives is of a paper
  data$type <- rep_len("article", nrow(cited))
  for (column in intersect(columns, names(cited))) {
    data[[column]] <- cited[[column]]
  }
  rownames(data) <- NULL
  return(data)
}

#What a recording is about: the taxon it is of, and the ones audible behind it,
#which the archive names in background_species. A recording is about a
#background species as it is about its own taxon, so the relationship is the
#same one, IAO is about; what tells them apart is the qualifier on the
#background ones, as it does for xeno-canto.
#
#A paper that used a recording references it, which is the relationship
#dcterms:isReferencedBy gives.
#
#Sound types are not linked to the vocabulary. Its Type of Call terms were
#written for insects, and the archive is three quarters birds: of its sound
#types only alarm call and courtship call have terms (Defensive Call and
#Courtship Song), so linking would mark a handful and leave call, song, flight
#call, begging call and echolocation looking unclassified. NOTE: worth
#revisiting once cv/callType has terms for them.
tsaLinks <- function(records) {
  id <- tsaField(records, "unique_identifier")
  focal <- tsaTaxon(tsaField(records, "species"), tsaField(records, "subspecies"))
  background <- tsaBackground(tsaField(records, "background_species"))
  cited <- tsaCitations(tsaField(records, "notes"))$id

  return(rbind(
    tsaLink("recordings", id[focal != ""], "http://purl.obolibrary.org/obo/IAO_0000136",
            "taxa", focal[focal != ""]),
    tsaLink("recordings", rep(id, lengths(background)),
            "http://purl.obolibrary.org/obo/IAO_0000136",
            "taxa", unlist(background, use.names=FALSE),
            "https://vocab.audioblast.org/cv/recordingContent#NonFocalTaxa"),
    tsaLink("recordings", id[!is.na(cited)], "http://purl.org/dc/terms/isReferencedBy",
            "references", cited[!is.na(cited)])))
}

#Links from one record to another, in the columns of getHeaders("links")
tsaLink <- function(subjectType, subject, predicate, objectType, object, qualifier="") {
  if (is.null(object)) object <- character(0)
  column <- function(value) rep_len(value, length(object))
  return(data.frame(
    source=column(""),
    subject_type=column(subjectType),
    subject_source=column(""),
    subject_id=subject,
    predicate=column(predicate),
    object_type=column(objectType),
    object_source=column(""),
    object_id=object,
    qualifier=column(qualifier),
    remarks=column(""),
    reference=column(""),
    stringsAsFactors=FALSE))
}

#Scientific names as the archive writes them: a capitalised genus and one or
#two lower case epithets. A name left open at the species, e.g. "Acrocephalus
#spec.", is the genus, which is as far as the recordist identified the animal;
#anything else that is not a name, such as the "div." of a recording of several
#species, names no taxon here, so it is left out rather than made one.
tsaName <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  open <- grepl("^[A-Z][a-z]+ (spec|sp)\\.?$", x)
  x[open] <- sub(" .*$", "", x[open])
  x[!grepl("^[A-Z][a-z]+( [a-z-]+){0,2}$", x)] <- ""
  return(x)
}

#The name of the taxon each recording is of. A subspecies is given as a bare
#epithet, so it is added to the species it belongs to.
tsaTaxon <- function(species, subspecies) {
  taxon <- tsaName(species)
  trinomial <- taxon != "" & grepl(" ", taxon, fixed=TRUE) &
    grepl("^[a-z]+(-[a-z]+)?$", trimws(subspecies))
  taxon[trinomial] <- paste(taxon[trinomial], trimws(subspecies[trinomial]))
  return(taxon)
}

#The names of the taxa audible behind each recording, as a list of one
#character vector per recording. The archive writes them as free text, so a
#record can name more than one, and many name none: "birds" is the commonest
#value and is no taxon.
tsaBackground <- function(background) {
  return(lapply(tsaSplit(background), function(names) {
    names <- tsaName(names)
    return(unique(names[names != ""]))
  }))
}

#Values that a record can hold more than one of, which the archive separates
#with commas, as a list of one character vector per record
tsaSplit <- function(x) {
  return(lapply(strsplit(x, "[,;]"), function(values) {
    values <- trimws(values)
    return(values[values != ""])
  }))
}

#The archive gives a recording no title of its own, so one is made of what was
#recorded and what it was doing, e.g. "Anas acuta - courtship call"
tsaTitle <- function(id, written, sound_type) {
  name <- tsaEither(written != "", written, id)
  return(trimws(tsaEither(sound_type != "", paste(name, "-", sound_type), name)))
}

#What the archive says was recorded, as it wrote it: the species with the
#subspecies after it. A recording is titled by this rather than by its taxon,
#so that a name left open at the species keeps its "spec." for a reader where
#the taxon it is linked to is the genus (see tsaName()).
tsaWritten <- function(species, subspecies) {
  subspecies <- trimws(subspecies)
  named <- grepl("^[a-z]+(-[a-z]+)?$", subspecies)
  return(trimws(tsaEither(named, paste(species, subspecies), species)))
}

#The audio of a recording: the master the archive serves as WAV. The short MP3
#at download_short.mp3 is a clip made from it, and is not there for every
#record, so it is not what a recording is.
#' @importFrom curl curl_escape
tsaFile <- function(id, filename) {
  return(tsaEither(id == "" | filename == "", "",
                   paste0(tsaSite, "download.wav?unique_identifier=", curl_escape(id))))
}

#The archive's page for a recording
#' @importFrom curl curl_escape
tsaRecordURL <- function(id) {
  return(tsaEither(id == "", "",
                   paste0(tsaSite, "search/details.html?unique_identifier=", curl_escape(id))))
}

#Dates, which the archive writes as YYYY-MM-DD, and the moment a record was
#submitted, which it writes as a timestamp; empty where there is no date
tsaDate <- function(x) {
  date <- isoDate(x)
  return(tsaEither(is.na(date), "", date))
}

tsaTime <- function(x) {
  time <- clockTime(x)
  return(tsaEither(is.na(time), "", time))
}

#Coordinates, which the archive gives as decimal degrees
tsaCoordinate <- function(x, limit) {
  value <- coordinate(x, limit)
  return(tsaEither(is.na(value), "", value))
}

#How long a recording is, in seconds. The archive gives a length as HH:MM:SS,
#and for a few records in seconds as well; both are 0 where nobody measured
#one, which is no length rather than a recording of no length.
tsaDuration <- function(duration, seconds) {
  clock <- vapply(strsplit(duration, ":", fixed=TRUE), function(parts) {
    if (length(parts) != 3 || !all(grepl("^[0-9]+$", parts))) return(NA_character_)
    return(sprintf("%.0f", sum(as.numeric(parts) * c(3600, 60, 1))))
  }, character(1), USE.NAMES=FALSE)

  measured <- positiveNumber(seconds)
  value <- positiveNumber(tsaEither(is.na(measured), clock, measured))
  return(tsaEither(is.na(value), "", value))
}

#Sample rates as samples a second, which is what the recordings table holds.
#About a third of the records that give one give it in kHz instead: of 668
#rates in a sample of 1,038 records, 216 were 48, 96, 192, 256 or 384 and the
#rest 22050 and above. Nothing records audio at 384 samples a second, and no
#recording here is below 22050, so a rate under 1000 is read as kHz. NOTE: this
#is the archive's own inconsistency, not a reading of it, and it is worth
#telling them about.
tsaSampleRate <- function(x) {
  rate <- suppressWarnings(as.numeric(wholeNumber(x)))
  rate[!is.na(rate) & rate < 1000] <- rate[!is.na(rate) & rate < 1000] * 1000
  return(tsaEither(is.na(rate), "", format(rate, scientific=FALSE, trim=TRUE)))
}

#The licence a recording is under, as an address. The archive names a licence
#rather than addressing one, and never says which version, so the version the
#museum itself publishes for these recordings to GBIF is used: it gives them as
#http://creativecommons.org/licenses/by-nc-sa/4.0/ there.
#
#One licence is named two ways, "CC BY-NC-SA" and "CC BY-NC-SA, no commercial
#use", so what follows the first comma is a restatement of the licence rather
#than another condition.
tsaLicense <- function(x) {
  licenses <- c(
    "cc by"="by", "cc by-sa"="by-sa", "cc by-nd"="by-nd", "cc by-nc"="by-nc",
    "cc by-nc-sa"="by-nc-sa", "cc by-nc-nd"="by-nc-nd")
  named <- gsub("\\s+", " ", tolower(trimws(sub(",.*$", "", x))))
  path <- unname(licenses[named])
  url <- tsaEither(is.na(path), NA_character_,
                   paste0("https://creativecommons.org/licenses/", path, "/4.0/"))
  warnUnread("Tierstimmenarchiv recordings", "licence", x, url)
  return(tsaEither(is.na(url), "", url))
}

#The archive says which paper a recording was used in inside its notes, e.g.
#"Used in: Frommolt K-H, Schmidt EW (2026) Social trills of the pipistrelle
#bat. J Acoust Soc Am 160(2):1343-1349. https://doi.org/10.1121/10.0044584",
#sometimes after a note of its own ("sv; Used in: ...").
#
#A citation is read as authors, a year in brackets, a title, the journal with
#its volume, issue and pages where it has them, and a DOI. It is read strictly:
#a note that does not have all of that is not a citation, so it gives no
#reference and stays a note. The field a title ends at is the last full stop
#before the journal, as a title can have full stops in it ("L. macrotis") and a
#journal does not.
#
#The paper is identified by its DOI, which every citation in the archive gives,
#and otherwise by the citation itself, so that one paper is one reference
#however many recordings it used.
#' @importFrom digest digest
tsaCitations <- function(notes) {
  columns <- c("id", "title", "author", "year", "journal", "volume", "number",
               "pages", "doi", "url")
  citations <- as.data.frame(
    matrix(NA_character_, nrow=length(notes), ncol=length(columns),
           dimnames=list(NULL, columns)),
    stringsAsFactors=FALSE)

  cited <- regmatches(notes, regexec("Used in:\\s*(.+)$", notes))
  for (i in which(lengths(cited) == 2)) {
    citation <- trimws(cited[[i]][2])
    parts <- regmatches(citation, regexec(
      "^(.+?) \\(([0-9]{4})\\) (.+?)\\.? (https?://\\S+?)\\.?$", citation))[[1]]
    if (length(parts) != 5) next
    published <- tsaJournal(parts[4])
    if (is.null(published)) next

    citations$title[i] <- published$title
    citations$author[i] <- tsaAuthors(parts[2])
    citations$year[i] <- parts[3]
    citations$journal[i] <- published$journal
    citations$volume[i] <- published$volume
    citations$number[i] <- published$number
    citations$pages[i] <- published$pages

    #Every citation the archive gives addresses a DOI, but one that addressed
    #something else would be the paper's address rather than its DOI, and would
    #have to be identified by the citation itself
    doi <- sub("^https?://(dx\\.)?doi\\.org/", "", parts[5], ignore.case=TRUE)
    citations$doi[i] <- if (doi != parts[5]) doi else ""
    citations$url[i] <- if (doi != parts[5]) "" else parts[5]
    citations$id[i] <- if (doi != parts[5]) doi else
      digest(citation, algo="sha1", serialize=FALSE)
  }
  return(citations)
}

#The title and where it was published, read from the end of a citation: a
#journal with a volume, an issue and pages, or a journal on its own. NULL where
#it reads as neither.
tsaJournal <- function(citation) {
  parts <- regmatches(citation, regexec(
    "^(.*)\\. ([^.]+?) ([0-9]+)\\(([^()]+)\\):([0-9]+[-\u2013][0-9]+)$", citation))[[1]]
  if (length(parts) == 6) {
    return(list(title=parts[2], journal=parts[3], volume=parts[4], number=parts[5],
                pages=gsub("\u2013", "-", parts[6])))
  }
  parts <- regmatches(citation, regexec("^(.*)\\. ([^.]+)$", citation))[[1]]
  if (length(parts) == 3) {
    return(list(title=parts[2], journal=parts[3], volume="", number="", pages=""))
  }
  return(NULL)
}

#The authors of a citation, which the archive writes surname first with the
#initials after it and no comma ("Frommolt K-H, Schmidt EW"), as the references
#table holds them: "Frommolt, K-H; Schmidt, EW". A name that is not written
#that way is kept as it is.
tsaAuthors <- function(authors) {
  names <- trimws(strsplit(authors, ",", fixed=TRUE)[[1]])
  names <- names[names != ""]
  return(paste(sub("^(.+?) ([A-Z]{1,3}(-[A-Z])*)$", "\\1, \\2", names), collapse="; "))
}

#What is left of a note once the paper it cites has been read out of it, e.g.
#"sv" of "sv; Used in: ...". A note whose citation could not be read is kept
#whole, so that nothing the archive says is lost when a citation is written a
#way this does not know.
tsaRemarks <- function(notes) {
  read <- !is.na(tsaCitations(notes)$id)
  return(tsaEither(read, trimws(sub("[;,]?\\s*Used in:.*$", "", notes)), notes))
}

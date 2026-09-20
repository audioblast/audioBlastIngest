#The fixtures are responses of the public Orthoptera Species File API,
#recorded on 2026-09-20: a page of sounds, and the sounds conveyed on a
#specimen, on field observations and on collecting events, with the Darwin
#Core records, OTUs and audio headers they lead to.
osfJSON <- function(name) {
  path <- test_path("fixtures", name)
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  rjson::fromJSON(body)
}

osfFixture <- function() osfJSON("orthoptera-page.json")
osfLinked <- function() osfJSON("orthoptera-linked.json")

#Replays the recorded responses, keeping the paths asked for
osfReplay <- function(fixture) {
  paths <- character()
  fetch <- function(path) {
    paths <<- c(paths, path)
    response <- fixture$responses[[path]]
    if (is.null(response)) stop("Unexpected request: ", path)
    response
  }
  list(fetch=fetch, paths=function() paths, forget=function() paths <<- character())
}

osfLookup <- function(fetch) {
  function(id) orthopteraValue(fetch(paste0("otus/", id, "?extend[]=taxon_name"))$data$taxon_name$cached)
}

osfFileInfo <- function(fixture) {
  info <- list()
  for (sound in fixture$sounds) info[[sound$sound_file]] <- fixture$files[[as.character(sound$id)]]
  function(url) {
    found <- info[[url]]
    if (is.null(found)) return(list(type="", size=""))
    return(list(type=found$type, size=found$size))
  }
}

test_that("linked records give a recording its taxon, place and date", {
  fixture <- osfLinked()
  replay <- osfReplay(fixture)
  lookup <- osfLookup(replay$fetch)
  data <- orthopteraRecordings(fixture$sounds, lookup,
                               orthopteraOccurrences(replay$fetch, lookup),
                               osfFileInfo(fixture))

  expect_identical(data$id, c("55", "125", "126", "199", "208"))
  #The specimen, the two field observations and the one event whose
  #occurrences agree; the last recording is not identified by the source
  expect_identical(data$taxon, c("Heteropternis couloniana",
                                 "Chorthippus (Glyptobothrus) eisentrauti",
                                 "Chorthippus (Glyptobothrus) eisentrauti",
                                 "Chorthippus (Altichorthippus) intermedius", ""))
  expect_identical(data$Date, c("1994-07-22", "1991-09-03", "1991-09-04", "1997-06-17", ""))
  expect_identical(data$country, c("TZ", "IT", "IT", "MN", ""))
  expect_identical(data$lat[1], "-5.100342")
  expect_identical(data$locality[1], "East Usambara Mountains, Amani")
  expect_identical(data$locality[4], "T\u00f6v Aimag, Ulan Baatar, hill at Zaisan monument")
  #The specimen has no attribution, so its Darwin Core recorder is the author
  expect_identical(data$author, c("Axel Hochkirch", "Sigfrid Ingrisch", "Sigfrid Ingrisch",
                                  "Klaus-Gerhard Heller", "Holger Braun"))
  expect_identical(data$type, c("audio/x-wav", "audio/mpeg", "audio/mpeg", "audio/mpeg", "audio/x-wav"))
  expect_identical(data$size_raw[1], "7716908")
  expect_identical(data$sample_rate, c("44100", "48000", "48000", "44100", "200000"))
  #OSF has no page for a sound, so a recording has no info_url
  expect_true(all(data$info_url == ""))
})

test_that("a name read from a title is a qualified link, not the recording's taxon", {
  fixture <- osfLinked()
  replay <- osfReplay(fixture)
  lookup <- osfLookup(replay$fetch)
  data <- orthopteraRecordings(fixture$sounds, lookup,
                               orthopteraOccurrences(replay$fetch, lookup),
                               osfFileInfo(fixture))
  links <- attr(data, "links")

  #The collecting event of sound 208 has no occurrences at all, so the source
  #identifies nothing: only its title names Parasubria vittipes
  expect_identical(data$taxon[data$id == "208"], "")
  expect_identical(names(links), names(getHeaders("links")))
  expect_equal(nrow(links), 1)
  expect_identical(links$subject_type, "recordings")
  expect_identical(links$subject_id, "208")
  expect_identical(links$predicate, "http://purl.obolibrary.org/obo/IAO_0000136")
  expect_identical(links$object_type, "iri")
  expect_identical(links$object_id, "https://orthoptera.speciesfile.org/otus/842419")
  expect_identical(links$qualifier,
                   "https://vocab.audioblast.org/cv/identificationBasis#RecordingTitle")
  expect_match(links$remarks, "does not identify this recording", fixed=TRUE)
  #uploadLinks() must accept it, with the source filled in as ingestR() does
  links$source <- "orthoptera-speciesfile"
  expect_equal(nrow(normaliseLinks(links)), 1)
})

test_that("Darwin Core records and OTUs are each read once", {
  fixture <- osfLinked()
  replay <- osfReplay(fixture)
  occurrences <- orthopteraOccurrences(replay$fetch, osfLookup(replay$fetch))
  invisible(lapply(fixture$sounds, occurrences))
  replay$forget()
  invisible(lapply(fixture$sounds, occurrences))
  #The second pass reads nothing: every record, event and title is cached
  expect_length(replay$paths(), 0)
})

test_that("events page through both occurrence types and reject mixed taxa", {
  sound <- list(name="field recording", conveyances=list(list(
    conveyance_object_type="CollectingEvent", conveyance_object_id=10)))
  paths <- character()
  fetch <- function(path) {
    paths <<- c(paths, path)
    if (grepl("collection_object_query", path)) return(list(data=list(), total_pages="0"))
    list(data=list(list(otu_id=if (grepl("page=1$", path)) 1 else 2)), total_pages="2")
  }
  resolve <- orthopteraOccurrences(fetch, function(id) paste("Taxon", id))
  found <- resolve(sound)
  expect_identical(found$taxa, character())
  expect_identical(found$inferred, "")
  #Both occurrence types are asked for, and the second page of the second
  expect_length(paths, 3)
  expect_match(paths[3], "page=2$")

  #Missing identifications must not make an event appear unambiguous
  resolve <- orthopteraOccurrences(function(path) list(
    data=list(list(otu_id=1), list()), total_pages="1"), function(id) "Taxon")
  expect_identical(resolve(sound)$taxa, character())
})

test_that("a title is only read where an indirect link gave no taxon", {
  sound <- list(name="Parasubria vittipes 4 field recording", conveyances=list(list(
    conveyance_object_type="CollectingEvent", conveyance_object_id=10)))
  candidates <- list(list(id=1, taxon_name=list(cached="Parasubria vittipes", cached_is_valid=TRUE)))
  fetch <- function(path) list(data=if (startsWith(path, "otus?")) candidates else list(), total_pages="1")
  resolve <- function() orthopteraOccurrences(fetch, function(id) "unused")(sound)$inferred

  expect_identical(resolve(), "1")
  candidates[[1]]$taxon_name$cached_is_valid <- FALSE
  expect_identical(resolve(), "")
  candidates[[1]]$taxon_name$cached_is_valid <- TRUE
  candidates[[1]]$taxon_name$cached <- "Different species"
  expect_identical(resolve(), "")
  candidates[[1]]$taxon_name$cached <- "Parasubria vittipes"
  candidates <- c(candidates, candidates)
  expect_identical(resolve(), "")
  candidates <- candidates[1]
  sound$name <- "Parasubria vittipes subspecies 4 field recording"
  expect_identical(resolve(), "")

  #A recording conveyed on a taxon or a specimen is never read from its title
  sound$name <- "Parasubria vittipes 4 field recording"
  sound$conveyances <- list(list(conveyance_object_type="Otu", conveyance_object_id=7))
  expect_identical(resolve(), "")
})

test_that("indirect lookups fail on incomplete pagination", {
  expect_error(orthopteraPages("dwc_occurrences?x=1", function(path)
    list(data=list(), total_pages="2")), "empty.*lookup page")
  expect_error(orthopteraPages("dwc_occurrences?x=1", function(path)
    list(data=list(), total_pages=NULL)), "pagination")
})

test_that("OSF maps live sound fields without inventing recording metadata", {
  fixture <- osfFixture()
  data <- orthopteraRecordings(fixture, function(id) {
    c(`804734`="Aglaothorax segnis", `810653`="Stethophyma grossum")[[id]]
  })
  expect_identical(names(data), names(getHeaders("recordings")))
  expect_true(all(vapply(data, is.character, logical(1))))
  expect_identical(data$id, c("44", "62"))
  expect_identical(data$taxon, c("Aglaothorax segnis", "Stethophyma grossum"))
  expect_identical(data$file[1], "https://sfg.taxonworks.org/s/klud2b")
  expect_identical(data$post_date[1], "2025-08-27")
  expect_identical(data$Duration[1], "10.014")
  expect_identical(data$sample_rate, c("44100", "96000"))
  #The copyright notice is not part of the recordist's name
  expect_identical(data$author[1], "Jeffrey A. Cole")
  expect_identical(data$rights_holder[1], "Jeffrey A. Cole")
  expect_identical(data$author[2], "Sigfrid Ingrisch")
  expect_identical(data$rights_holder[2], "")
  #The API gives no time, place, licence or device for a sound of a taxon
  expect_true(all(data[c("Date", "Time", "lat", "lon", "country", "locality",
                         "license", "device", "channels", "info_url")] == ""))
  expect_equal(nrow(attr(data, "links")), 0)
})

test_that("empty, unavailable and multiply linked sounds are handled", {
  empty <- orthopteraRecordings(list(), function(id) stop("unexpected lookup"))
  expect_equal(nrow(empty), 0)
  expect_identical(names(empty), names(getHeaders("recordings")))
  expect_equal(nrow(attr(empty, "links")), 0)

  sounds <- osfFixture()
  sounds[[1]]$sound_file <- NULL
  sounds[[2]]$metadata$error <- "Missing sound file"
  expect_equal(nrow(orthopteraRecordings(sounds, function(id) stop("unexpected lookup"))), 0)

  #A scientific name is one name, so a recording of two taxa says what it is
  #about as a link to each of them instead
  sound <- osfFixture()[1]
  sound[[1]]$conveyances <- c(sound[[1]]$conveyances,
    list(list(conveyance_object_type="Otu", conveyance_object_id=123)))
  data <- orthopteraRecordings(sound, function(id) paste("Taxon", id))
  expect_identical(data$taxon, "")
  links <- attr(data, "links")
  expect_identical(links$object_id, c("https://orthoptera.speciesfile.org/otus/804734",
                                      "https://orthoptera.speciesfile.org/otus/123"))
  expect_true(all(links$qualifier == ""))

  sound[[1]]$conveyances <- NULL
  expect_identical(orthopteraRecordings(sound, function(id) stop("lookup"))$taxon, "")
})

test_that("paging deduplicates sounds, caches OTUs and keeps the links", {
  paths <- character()
  local_mocked_bindings(
    orthopteraFile=function(url, ...) list(type="audio/mpeg", size="1234"),
    orthopteraFetch=function(path, ...) {
      paths <<- c(paths, path)
      if (startsWith(path, "otus/")) return(list(data=list(id=804734, taxon_name=list(cached="Aglaothorax segnis"))))
      sound <- osfFixture()[1]
      if (grepl("page=2", path)) sound <- c(sound, sound)
      list(data=sound, total_pages="2")
    })
  data <- orthopteraSpeciesFileR(pause=0)
  expect_identical(data$id, "44")
  expect_identical(data$type, "audio/mpeg")
  expect_identical(data$size_raw, "1234")
  expect_equal(nrow(attr(data, "links")), 0)
  expect_length(paths, 3)
  expect_equal(sum(startsWith(paths, "otus/")), 1)
  expect_match(paths[3], "page=2", fixed=TRUE)
})

test_that("OSF rejects invalid arguments and incomplete pagination", {
  expect_error(orthopteraSpeciesFileR(per_page=1.5), "per_page")
  expect_error(orthopteraSpeciesFileR(token=""), "token")
  expect_error(orthopteraSpeciesFileR(pause=-1), "pause")
  local_mocked_bindings(orthopteraFetch=function(...) list(data=list(), total_pages=NULL))
  expect_error(orthopteraSpeciesFileR(pause=0), "pagination")
})

test_that("the default token is the one the site publishes", {
  #https://sfg.taxonworks.org/api/v1/ needs no authentication and lists the
  #token of every open TaxonWorks project, so this is not a credential
  expect_identical(formals(orthopteraSpeciesFileR)$token, "3oerVKf82_196cIECvHYNg")
})

test_that("specimen recordings use the accepted determination of their record", {
  paths <- character()
  local_mocked_bindings(
    orthopteraFile=function(url, ...) list(type="", size=""),
    orthopteraFetch=function(path, ...) {
      paths <<- c(paths, path)
      if (startsWith(path, "otus/")) return(list(data=list(id=123, taxon_name=list(cached="Accepted taxon"))))
      if (startsWith(path, "collection_objects/")) {
        return(list(data=list(otu_id=123, eventDate="1994-07-22", country="Mongolia")))
      }
      sound <- osfFixture()[1]
      sound[[1]]$conveyances <- list(list(conveyance_object_type="CollectionObject",
                                          conveyance_object_id=456))
      list(data=c(sound, sound), total_pages="1")
    })
  data <- orthopteraSpeciesFileR(pause=0)
  expect_identical(data$taxon, "Accepted taxon")
  expect_identical(data$Date, "1994-07-22")
  expect_identical(data$country, "MN")
  #One Darwin Core request gives the determination and the occurrence together
  expect_length(paths, 3)
  expect_true(any(grepl("collection_objects/456/dwc", paths, fixed=TRUE)))
})

test_that("OSF HTTP errors retry selectively and report what the server said", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(...) {
    calls <<- calls + 1
    if (calls == 1) return(list(status_code=503, content=charToRaw(""), headers=charToRaw("")))
    list(status_code=200, content=charToRaw("[]"),
         headers=charToRaw("HTTP/2 200\r\nPagination-Total-Pages: 0\r\n"))
  })
  response <- orthopteraFetch("sounds?page=1", "public", NULL, backoff=0)
  expect_identical(response$data, list())
  expect_identical(response$total_pages, "0")
  expect_equal(calls, 2)

  #A client error is not retried, and the server's own message is kept
  local_mocked_bindings(curl_fetch_memory=function(...) list(
    status_code=401, content=charToRaw("{\"success\": false, \"message\": \"Invalid project token\"}"),
    headers=charToRaw("")))
  expect_error(orthopteraFetch("sounds", "secret", NULL, backoff=numeric()),
               "Invalid project token \\(HTTP 401\\)")
  #The token is never in the message
  expect_error(orthopteraFetch("sounds", "secret", NULL, backoff=numeric()), "^(?!.*secret).*$", perl=TRUE)

  local_mocked_bindings(curl_fetch_memory=function(...) list(
    status_code=200, content=charToRaw("<html>"), headers=charToRaw("")))
  expect_error(orthopteraFetch("sounds", "public", NULL, backoff=numeric()), "invalid JSON")
})

test_that("a recording's type and size are read from the last response of the chain", {
  #The short link redirects twice, and only the last response is the audio
  chain <- paste0("HTTP/1.1 301 Moved Permanently\r\nContent-Type: text/html; charset=utf-8\r\n",
                  "HTTP/1.1 302 Found\r\nContent-Type: text/html; charset=utf-8\r\n",
                  "HTTP/1.1 200 OK\r\nContent-Type: audio/x-wav\r\nContent-Length: 882502\r\n")
  local_mocked_bindings(curl_fetch_memory=function(...) list(
    status_code=200, headers=charToRaw(chain)))
  expect_identical(orthopteraFile("https://sfg.taxonworks.org/s/klud2b", NULL),
                   list(type="audio/x-wav", size="882502"))

  #Audio that can't be reached leaves both empty rather than stopping a harvest
  local_mocked_bindings(curl_fetch_memory=function(...) stop("network is down"))
  expect_identical(orthopteraFile("https://sfg.taxonworks.org/s/klud2b", NULL, backoff=numeric()),
                   list(type="", size=""))
})

test_that("attribution labels are read as author, rights holder and licence", {
  #Every shape the 202 published labels take
  expect_identical(orthopteraAttribution("Created by Sigfrid Ingrisch"),
                   list(author="Sigfrid Ingrisch", rights_holder="", license=""))
  expect_identical(orthopteraAttribution("\u00a92025. Created by Jeffrey A. Cole"),
                   list(author="Jeffrey A. Cole", rights_holder="Jeffrey A. Cole", license=""))
  expect_identical(orthopteraAttribution("\u00a92020. Created by Holger Braun. License: CC BY 4.0"),
                   list(author="Holger Braun", rights_holder="Holger Braun",
                        license="https://creativecommons.org/licenses/by/4.0/"))
  #A label that only claims copyright names no author
  expect_identical(orthopteraAttribution("\u00a92020 Klaus-Gerhard Heller & Claudia Hemp"),
                   list(author="", rights_holder="Klaus-Gerhard Heller & Claudia Hemp", license=""))
  expect_identical(orthopteraAttribution(""), list(author="", rights_holder="", license=""))
  #A licence that isn't one of the known ones is left out rather than guessed
  expect_identical(orthopteraAttribution("Created by A. N. Other. License: ask me")$license, "")
})

test_that("country names and whole-year dates are read", {
  expect_identical(countryName2Code(c("Mongolia", "United Republic of Tanzania",
                                      "United States", "C\u00f4te d'Ivoire", "Namibia")),
                   c("MN", "TZ", "US", "CI", "NA"))
  #A code is already a code, and anything that is neither is left out
  expect_identical(countryName2Code(c("de", "Atlantis", "")), c("DE", NA, NA))

  #TaxonWorks writes a year-only date as the whole of that year
  expect_identical(orthopteraDate("1971-01-01/1971-12-31"), "1971")
  expect_identical(orthopteraDate("1994-07-22"), "1994-07-22")
  #Any other range is passed on for normaliseRecordings() to warn about
  expect_identical(orthopteraDate("1971-06-01/1971-08-31"), "1971-06-01/1971-08-31")
  expect_identical(orthopteraDate(""), "")
})

test_that("ingestR uploads an OSF harvest with its links, and skips a failed one", {
  recordings <- NULL
  links <- NULL
  local_mocked_bindings(
    getSources=function() list(list(name="orthoptera-speciesfile", type="recordings",
                                    orthoptera=list(pause=0), process="sourceR")),
    orthopteraSpeciesFileR=function(...) {
      fixture <- osfLinked()
      replay <- osfReplay(fixture)
      lookup <- osfLookup(replay$fetch)
      orthopteraRecordings(fixture$sounds, lookup,
                           orthopteraOccurrences(replay$fetch, lookup), osfFileInfo(fixture))
    },
    uploadTraits=function(...) NULL,
    uploadLinks=function(db, table) links <<- table,
    uploadRecordings=function(db, table) recordings <<- table)
  ingestR(db="db")

  expect_identical(recordings$source, rep("orthoptera-speciesfile", 5))
  #The harvest's links are uploaded as the harvesting source's own
  expect_equal(nrow(links), 1)
  expect_identical(links$source, "orthoptera-speciesfile")
  expect_identical(links$subject_id, "208")

  recordings <- NULL
  links <- NULL
  local_mocked_bindings(orthopteraSpeciesFileR=function(...) stop("harvest failed"))
  expect_warning(ingestR(db="db"), "Skipping source orthoptera-speciesfile - harvest failed")
  expect_null(recordings)
  expect_null(links)
})

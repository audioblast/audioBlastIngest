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

#A harvest of the fixture, as orthopteraSpeciesFileR() runs one
osfHarvest <- function(fixture, replay) {
  taxonomy <- orthopteraTaxonomy(replay$fetch)
  data <- orthopteraRecordings(fixture$sounds, taxonomy$name,
                               orthopteraOccurrences(replay$fetch, taxonomy$name),
                               osfFileInfo(fixture))
  return(list(recordings=data, taxa=taxonomy$taxa(), links=attr(data, "links")))
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
  data <- osfHarvest(fixture, osfReplay(fixture))$recordings

  expect_identical(data$id, c("55", "125", "126", "199", "208"))
  #The specimen, the two field observations and the one event whose
  #occurrences agree; the last recording is not identified by the source
  expect_identical(data$taxon, c("Heteropternis couloniana",
                                 "Chorthippus (Glyptobothrus) eisentrauti",
                                 "Chorthippus (Glyptobothrus) eisentrauti",
                                 "Chorthippus (Altichorthippus) intermedius", ""))
  expect_identical(data$Date, c("1994-07-22", "1991-09-03", "1991-09-04", "1997-06-17", ""))
  #Darwin Core names a country rather than coding it; countryCode() reads it,
  #as it does for every source
  expect_identical(data$country,
                   c("United Republic of Tanzania", "Italy", "Italy", "Mongolia", ""))
  expect_identical(normaliseRecordings(data)$country, c("TZ", "IT", "IT", "MN", NA))
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

test_that("each recording says which taxa it is about", {
  fixture <- osfLinked()
  links <- osfHarvest(fixture, osfReplay(fixture))$links

  expect_identical(names(links), names(getHeaders("links")))
  #One for each of the five recordings, the last of them the inferred name
  expect_identical(links$subject_id, c("55", "125", "126", "199", "208"))
  expect_true(all(links$subject_type == "recordings"))
  expect_true(all(links$predicate == "http://purl.obolibrary.org/obo/IAO_0000136"))
  #The taxon is one the harvest gives, by its OTU id
  expect_true(all(links$object_type == "taxa"))
  expect_identical(links$object_id, c("809995", "812603", "812603", "812775", "842419"))
  #uploadLinks() must accept them, with the source filled in as ingestR() does
  links$source <- "orthoptera-speciesfile"
  expect_equal(nrow(normaliseLinks(links)), 5)
})

test_that("a name read from a title is qualified, and is not the recording's taxon", {
  fixture <- osfLinked()
  harvest <- osfHarvest(fixture, osfReplay(fixture))
  recordings <- harvest$recordings
  inferred <- harvest$links[harvest$links$subject_id == "208", ]

  #The collecting event of sound 208 has no occurrences at all, so the source
  #identifies nothing: only its title names Parasubria vittipes
  expect_identical(recordings$taxon[recordings$id == "208"], "")
  expect_equal(nrow(inferred), 1)
  expect_identical(inferred$object_id, "842419")
  expect_identical(inferred$qualifier,
                   "https://vocab.audioblast.org/cv/identificationBasis#RecordingTitle")
  expect_match(inferred$remarks, "does not identify this recording", fixed=TRUE)
  #The taxa the source does identify are linked without a qualifier
  expect_true(all(harvest$links$qualifier[harvest$links$subject_id != "208"] == ""))
  #The taxon it names is still given, so the link does not dangle
  expect_true("842419" %in% harvest$taxa$id)
})

test_that("a taxon is given with the whole classification above it", {
  fixture <- osfLinked()
  replay <- osfReplay(fixture)
  taxa <- osfHarvest(fixture, replay)$taxa

  expect_identical(names(taxa), names(getHeaders("taxa")))
  #Four OTUs are read, and every rank above each of them comes with it
  expect_equal(sum(startsWith(replay$paths(), "otus/")), 4)
  above <- taxa[order(as.integer(taxa$id)), ]
  expect_identical(taxa$taxon[taxa$id == "842419"], "Parasubria vittipes")
  expect_identical(taxa$Rank[taxa$id == "842419"], "Species")
  #A taxon's parent is the OTU of the taxon name above it
  expect_identical(taxa$parent_id[taxa$id == "842419"], "842418")
  expect_identical(taxa$taxon[taxa$id == "842418"], "Parasubria")
  expect_identical(taxa$Rank[taxa$id == "842418"], "Genus")
  #The two Chorthippus taxa share everything above their genus
  expect_true(all(c("805980", "805967") %in% taxa$id))
  expect_identical(taxa$Rank[taxa$id == "805980"], "Order")
  expect_identical(taxa$taxon[taxa$id == "805980"], "Orthoptera")
  #TaxonWorks roots its names at a rankless Root, which is not a taxon, so the
  #walk ends at the kingdom
  expect_false("Root" %in% taxa$taxon)
  expect_identical(taxa$parent_id[taxa$id == "805967"], "")

  #taxonomiseR() walks the parents into a column for each rank
  walked <- taxonomiseR(sourceR("orthoptera-speciesfile", taxa))
  species <- walked[walked$id == "842419", ]
  expect_identical(species$Species, "Parasubria vittipes")
  expect_identical(species$Genus, "Parasubria")
  expect_identical(species$Family, "Tettigoniidae")
  expect_identical(species$Order, "Orthoptera")
  expect_identical(species$Kingdom, "Animalia")
  #A rank the taxa table has no column for is still walked through
  expect_identical(walked$Family[walked$id == "809995"], "Acrididae")
})

test_that("Darwin Core records and OTUs are each read once", {
  fixture <- osfLinked()
  replay <- osfReplay(fixture)
  occurrences <- orthopteraOccurrences(replay$fetch, orthopteraTaxonomy(replay$fetch)$name)
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
  #Each is still about the taxon it is conveyed on
  expect_identical(attr(data, "links")$object_id, c("804734", "810653"))
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
  expect_identical(links$object_id, c("804734", "123"))
  expect_true(all(links$qualifier == ""))

  #A taxon that can't be named is not given, so nothing is linked to it
  expect_equal(nrow(attr(orthopteraRecordings(sound, function(id) ""), "links")), 0)

  sound[[1]]$conveyances <- NULL
  expect_identical(orthopteraRecordings(sound, function(id) stop("lookup"))$taxon, "")
})

test_that("a harvest gives recordings, taxa and links, and pages once each", {
  paths <- character()
  local_mocked_bindings(
    orthopteraFile=function(url, ...) list(type="audio/mpeg", size="1234"),
    orthopteraFetch=function(path, ...) {
      paths <<- c(paths, path)
      if (startsWith(path, "otus/")) {
        return(list(data=list(id=804734, taxon_name=list(id=1, cached="Aglaothorax segnis",
                                                         rank="species", parent_id=2))))
      }
      sound <- osfFixture()[1]
      if (grepl("page=2", path)) sound <- c(sound, sound)
      list(data=sound, total_pages="2")
    })
  harvest <- orthopteraSpeciesFileR(pause=0)

  expect_identical(names(harvest), c("recordings", "taxa", "links"))
  #Results that change while paging can repeat a sound on two pages
  expect_identical(harvest$recordings$id, "44")
  expect_identical(harvest$recordings$type, "audio/mpeg")
  expect_identical(harvest$recordings$size_raw, "1234")
  expect_identical(harvest$taxa$id, "804734")
  expect_identical(harvest$taxa$Rank, "Species")
  #Its parent is a taxon name no OTU of this chain gives, so the walk ends here
  expect_identical(harvest$taxa$parent_id, "")
  expect_identical(harvest$links$subject_id, "44")
  expect_identical(harvest$links$object_id, "804734")
  expect_length(paths, 3)
  expect_equal(sum(startsWith(paths, "otus/")), 1)
  expect_match(paths[3], "page=2", fixed=TRUE)
  expect_match(paths[2], "extend[]=parents", fixed=TRUE)
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
      if (startsWith(path, "otus/")) {
        return(list(data=list(id=123, taxon_name=list(id=9, cached="Accepted taxon",
                                                      rank="species"))))
      }
      if (startsWith(path, "collection_objects/")) {
        return(list(data=list(otu_id=123, eventDate="1994-07-22", country="Mongolia")))
      }
      sound <- osfFixture()[1]
      sound[[1]]$conveyances <- list(list(conveyance_object_type="CollectionObject",
                                          conveyance_object_id=456))
      list(data=c(sound, sound), total_pages="1")
    })
  harvest <- orthopteraSpeciesFileR(pause=0)
  data <- harvest$recordings
  expect_identical(data$taxon, "Accepted taxon")
  expect_identical(data$Date, "1994-07-22")
  expect_identical(data$country, "Mongolia")
  expect_identical(harvest$taxa$taxon, "Accepted taxon")
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
  #countryName2Code() reads names alone; countryCode(), which normalising
  #calls, reads codes too and is what a source's countries go through
  expect_identical(countryName2Code(c("de", "Atlantis", "")), rep(NA_character_, 3))
  expect_identical(countryCode(c("de", "Mongolia", "Atlantis", "")), c("DE", "MN", NA, NA))

  #TaxonWorks writes a year-only date as the whole of that year
  expect_identical(orthopteraDate("1971-01-01/1971-12-31"), "1971")
  expect_identical(orthopteraDate("1994-07-22"), "1994-07-22")
  #Any other range is passed on for normaliseRecordings() to warn about
  expect_identical(orthopteraDate("1971-06-01/1971-08-31"), "1971-06-01/1971-08-31")
  expect_identical(orthopteraDate(""), "")
})

test_that("ingestR uploads an OSF harvest's recordings, taxa and links", {
  recordings <- NULL
  taxa <- NULL
  links <- NULL
  local_mocked_bindings(
    getSources=function() list(list(name="orthoptera-speciesfile", type="recordings",
                                    orthoptera=list(pause=0), process="sourceR")),
    orthopteraSpeciesFileR=function(...) {
      fixture <- osfLinked()
      osfHarvest(fixture, osfReplay(fixture))
    },
    uploadTraits=function(...) NULL,
    uploadLinks=function(db, table) links <<- table,
    uploadTaxa=function(db, table) taxa <<- table,
    uploadRecordings=function(db, table) recordings <<- table)
  ingestR(db="db")

  #Each table the harvest gives is ingested as though it were a source of its own
  expect_identical(recordings$source, rep("orthoptera-speciesfile", 5))
  expect_true(all(taxa$source == "orthoptera-speciesfile"))
  expect_true(all(links$source == "orthoptera-speciesfile"))
  expect_identical(links$subject_id, c("55", "125", "126", "199", "208"))
  #taxonomiseR() has run, so the taxa carry a column for each rank
  expect_identical(taxa$Species[taxa$id == "842419"], "Parasubria vittipes")
  #Every taxon a link names is one of the taxa uploaded, so none dangles
  expect_true(all(links$object_id %in% taxa$id))

  recordings <- NULL
  taxa <- NULL
  links <- NULL
  local_mocked_bindings(orthopteraSpeciesFileR=function(...) stop("harvest failed"))
  expect_warning(ingestR(db="db"), "Skipping source orthoptera-speciesfile - harvest failed")
  expect_null(recordings)
  expect_null(taxa)
  expect_null(links)
})

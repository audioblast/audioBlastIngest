xcFixture <- function() {
  path <- test_path("fixtures", "xenocanto-page.json")
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  rjson::fromJSON(body)
}

xcPage <- function(ids, page, numPages) {
  recordings <- lapply(ids, function(id) {
    list(id=id, gen="Gryllus", sp="campestris", ssp="", grp="grasshoppers",
         status="identified", en="Field Cricket", rec="A. Recordist",
         cnt="United Kingdom", q="A",
         lat="51.5", lon="-0.1", type="calling song",
         file=paste0("https://xeno-canto.org/", id, "/download"),
         `file-name`=paste0("XC", id, ".mp3"), length="0:30", time="14:00",
         date="2020-07-01", uploaded="2020-07-02")
  })
  rjson::toJSON(list(numRecordings=as.character(length(ids)), numSpecies="1",
                     page=page, numPages=numPages, recordings=recordings))
}

xcResponse <- function(status, body) {
  list(status_code=status, content=charToRaw(enc2utf8(body)))
}

test_that("xeno-canto recordings are converted to the recordings format", {
  data <- xenocantoRecordings(xcFixture()$recordings)

  expect_identical(names(data), names(getHeaders("recordings")))
  expect_true(all(vapply(data, is.character, logical(1))))
  #The restricted species recording has no audio, so is left out
  expect_identical(data$id, c("694038", "700002", "1179094", "100000"))

  wren <- data[1, ]
  expect_identical(wren$source, "")
  expect_identical(wren$Title, "XC694038 Eurasian Wren (Troglodytes troglodytes) - song")
  expect_identical(wren$taxon, "Troglodytes troglodytes")
  expect_identical(wren$file, "https://xeno-canto.org/694038/download")
  expect_identical(wren$author, "Jos\u00e9 N\u00fa\u00f1ez")
  expect_identical(Encoding(wren$author), "UTF-8")
  expect_identical(wren$post_date, "2021-12-27")
  expect_identical(wren$size, "")
  expect_identical(wren$size_raw, "")
  expect_identical(wren$type, "audio/mpeg")
  expect_identical(wren$NonSpecimen, "")
  expect_identical(wren$Date, "2021-12-23")
  expect_identical(wren$Time, "09:30")
  expect_identical(wren$Duration, "248")
  expect_identical(wren$deployment, "")
  expect_identical(wren$lat, "42.8373")
  expect_identical(wren$lon, "-8.652")
  expect_identical(wren$time_of_day, "")
  expect_identical(wren$license, "https://creativecommons.org/licenses/by-nc-sa/4.0/")
  expect_identical(wren$info_url, "https://xeno-canto.org/694038")
  expect_identical(wren$device, "")
  expect_identical(wren$rights_holder, "Jos\u00e9 N\u00fa\u00f1ez")
  #xeno-canto names a country rather than coding it, so the name is read as one
  expect_identical(wren$country, "ES")
  expect_identical(wren$locality, "A Coru\u00f1a, Galicia")
  expect_identical(wren$sample_rate, "44100")
  expect_identical(wren$channels, "")

  soundscape <- data[2, ]
  expect_identical(soundscape$Title, "XC700002 Soundscape")
  expect_identical(soundscape$taxon, "")
  expect_identical(soundscape$NonSpecimen, "Soundscape")
  expect_identical(soundscape$type, "audio/x-wav")
  expect_identical(soundscape$Duration, "3723")
  expect_identical(soundscape$Date, "1998-06")
  expect_identical(soundscape$Time, "")
  #A time of "?" says nothing about the time of day
  expect_identical(soundscape$time_of_day, "")
  expect_identical(soundscape$lat, NA_character_)
  expect_identical(soundscape$lon, NA_character_)
  expect_identical(soundscape$license, "")
  expect_identical(soundscape$info_url, "https://xeno-canto.org/700002")
  expect_identical(soundscape$country, "BR")

  mystery <- data[3, ]
  expect_identical(mystery$Title, "XC1179094 Identity unknown - call, flight call")
  expect_identical(mystery$taxon, "")
  expect_identical(mystery$file, "https://xeno-canto.org/1179094/download")
  expect_identical(mystery$type, "audio/flac")
  expect_identical(mystery$Date, "")
  expect_identical(mystery$Time, "07:05")
  expect_identical(mystery$time_of_day, "")
  expect_identical(mystery$info_url, "https://xeno-canto.org/1179094")

  gull <- data[4, ]
  expect_identical(gull$Title, "XC100000 Lesser Black-backed Gull (Larus fuscus fuscus)")
  expect_identical(gull$taxon, "Larus fuscus fuscus")
  expect_identical(gull$type, "")
  expect_identical(gull$Duration, NA_character_)
  expect_identical(gull$Time, "")
  expect_identical(gull$time_of_day, "morning")
  expect_identical(gull$Date, "2004")
  expect_identical(gull$lat, NA_character_)
  expect_identical(gull$lon, "24.9")
})

test_that("an empty xeno-canto page has no recordings", {
  data <- xenocantoRecordings(list())
  expect_identical(names(data), names(getHeaders("recordings")))
  expect_equal(nrow(data), 0)
})

test_that("what a recording holds beside its columns becomes its details", {
  details <- xenocantoDetails(xcFixture()$recordings)

  expect_identical(names(details), names(getHeaders("details")))
  expect_true(all(vapply(details, is.character, logical(1))))
  expect_true(all(details$source == "" & details$delta == "0"))
  expect_true(all(details$type %in% c("recordings", "annomate")))
  details <- details[details$type == "recordings", ]
  #The restricted species recording is not in the recordings table, so its
  #details would belong to no record
  expect_false("700001" %in% details$id)

  wren <- details[details$id == "694038", ]
  expect_identical(
    setNames(wren$value, wren$name),
    c(alt="30", q="A", method="field recording", sex="male", stage="adult",
      `animal-seen`="yes", `playback-used`="no"))
  expect_identical(wren$unit[wren$name == "alt"], "m")
  #An automatic recording of "unknown" says nothing about the recording
  expect_false("auto" %in% wren$name)

  #A soundscape whose every extra field says the recordist did not know
  expect_equal(nrow(details[details$id == "700002", ]), 0)

  mystery <- details[details$id == "1179094", ]
  expect_identical(
    setNames(mystery$value, mystery$name),
    c(temp="23.5", method="in the hand", regnr="332"))
  expect_identical(mystery$unit[mystery$name == "temp"], "\u00b0C")
  #An altitude of "-" is not a number, and a life stage of "uncertain" is not one
  expect_false(any(c("alt", "stage") %in% mystery$name))

  gull <- details[details$id == "100000", ]
  expect_identical(
    setNames(gull$value, gull$name),
    c(`animal-seen`="no", rmk="Calling from a rooftop."))
  #Playback of "unknown" says nothing about whether playback was used
  expect_false("playback-used" %in% gull$name)
})

test_that("an empty xeno-canto page has no details", {
  details <- xenocantoDetails(list())
  expect_identical(names(details), names(getHeaders("details")))
  expect_equal(nrow(details), 0)
})

test_that("a recording is linked to the taxon it is of and the ones behind it", {
  links <- xenocantoLinks(xcFixture()$recordings)
  background <- "https://vocab.audioblast.org/cv/recordingContent#NonFocalTaxa"

  expect_identical(names(links), names(getHeaders("links")))
  #A recording is about a background species as it is about its own taxon
  expect_true(all(links$predicate == "http://purl.obolibrary.org/obo/IAO_0000136"))
  #The sources are the linking source's own, which normaliseLinks() fills in
  expect_true(all(links$source == "" & links$subject_source == "" & links$object_source == ""))

  taxa <- links[links$object_type == "taxa", ]
  expect_true(all(taxa$subject_type == "recordings"))

  #The wren and the gull are identified; the soundscape and the unidentified
  #recording are of no taxon, so neither is linked to one
  focal <- taxa[taxa$qualifier == "", ]
  expect_identical(focal$subject_id, c("694038", "100000"))
  expect_identical(focal$object_id, c("Troglodytes troglodytes", "Larus fuscus fuscus"))

  #Only the wren recording lists anything in also, and the qualifier says those
  #taxa are not what it is of
  heard <- links[links$qualifier == background, ]
  expect_identical(heard$subject_id, c("694038", "694038"))
  expect_identical(heard$object_id, c("Turdus viscivorus", "Parus major"))

  #uploadLinks() takes them without complaint and gives each one an id: two
  #taxa the wren recording is of and heard behind it, the gull's own taxon,
  #and the sonogram of the wren recording
  normalised <- normaliseLinks(sourceR("xeno-canto", links))
  expect_equal(nrow(normalised), 5)
  expect_true(all(normalised$subject_source == "xeno-canto" &
                    normalised$object_source == "xeno-canto"))
  expect_true(all(grepl("^[0-9a-f]{40}$", normalised$id)))
})

test_that("a page of recordings of nothing has no links", {
  expect_equal(nrow(xenocantoLinks(list())), 0)
  #The restricted species recording has an empty also, and no audio either
  expect_equal(nrow(xenocantoLinks(xcFixture()$recordings[2])), 0)
  expect_identical(names(xenocantoLinks(list())), names(getHeaders("links")))
})

test_that("the regions someone marked in a recording become annotations", {
  annotations <- xenocantoAnnotations(xcFixture()$recordings)

  expect_identical(names(annotations), names(getHeaders("ann-o-mate")))
  expect_true(all(vapply(annotations, is.character, logical(1))))
  #Only the wren recording has an annotation set, and its number is
  #xeno-canto's own across the collection rather than within the set
  expect_identical(annotations$annotation_id, c("86", "87"))
  expect_true(all(annotations$source_id == "694038"))
  expect_identical(annotations$taxon, c("Troglodytes troglodytes", "Periparus ater"))
  expect_identical(annotations$time_start, c("0.27", "10.28"))
  expect_identical(annotations$time_end, c("3.33", "11.92"))
  expect_identical(annotations$annotator, c("W.P. Vellinga", "W.P. Vellinga"))
  #A sound type xeno-canto does not hold is empty rather than "NULL"
  expect_identical(annotations$type, c("song", ""))

  #The date and address are the set the annotation really came from, not the
  #set xeno-canto builds for the response, whose date is the request's
  expect_identical(annotations$annotation_date, c("2026-03-01", "2026-03-18"))
  expect_identical(annotations$annotation_info_url,
                   c("https://xeno-canto.org/annotation/set/1",
                     "https://xeno-canto.org/annotation/set/14"))

  #Each annotation carries where and what the recording it is of is
  expect_true(all(annotations$recording_url == "https://xeno-canto.org/694038/download"))
  expect_true(all(annotations$recording_info_url == "https://xeno-canto.org/694038"))
  expect_true(all(annotations$lat == "42.8373" & annotations$lon == "-8.652"))
  expect_true(all(annotations$contact == ""))
})

test_that("what an annotation holds beside its columns becomes its details", {
  details <- xenocantoDetails(xcFixture()$recordings)
  annotation <- details[details$type == "annomate", ]

  expect_identical(
    setNames(annotation$value[annotation$id == "86"], annotation$name[annotation$id == "86"]),
    c(frequency_low="2551", frequency_high="10204", sex="male", life_stage="adult",
      annotation_remarks="audible rain drops", set_name="Demonstration set",
      set_license="CC-BY-NC-4.0"))
  expect_identical(annotation$unit[annotation$name == "frequency_low"], c("Hz", "Hz"))

  #The second annotation says nothing about the animal, and is bounded from
  #0 Hz, which is a frequency it holds rather than one it does not have
  second <- annotation[annotation$id == "87", ]
  expect_identical(second$name, c("frequency_low", "frequency_high", "set_name", "set_license"))
  expect_identical(second$value[second$name == "frequency_low"], "0")
  #and a recording's own details are still there beside them
  expect_true(any(details$type == "recordings" & details$name == "alt"))
})

test_that("a page with no annotations has none", {
  expect_equal(nrow(xenocantoAnnotations(list())), 0)
  expect_identical(names(xenocantoAnnotations(list())), names(getHeaders("ann-o-mate")))
  #The soundscape has no annotation-set at all
  expect_equal(nrow(xenocantoAnnotations(xcFixture()$recordings[3])), 0)
  expect_equal(nrow(xenocantoAnnotationDetails(list())), 0)
})

test_that("the sonogram xeno-canto renders is an image of its own", {
  images <- xenocantoImages(xcFixture()$recordings)

  expect_identical(names(images), names(getHeaders("images")))
  #Only the wren has a sono in the fixture, and the colour one is taken rather
  #than the greyscale thumbnails of it
  expect_identical(images$id, "694038-colour")
  expect_identical(images$file,
                   "https://xeno-canto.org/sounds/spectrograms/X/694038/colour.png")
  expect_identical(images$subtype, "Sonogram")
  expect_identical(images$creator, "Xeno-canto Foundation")
  #A sonogram is under the licence of the recording it depicts
  expect_identical(images$license, "https://creativecommons.org/licenses/by-nc-sa/4.0/")
  expect_identical(images$type, "image/png")
  expect_true(all(images$title == "" & images$post_date == "" & images$width == ""))

  #uploadImages() takes it as it stands
  normalised <- normaliseImages(sourceR("xeno-canto", images))
  expect_equal(nrow(normalised), 1)
  expect_identical(normalised$source, "xeno-canto")
})

test_that("a sonogram is linked to the recording it shows", {
  links <- xenocantoLinks(xcFixture()$recordings)
  shows <- links[links$subject_type == "images", ]

  expect_identical(shows$subject_id, "694038-colour")
  expect_identical(shows$object_type, "recordings")
  expect_identical(shows$object_id, "694038")
  #An image says what it shows the way bio.acousti.ca's images do
  expect_identical(shows$predicate, "http://purl.obolibrary.org/obo/IAO_0000136")
  expect_identical(shows$qualifier, "")
  #and the image it names is one the harvest gives
  expect_true(all(shows$subject_id %in% xenocantoImages(xcFixture()$recordings)$id))
})

test_that("a page with no sonograms has no images", {
  expect_equal(nrow(xenocantoImages(list())), 0)
  expect_identical(names(xenocantoImages(list())), names(getHeaders("images")))
  #The restricted species has no audio, so nothing rendered a sonogram of it
  expect_equal(nrow(xenocantoImages(xcFixture()$recordings[2])), 0)
})

test_that("the taxa a recording names are records of their own", {
  taxa <- xenocantoTaxa(xcFixture()$recordings)

  expect_identical(names(taxa), names(getHeaders("taxa")))
  #Each name, and the names it sits in, whether or not anything was recorded
  #of those alone
  expect_identical(
    sort(taxa$id),
    sort(c("Troglodytes", "Troglodytes troglodytes",
           "Larus", "Larus fuscus", "Larus fuscus fuscus",
           "Turdus", "Turdus viscivorus", "Parus", "Parus major")))
  expect_identical(taxa$taxon, taxa$id)

  rank <- setNames(taxa$Rank, taxa$id)
  expect_identical(unname(rank[c("Larus", "Larus fuscus", "Larus fuscus fuscus")]),
                   c("Genus", "Species", "Subspecies"))
  parent <- setNames(taxa$parent_id, taxa$id)
  expect_identical(unname(parent[c("Larus", "Larus fuscus", "Larus fuscus fuscus")]),
                   c("", "Larus", "Larus fuscus"))
  expect_identical(taxa$parent_taxon, taxa$parent_id)

  #taxonomiseR() reads the classification the names carry
  out <- taxonomiseR(sourceR("xeno-canto", taxa))
  gull <- out[out$id == "Larus fuscus fuscus", ]
  expect_identical(gull$Genus, "Larus")
  expect_identical(gull$Species, "Larus fuscus")
  expect_identical(gull$Subspecies, "Larus fuscus fuscus")
  #xeno-canto gives nothing above the genus, so there is no rank above it to
  #have a column at all; uploadTaxa() fills the ones the table has as NULL
  expect_identical(names(out)[-(1:5)], c("Genus", "Species", "Subspecies"))
  expect_null(gull$Family)
})

test_that("only a scientific name becomes a taxon", {
  expect_identical(
    xenocantoName(c("Larus fuscus", "Larus fuscus fuscus", "Larus",
                    "Pipistrellus sp.", "cf. graellsii", "larus fuscus",
                    "Larus fuscus fuscus graellsii", "")),
    c("Larus fuscus", "Larus fuscus fuscus", "", "", "", "", "", ""))
  expect_equal(nrow(xenocantoTaxa(list())), 0)
  expect_identical(names(xenocantoTaxa(list())), names(getHeaders("taxa")))
})

test_that("xeno-canto values are normalised", {
  expect_identical(
    xenocantoDuration(c("0:05", "4:08", "1:02:03", "100:00:00", "", "?", "1:2:3:4")),
    c("5", "248", "3723", "360000", NA, NA, NA))
  expect_identical(
    xenocantoDate(c("2021-12-23", "2021-12-00", "2021-00-00", "0000-00-00", "2021-13-01", "?", "")),
    c("2021-12-23", "2021-12", "2021", "", "", "", ""))
  expect_identical(
    xenocantoTime(c("09:30", "9:30", "09.30", "23:59", "24:00", "?", "")),
    c("09:30", "09:30", "09:30", "23:59", "", "", ""))
  expect_identical(
    xenocantoTaxon(c("Larus", "Larus", "Larus", "Larus"), rep("fuscus", 4),
                   c("fuscus", "cf. graellsii", "", "fuscus"),
                   c("birds", "birds", "birds", "birds"),
                   c("identified", "questioned", "", "unidentified")),
    c("Larus fuscus fuscus", "Larus fuscus", "Larus fuscus", ""))
  expect_identical(
    xenocantoTimeOfDay(c("09:30", "morning", "dawn chorus", "?", "")),
    c("", "morning", "dawn chorus", "", ""))
  expect_identical(
    xenocantoURL(c("https://xeno-canto.org/1", "//xeno-canto.org/2",
                   "//creativecommons.org/licenses/by-nc-sa/4.0/", "", "not a URL")),
    c("https://xeno-canto.org/1", "https://xeno-canto.org/2",
      "https://creativecommons.org/licenses/by-nc-sa/4.0/", "", ""))
  expect_identical(
    xenocantoDevice(c("Zoom H5", "Zoom H5", "", ""), c("Telinga", "", "Telinga", "")),
    c("Zoom H5, Telinga", "Zoom H5", "Telinga", ""))
  expect_identical(
    coordinate(c("51.5", "-180", "180.5", "", "north"), 180),
    c("51.5", "-180", NA, NA, NA))
  #The ISO 3166-1 name, a shortening only one country has, a name in common
  #use, and one that is no country's
  expect_identical(
    countryName2Code(c("Russian Federation", "Bolivia", "united  states", "Laos", "Atlantis", "")),
    c("RU", "BO", "US", "LA", NA, NA))
  #xeno-canto writes these names without their and, and tells the two Congos
  #apart in brackets
  expect_identical(
    countryName2Code(c("Bosnia Herzegovina", "Trinidad Tobago",
                       "Congo (Brazzaville)", "Congo (Democratic Republic)")),
    c("BA", "TT", "CG", "CD"))
  expect_warning(
    expect_identical(xenocantoCountry(c("Spain", "Atlantis", "")), c("ES", "", "")),
    "country that could not be read")
})

test_that("xeno-canto harvests page through every query", {
  urls <- character(0)
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    urls <<- c(urls, url)
    page <- as.integer(sub(".*[?&]page=([0-9]+).*", "\\1", url))
    if (grepl("bats", url, fixed=TRUE)) {
      #Results shifting between requests repeat recording 2 on page 2
      return(xcResponse(200, xcPage(list(c("1", "2"), c("2", "3"))[[page]], page, 2)))
    }
    xcResponse(200, xcPage("4", 1, 1))
  })

  harvest <- xenocantoR(c("grp:bats", 'grp:"land mammals"'), key="secret", per_page=50, pause=0)

  expect_identical(names(harvest),
                   c("recordings", "details", "taxa", "images", "ann-o-mate", "links"))
  expect_identical(names(harvest$recordings), names(getHeaders("recordings")))
  expect_identical(names(harvest$details), names(getHeaders("details")))
  expect_identical(names(harvest$taxa), names(getHeaders("taxa")))
  expect_identical(names(harvest$images), names(getHeaders("images")))
  expect_identical(names(harvest[["ann-o-mate"]]), names(getHeaders("ann-o-mate")))
  expect_identical(names(harvest$links), names(getHeaders("links")))
  #Every page names the same cricket, which is one taxon record, not four
  expect_identical(harvest$taxa$id, c("Gryllus", "Gryllus campestris"))
  #A recording that a shifting result set puts on two pages is harvested once
  expect_identical(harvest$recordings$id, c("1", "2", "3", "4"))
  #and so gives its details once as well
  expect_identical(harvest$details$id, c("1", "2", "3", "4"))
  expect_true(all(harvest$details$name == "q"))
  expect_identical(harvest$recordings$country, rep("GB", 4))
  expect_length(urls, 3)
  expect_match(urls[1], "?query=grp%3Abats&page=1&per_page=50&key=secret", fixed=TRUE)
  expect_match(urls[2], "&page=2&", fixed=TRUE)
  expect_match(urls[3], "?query=grp%3A%22land%20mammals%22&page=1&", fixed=TRUE)
})

test_that("xeno-canto harvests check their arguments", {
  expect_error(xenocantoR("grp:bats", key=""), "XC_API_KEY")
  expect_error(xenocantoR(character(0), key="secret"), "query")
  expect_error(xenocantoR("grp:bats", key="secret", per_page=1000), "per_page")
})

test_that("failed xeno-canto requests are retried", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    calls <<- calls + 1
    if (calls == 1) stop("Timeout was reached")
    if (calls == 2) return(xcResponse(503, "<html>Service Unavailable</html>"))
    xcResponse(200, xcPage("1", 1, 1))
  })

  json <- xenocantoFetch("grp:bats", 1, 100, "secret", NULL, backoff=c(0, 0))

  expect_equal(calls, 3)
  expect_identical(json$recordings[[1]]$id, "1")
})

test_that("xeno-canto requests give up after the last retry", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    calls <<- calls + 1
    xcResponse(503, "<html>Service Unavailable</html>")
  })

  expect_error(
    xenocantoFetch("grp:bats", 2, 100, "secret", NULL, backoff=c(0, 0)),
    "xeno-canto request for 'grp:bats' page 2 failed: unexpected response (HTTP 503)",
    fixed=TRUE)
  expect_equal(calls, 3)
})

test_that("xeno-canto client errors are reported without retrying", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    calls <<- calls + 1
    if (grepl("bats", url, fixed=TRUE)) {
      return(xcResponse(401, '{"error": "client_error", "message": "Missing or invalid \'key\' parameter."}'))
    }
    xcResponse(400, '{"error": {"code": "missing_parameter", "message": "No query specified"}}')
  })

  expect_error(
    xenocantoFetch("grp:bats", 1, 100, "secret", NULL, backoff=c(0, 0)),
    "Missing or invalid 'key' parameter. (HTTP 401)", fixed=TRUE)
  expect_error(
    xenocantoFetch("grp:frogs", 1, 100, "secret", NULL, backoff=c(0, 0)),
    "No query specified (HTTP 400)", fixed=TRUE)
  expect_equal(calls, 2)
})

test_that("the xeno-canto API key is kept out of error messages", {
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    stop(paste("Could not open", url))
  })

  error <- expect_error(xenocantoFetch("grp:bats", 1, 100, "secret-key", NULL, backoff=numeric(0)))

  expect_false(grepl("secret-key", conditionMessage(error), fixed=TRUE))
  expect_match(conditionMessage(error), "key=<key>", fixed=TRUE)
})

test_that("the xeno-canto source module is read from list_sources", {
  #JSON as served for modules/xenocanto/module.php
  json <- '{"data":{"xeno-canto":[{"type":"recordings","xenocanto":{"query":["grp:birds","grp:\\"land mammals\\""]},"process":["sourceR"]}]}}'
  local_mocked_bindings(fromJSON=function(...) rjson::fromJSON(json))

  sources <- getSources()

  expect_identical(sources[[1]]$name, "xeno-canto")
  expect_identical(sources[[1]]$xenocanto$query, c("grp:birds", 'grp:"land mammals"'))
  expect_identical(sources[[1]]$process, "sourceR")
})

ingestWithSources <- function(harvest) {
  csv <- tempfile(fileext=".csv")
  legacy <- c(source="legacy", id="7", Title="Pond", taxon="", file="https://example.org/7.wav",
              author="", post_date="", size="", size_raw="", type="audio/x-wav",
              NonSpecimen="Soundscape", Date="", Time="", Duration="60", deployment="pond")
  write.csv(as.data.frame(t(legacy)), csv, row.names=FALSE)

  uploaded <- list()
  local_mocked_bindings(
    getSources=function() list(
      list(name="legacy", type="recordings", url=csv, process=list()),
      list(name="xeno-canto", type="recordings", xenocanto=list(query="grp:birds"), process="sourceR")),
    xenocantoR=harvest,
    uploadTraits=function(db, table) NULL,
    uploadDetails=function(db, table) uploaded$details <<- table,
    uploadLinks=function(db, table) uploaded$links <<- table,
    uploadTaxa=function(db, table) uploaded$taxa <<- table,
    uploadImages=function(db, table) uploaded$images <<- table,
    uploadAnnOmate=function(db, table) uploaded$annomate <<- table,
    uploadRecordings=function(db, table) uploaded$recordings <<- table)
  ingestR(db="db")
  unlink(csv)
  return(uploaded)
}

xcHarvest <- function(query, ...) {
  recordings <- xcFixture()$recordings
  list(recordings=xenocantoRecordings(recordings), details=xenocantoDetails(recordings),
       taxa=xenocantoTaxa(recordings), images=xenocantoImages(recordings),
       `ann-o-mate`=xenocantoAnnotations(recordings), links=xenocantoLinks(recordings))
}

test_that("ingestR uploads xeno-canto recordings with other recordings sources", {
  uploaded <- ingestWithSources(xcHarvest)$recordings

  expect_identical(names(uploaded), names(getHeaders("recordings")))
  expect_identical(uploaded$source, c("legacy", rep("xeno-canto", 4)))
  expect_identical(uploaded$id, c("7", "694038", "700002", "1179094", "100000"))
  #Sources without lat and lon still line up with the standard columns
  expect_identical(unlist(uploaded[1, c("deployment", "lat", "lon")], use.names=FALSE), c("pond", "", ""))
  expect_identical(unlist(uploaded[2, c("deployment", "lat", "lon")], use.names=FALSE), c("", "42.8373", "-8.652"))
  #as do sources without the columns added after them
  added <- c("time_of_day", "license", "info_url", "device")
  expect_identical(unlist(uploaded[1, added], use.names=FALSE), c("", "", "", ""))
  expect_identical(unlist(uploaded[2, added], use.names=FALSE),
                   c("", "https://creativecommons.org/licenses/by-nc-sa/4.0/", "https://xeno-canto.org/694038", ""))
})

test_that("ingestR uploads the details a harvest gives beside its recordings", {
  uploaded <- ingestWithSources(xcHarvest)

  #One source gave two types of table, and each was ingested as its own type
  expect_identical(names(uploaded$details), names(getHeaders("details")))
  expect_true(all(uploaded$details$source == "xeno-canto"))
  recorded <- uploaded$details[uploaded$details$type == "recordings", ]
  expect_identical(sort(unique(recorded$id)), c("100000", "1179094", "694038"))
  expect_identical(recorded$value[recorded$name == "alt"], "30")
  #The annotations of those recordings are uploaded with their own details
  expect_identical(uploaded$annomate$annotation_id, c("86", "87"))
  expect_identical(sort(unique(uploaded$details$id[uploaded$details$type == "annomate"])),
                   c("86", "87"))

  expect_identical(names(uploaded$links), names(getHeaders("links")))
  expect_true(all(uploaded$links$source == "xeno-canto"))
  taxonLinks <- uploaded$links[uploaded$links$object_type == "taxa", ]
  expect_identical(taxonLinks$object_id,
                   c("Troglodytes troglodytes", "Larus fuscus fuscus",
                     "Turdus viscivorus", "Parus major"))

  #The taxa those links name reach a record of their own, taxonomised on the way
  expect_true(all(uploaded$taxa$source == "xeno-canto"))
  expect_true(all(taxonLinks$object_id %in% uploaded$taxa$id))

  #and the sonogram links reach an image record, uploaded from the same harvest
  imageLinks <- uploaded$links[uploaded$links$subject_type == "images", ]
  expect_identical(imageLinks$object_id, "694038")
  expect_true(all(imageLinks$subject_id %in% uploaded$images$id))
  expect_true(all(uploaded$images$source == "xeno-canto"))
  expect_identical(uploaded$taxa[uploaded$taxa$id == "Larus fuscus", "Genus"], "Larus")
})

test_that("ingestR carries on when the xeno-canto harvest fails", {
  expect_warning(
    uploaded <- ingestWithSources(function(query, ...) stop("No xeno-canto API key")),
    "Skipping source xeno-canto - No xeno-canto API key")
  expect_identical(uploaded$recordings$id, "7")
  #A source that gave nothing gives no details either
  expect_null(uploaded$details)
})

test_that("uploadRecordings uploads lat and lon", {
  table <- sourceR("xeno-canto", xenocantoRecordings(xcFixture()$recordings))

  upload <- mockUpload(uploadRecordings, table)

  #All four recordings are inserted by one statement
  expect_length(upload$executed, 1)
  sql <- upload$executed[[1]]$sql
  placeholders <- lengths(regmatches(sql, gregexpr("?", sql, fixed=TRUE)))
  expect_length(upload$executed[[1]]$params, placeholders)
  rows <- boundRows(upload$executed[[1]])
  expect_length(rows, 4)
  expect_identical(rows[[1]][c(2, 16, 17)], list("694038", "42.8373", "-8.652"))
  #Missing coordinates are uploaded as NULL
  expect_identical(rows[[2]][c(16, 17)], list(NA_character_, NA_character_))
  #as are the time of day and device when they aren't known
  expect_identical(rows[[1]][18:21], list(NA_character_, "https://creativecommons.org/licenses/by-nc-sa/4.0/",
                                          "https://xeno-canto.org/694038", NA_character_))
  expect_identical(rows[[4]][c(13, 18)], list(NA_character_, "morning"))
})

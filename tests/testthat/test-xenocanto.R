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
  #xeno-canto names a country rather than coding it, so it is left out
  expect_identical(wren$country, "")
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

  data <- xenocantoR(c("grp:bats", 'grp:"land mammals"'), key="secret", per_page=50, pause=0)

  expect_identical(names(data), names(getHeaders("recordings")))
  expect_identical(data$id, c("1", "2", "3", "4"))
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

  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="legacy", type="recordings", url=csv, process=list()),
      list(name="xeno-canto", type="recordings", xenocanto=list(query="grp:birds"), process="sourceR")),
    xenocantoR=harvest,
    uploadTraits=function(db, table) NULL,
    uploadRecordings=function(db, table) uploaded <<- table)
  ingestR(db="db")
  unlink(csv)
  return(uploaded)
}

test_that("ingestR uploads xeno-canto recordings with other recordings sources", {
  uploaded <- ingestWithSources(function(query, ...) xenocantoRecordings(xcFixture()$recordings))

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

test_that("ingestR carries on when the xeno-canto harvest fails", {
  expect_warning(
    uploaded <- ingestWithSources(function(query, ...) stop("No xeno-canto API key")),
    "Skipping source xeno-canto - No xeno-canto API key")
  expect_identical(uploaded$id, "7")
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

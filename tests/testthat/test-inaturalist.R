inatFixture <- function() {
  path <- test_path("fixtures", "inaturalist-page.json")
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  rjson::fromJSON(body)
}

inatPage <- function(ids, remaining=length(ids)) {
  observations <- lapply(ids, function(id) {
    list(id=id, observed_on="2020-07-01", time_observed_at="2020-07-01T14:00:00+01:00",
         created_at="2020-07-02T09:00:00+01:00", location="51.5,-0.1",
         place_guess="London, England",
         taxon=list(name="Gryllus campestris", preferred_common_name="Field Cricket"),
         user=list(name="A. Recordist", login="arecordist"),
         sounds=list(list(
           id=id * 10, license_code="cc-by-nc",
           file_url=paste0("https://static.inaturalist.org/sounds/", id * 10, ".wav?1"),
           file_content_type="audio/x-wav", hidden=FALSE)))
  })
  rjson::toJSON(list(total_results=remaining, page=1, per_page=length(ids), results=observations))
}

inatResponse <- function(status, body) {
  list(status_code=status, content=charToRaw(enc2utf8(body)))
}

test_that("iNaturalist observations are converted to the recordings format", {
  data <- inaturalistSounds(inatFixture()$results)

  expect_identical(names(data), names(getHeaders("recordings")))
  expect_true(all(vapply(data, is.character, logical(1))))
  #A recording is a sound rather than an observation, so an observation with two
  #of them is two recordings. The All Rights Reserved, taken down, unlicensed
  #and audioless sounds are left out, as is an observation carrying no sound.
  expect_identical(data$id, c("1654351", "1654352", "1270513", "900002", "274020",
                              "151738", "2165535"))

  bushcricket <- data[1, ]
  expect_identical(bushcricket$source, "")
  expect_identical(bushcricket$Title, "iNat317940343 Dark Bush-cricket (Pholidoptera griseoaptera)")
  expect_identical(bushcricket$taxon, "Pholidoptera griseoaptera")
  #The timestamp iNaturalist ends a file URL with is when the file was last
  #processed, so it is left off
  expect_identical(bushcricket$file, "https://static.inaturalist.org/sounds/1654351.wav")
  expect_identical(bushcricket$author, "Klaus Riede")
  expect_identical(bushcricket$post_date, "2025-08-17")
  expect_identical(bushcricket$size, "")
  expect_identical(bushcricket$size_raw, "")
  expect_identical(bushcricket$type, "audio/x-wav")
  expect_identical(bushcricket$NonSpecimen, "")
  expect_identical(bushcricket$Date, "2025-08-17")
  #The clock time is the observer's own, and is not moved to UTC
  expect_identical(bushcricket$Time, "05:00:00")
  #iNaturalist gives a sound no duration, sample rate, channels or size
  expect_identical(bushcricket$Duration, "")
  expect_identical(bushcricket$deployment, "")
  expect_identical(bushcricket$lat, "50.7365434086")
  expect_identical(bushcricket$lon, "7.165596485")
  expect_identical(bushcricket$time_of_day, "")
  expect_identical(bushcricket$license, "https://creativecommons.org/licenses/by-nc/4.0/")
  #A sound's own URL is the audio file, so the recording's page is its
  #observation's
  expect_identical(bushcricket$info_url, "https://www.inaturalist.org/observations/317940343")
  expect_identical(bushcricket$device, "")
  expect_identical(bushcricket$rights_holder, "Klaus Riede")
  #iNaturalist names a place in the observer's own words and codes no country
  expect_identical(bushcricket$country, "")
  expect_identical(bushcricket$locality, "53 Bonn-Beuel, Germany")
  expect_identical(bushcricket$sample_rate, "")
  expect_identical(bushcricket$channels, "")

  #The second sound of the same observation is a recording of its own
  second <- data[2, ]
  expect_identical(second$file, "https://static.inaturalist.org/sounds/1654352.mp3")
  expect_identical(second$type, "audio/mpeg")
  expect_identical(second$Title, bushcricket$Title)
  expect_identical(second$info_url, bushcricket$info_url)

  #Only one of this observation's two sounds is licensed, so the observation
  #passes the API's licence filter but its other sound is still left out
  katydid <- data[3, ]
  expect_identical(katydid$license, "https://creativecommons.org/licenses/by/4.0/")
  #An observer who has given no name is credited by their login
  expect_identical(katydid$author, "rostyslav_yurechko")
  expect_identical(katydid$rights_holder, "rostyslav_yurechko")

  cricket <- data[4, ]
  expect_identical(cricket$id, "900002")
  expect_identical(cricket$taxon, "Gryllus")
  expect_identical(cricket$type, "audio/x-wav")

  cicada <- data[5, ]
  expect_identical(cicada$Date, "")
  expect_identical(cicada$Time, "")
  expect_identical(cicada$license, "https://creativecommons.org/publicdomain/zero/1.0/")

  #A name above genus is what the community identified the observation as, not a
  #placeholder, so it is kept
  unnamed <- data[6, ]
  expect_identical(unnamed$taxon, "Orthoptera")
  expect_identical(unnamed$Title, "iNat65912878 Orthoptera")
  expect_identical(unnamed$Date, "2020-07-01")
  expect_identical(unnamed$Time, "")
  expect_identical(unnamed$author, "Mathieu P\u00e9lissi\u00e9")
  expect_identical(Encoding(unnamed$author), "UTF-8")
  expect_identical(unnamed$locality, "Vall\u00e9e du Rh\u00f4ne, France")
  expect_identical(unnamed$license, "https://creativecommons.org/licenses/by-nc-sa/4.0/")

  #No derivatives is harvested: audioBlast! links to a recording and never
  #copies it, so it never makes a derivative of one
  cricket2 <- data[7, ]
  expect_identical(cricket2$license, "https://creativecommons.org/licenses/by-nd/4.0/")
  #A name given as an empty string is no name, so the login is the credit
  expect_identical(cricket2$author, "carbenoid")
})

test_that("an empty iNaturalist page has no recordings", {
  data <- inaturalistSounds(list())
  expect_identical(names(data), names(getHeaders("recordings")))
  expect_equal(nrow(data), 0)
})

test_that("iNaturalist recordings need no correcting on upload", {
  data <- sourceR("iNaturalist", inaturalistSounds(inatFixture()$results))

  #normaliseRecordings() warns of any value it could not read
  expect_warning(normalised <- normaliseRecordings(data), regexp=NA)
  expect_identical(normalised$id, data$id)
  expect_identical(normalised$Date[1], "2025-08-17")
  expect_identical(normalised$Time[1], "05:00:00")
  expect_identical(normalised$lat[1], "50.7365434086")
  expect_identical(normalised$type[4], "audio/x-wav")
  #What iNaturalist does not hold is uploaded as NULL rather than guessed
  expect_identical(normalised$Date[5], NA_character_)
  expect_identical(normalised$Time[5], NA_character_)
  expect_identical(normalised$country[1], NA_character_)
  expect_identical(normalised$Duration[1], NA_character_)
  expect_identical(normalised$sample_rate[1], NA_character_)
  expect_identical(normalised$channels[1], NA_character_)

  #Normalising recordings that are already normalised leaves them unchanged
  expect_identical(normaliseRecordings(normalised), normalised)
})

test_that("iNaturalist values are normalised", {
  expect_identical(
    inaturalistLicense(c("cc0", "cc-by", "cc-by-sa", "cc-by-nd", "cc-by-nc",
                         "cc-by-nc-sa", "cc-by-nc-nd", "CC-BY", "", "cc-by-nc-xx")),
    c("https://creativecommons.org/publicdomain/zero/1.0/",
      "https://creativecommons.org/licenses/by/4.0/",
      "https://creativecommons.org/licenses/by-sa/4.0/",
      "https://creativecommons.org/licenses/by-nd/4.0/",
      "https://creativecommons.org/licenses/by-nc/4.0/",
      "https://creativecommons.org/licenses/by-nc-sa/4.0/",
      "https://creativecommons.org/licenses/by-nc-nd/4.0/",
      "https://creativecommons.org/licenses/by/4.0/", "", ""))
  expect_identical(
    inaturalistTime(c("2025-08-17T05:00:00+02:00", "2024-07-12T10:33:37-05:00",
                      "2020-07-01T14:00+01:00", "2025-08-17T25:00:00Z",
                      "2025-08-17", "")),
    c("05:00:00", "10:33:37", "14:00", "", "", ""))
  expect_identical(
    inaturalistDate(c("2025-08-17", "2025-08-17T09:12:44+02:00", "0000-00-00", "")),
    c("2025-08-17", "2025-08-17", "", ""))
  expect_identical(
    inaturalistMime(c("audio/x-wav", "audio/wav", "audio/mpeg", "audio/mp4", "")),
    c("audio/x-wav", "audio/x-wav", "audio/mpeg", "audio/mp4", ""))
  expect_identical(
    inaturalistFile(c("https://static.inaturalist.org/sounds/1.wav?1759302174",
                      "https://static.inaturalist.org/sounds/1.wav",
                      "https://static.inaturalist.org/sounds/1.wav?token=abc",
                      "", "not a URL")),
    c("https://static.inaturalist.org/sounds/1.wav",
      "https://static.inaturalist.org/sounds/1.wav",
      "https://static.inaturalist.org/sounds/1.wav?token=abc", "", ""))
  expect_identical(
    inaturalistObserver(c("Klaus Riede", "", ""), c("klaus16", "carbenoid", "")),
    c("Klaus Riede", "carbenoid", ""))
  expect_identical(
    inaturalistTitle(c("1", "2", "3", ""), c("Field Cricket", "", "", ""),
                     c("Gryllus campestris", "Orthoptera", "", "")),
    c("iNat1 Field Cricket (Gryllus campestris)", "iNat2 Orthoptera", "iNat3", ""))
  expect_identical(
    inaturalistPage(c("317940343", "")),
    c("https://www.inaturalist.org/observations/317940343", ""))

  location <- inaturalistCoordinates(c("51.5,-0.1", "-90,180", "51.5", "", "here"))
  expect_identical(location$lat, c("51.5", "-90", "", "", ""))
  expect_identical(location$lon, c("-0.1", "180", "", "", ""))
  expect_identical(coordinate(location$lat, 90), c("51.5", "-90", NA, NA, NA))

  #Ids are larger than an R integer holds, so they must not become 4e+08
  expect_identical(
    inaturalistLastID(list(list(id=1001), list(id=401842697), list(id=1002))),
    "401842697")
  expect_error(inaturalistLastID(list(list(uuid="no-id-here"))), "no id")
})

test_that("iNaturalist harvests page through every taxon with a sliding window", {
  urls <- character(0)
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    urls <<- c(urls, url)
    above <- as.numeric(sub(".*[?&]id_above=([0-9]+).*", "\\1", url))
    if (grepl("taxon_id=47651", url, fixed=TRUE)) {
      #A full page is followed by another; a short page ends the taxon
      if (above == 0) return(inatResponse(200, inatPage(c(1001, 1002), 3)))
      return(inatResponse(200, inatPage(1003, 1)))
    }
    inatResponse(200, inatPage(2001, 1))
  })

  data <- inaturalistR(c("47651", "50186"), per_page=2, pause=0)

  expect_identical(names(data), names(getHeaders("recordings")))
  expect_identical(data$id, c("10010", "10020", "10030", "20010"))
  expect_length(urls, 3)
  expect_match(urls[1], "&taxon_id=47651&", fixed=TRUE)
  expect_match(urls[1], "&quality_grade=research&", fixed=TRUE)
  #No derivatives is asked for; All Rights Reserved is not
  expect_match(urls[1], "&sound_license=cc0,cc-by,cc-by-sa,cc-by-nd,cc-by-nc,cc-by-nc-sa,cc-by-nc-nd&",
               fixed=TRUE)
  expect_match(urls[1], "&order_by=id&order=asc&id_above=0&per_page=2&", fixed=TRUE)
  #The next request asks for the observations above the last id of this one
  expect_match(urls[2], "&id_above=1002&", fixed=TRUE)
  expect_match(urls[3], "&taxon_id=50186&", fixed=TRUE)
})

test_that("an observation harvested under two taxa is one recording", {
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    #Orthoptera is within Insecta, so both harvests hold this observation
    inatResponse(200, inatPage(1001, 1))
  })

  data <- inaturalistR(c("47651", "47158"), per_page=2, pause=0)

  expect_identical(data$id, "10010")
})

test_that("iNaturalist harvests check their arguments", {
  expect_error(inaturalistR(character(0)), "taxon_id")
  expect_error(inaturalistR("Orthoptera"), "taxon_id")
  expect_error(inaturalistR("47651", per_page=500), "per_page")
  expect_error(inaturalistR("47651", quality_grade="Research Grade"), "quality_grade")
})

test_that("failed iNaturalist requests are retried", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    calls <<- calls + 1
    if (calls == 1) stop("Timeout was reached")
    #Being asked to slow down is worth retrying, unlike other client errors
    if (calls == 2) return(inatResponse(429, '{"errors":[{"message":"Too Many Requests"}]}'))
    inatResponse(200, inatPage(1001))
  })

  json <- inaturalistFetch("47651", "research", "0", 200, NULL, backoff=c(0, 0))

  expect_equal(calls, 3)
  expect_equal(json$results[[1]]$id, 1001)
})

test_that("iNaturalist requests give up after the last retry", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    calls <<- calls + 1
    inatResponse(503, "<html>Service Unavailable</html>")
  })

  expect_error(
    inaturalistFetch("47651", "research", "1002", 200, NULL, backoff=c(0, 0)),
    "iNaturalist request for taxon '47651' above id 1002 failed: unexpected response (HTTP 503)",
    fixed=TRUE)
  expect_equal(calls, 3)
})

test_that("iNaturalist client errors are reported without retrying", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    calls <<- calls + 1
    if (grepl("taxon_id=47651", url, fixed=TRUE)) {
      #Version 2 of the API reports errors in a list, version 1 in a string
      return(inatResponse(422, '{"status":"422","errors":[{"errorCode":"422","message":"Invalid taxon"}]}'))
    }
    inatResponse(403, '{"error":"Result window is too large","status":403}')
  })

  expect_error(
    inaturalistFetch("47651", "research", "0", 200, NULL, backoff=c(0, 0)),
    "Invalid taxon (HTTP 422)", fixed=TRUE)
  expect_error(
    inaturalistFetch("50186", "research", "0", 200, NULL, backoff=c(0, 0)),
    "Result window is too large (HTTP 403)", fixed=TRUE)
  expect_equal(calls, 2)
})

test_that("the iNaturalist source module is read from list_sources", {
  #JSON as served for modules/inaturalist/module.php
  json <- paste0('{"data":{"iNaturalist":[',
                 '{"type":"recordings","inaturalist":{"taxon_id":["47651"]},"process":["sourceR"]},',
                 '{"type":"recordings","inaturalist":{"taxon_id":["50186"]},"process":["sourceR"]}]}}')
  local_mocked_bindings(fromJSON=function(...) rjson::fromJSON(json))

  sources <- getSources()

  expect_length(sources, 2)
  expect_identical(sources[[1]]$name, "iNaturalist")
  expect_identical(sources[[1]]$process, "sourceR")
  expect_identical(
    sort(c(sources[[1]]$inaturalist$taxon_id, sources[[2]]$inaturalist$taxon_id)),
    c("47651", "50186"))
})

test_that("ingestR uploads iNaturalist recordings, and a failed taxon skips only itself", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="iNaturalist", type="recordings",
           inaturalist=list(taxon_id="47651"), process="sourceR"),
      list(name="iNaturalist", type="recordings",
           inaturalist=list(taxon_id="50186"), process="sourceR")),
    inaturalistR=function(taxon_id, ...) {
      if (taxon_id == "50186") stop("nothing came back")
      inaturalistSounds(inatFixture()$results)
    },
    uploadTraits=function(db, table) NULL,
    uploadRecordings=function(db, table) uploaded <<- table)

  expect_warning(ingestR(db="db"), "Skipping source iNaturalist - nothing came back")

  expect_identical(names(uploaded), names(getHeaders("recordings")))
  expect_identical(uploaded$source, rep("iNaturalist", 7))
  expect_identical(uploaded$id[1], "1654351")
})

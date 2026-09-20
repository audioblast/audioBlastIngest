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
         taxon=list(id=9001, name="Gryllus campestris", rank="species", parent_id=9000,
                    ancestor_ids=list(9000, 9001), preferred_common_name="Field Cricket"),
         user=list(name="A. Recordist", login="arecordist"),
         sounds=list(list(
           id=id * 10, license_code="cc-by-nc",
           file_url=paste0("https://static.inaturalist.org/sounds/", id * 10, ".wav?1"),
           file_content_type="audio/x-wav", hidden=FALSE)))
  })
  rjson::toJSON(list(total_results=remaining, page=1, per_page=length(ids), results=observations))
}

#The taxa above the one the stub observations are of, as version 1 gives them
inatTaxaPage <- function(ids) {
  results <- lapply(ids, function(id) {
    list(id=as.numeric(id), name="Gryllus", rank="genus", parent_id=47651)
  })
  rjson::toJSON(list(total_results=length(ids), page=1, per_page=30, results=results))
}

inatResponse <- function(status, body) {
  list(status_code=status, content=charToRaw(enc2utf8(body)))
}

#The stub API: observations from version 2, and the taxa above them from
#version 1, which is the one request a harvest makes of it
inatAPI <- function(observations) {
  function(url, handle) {
    if (grepl("/v1/taxa/", url, fixed=TRUE)) {
      return(inatResponse(200, inatTaxaPage(strsplit(sub(".*/v1/taxa/", "", url), ",")[[1]])))
    }
    return(observations(url))
  }
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
  expect_equal(nrow(attr(data, "links")), 0)
  expect_identical(names(inaturalistTaxa(list())), names(getHeaders("taxa")))
})

test_that("a page says which recording is about which taxon", {
  data <- inaturalistSounds(inatFixture()$results)
  links <- attr(data, "links")

  expect_identical(names(links), names(getHeaders("links")))
  #A link for every recording that was kept, and none for the sounds left out
  expect_identical(links$subject_id, data$id)
  expect_true(all(links$predicate == "http://purl.obolibrary.org/obo/IAO_0000136"))
  expect_true(all(links$subject_type == "recordings"))
  expect_true(all(links$object_type == "taxa"))
  #Both sounds of one observation are about its taxon
  expect_identical(links$object_id[1:2], c("123456", "123456"))
  #The ends are the linking source's own, which uploadLinks() fills in
  expect_true(all(links$subject_source == "" & links$object_source == ""))
})

test_that("iNaturalist taxa are converted to the taxa format", {
  taxa <- inaturalistTaxa(lapply(inatFixture()$results, `[[`, "taxon"))

  expect_identical(names(taxa), names(getHeaders("taxa")))
  #One row for each taxon, however many observations named it
  expect_identical(taxa$id, c("123456", "322222", "205461", "47936", "424321",
                              "47651", "332307", "50186"))
  expect_identical(taxa$taxon[1], "Pholidoptera griseoaptera")
  #Ranks are capitalised, as taxonomiseR() names a column after each and the
  #taxa table's columns are capitalised
  expect_identical(taxa$Rank, c("Species", "Species", "Species", "Genus", "Species",
                                "Order", "Species", "Family"))
  expect_identical(taxa$parent_id[1], "123400")
  expect_identical(taxa$source[1], "")

  #A taxon with no id or no name is not a taxon this can hold
  expect_equal(nrow(inaturalistTaxa(list(list(name="Nameless"), list(id=1), list()))), 0)
})

test_that("the taxa above a taxon are the ones its classification needs", {
  above <- inaturalistAncestorIDs(lapply(inatFixture()$results, `[[`, "taxon"))

  expect_identical(above[["123456"]],
                   c("48460", "1", "47120", "47158", "184884", "47651", "123400", "123456"))
  #A taxon named twice is listed once
  expect_equal(sum(names(above) == "50186"), 1)
  expect_identical(inaturalistAncestorIDs(list(list(id=1))), setNames(list(character(0)), "1"))
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
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    urls <<- c(urls, url)
    above <- as.numeric(sub(".*[?&]id_above=([0-9]+).*", "\\1", url))
    if (grepl("taxon_id=47651", url, fixed=TRUE)) {
      #A full page is followed by another; a short page ends the taxon
      if (above == 0) return(inatResponse(200, inatPage(c(1001, 1002), 3)))
      return(inatResponse(200, inatPage(1003, 1)))
    }
    inatResponse(200, inatPage(2001, 1))
  }))

  harvest <- inaturalistR(c("47651", "50186"), per_page=2, pause=0)
  data <- harvest$recordings

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
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    #Orthoptera is within Insecta, so both harvests hold this observation
    inatResponse(200, inatPage(1001, 1))
  }))

  harvest <- inaturalistR(c("47651", "47158"), per_page=2, pause=0)

  expect_identical(harvest$recordings$id, "10010")
  #and one link to the taxon, not one for each harvest it was found in
  expect_equal(nrow(harvest$links), 1)
})

test_that("a sound on two observations is one recording about two taxa", {
  #A recording with two taxa singing in it, entered once for each of them
  page <- function(observation, taxon, name) {
    rjson::toJSON(list(total_results=1, page=1, per_page=200, results=list(list(
      id=observation, observed_on="2020-07-01", time_observed_at="2020-07-01T14:00:00+01:00",
      created_at="2020-07-02T09:00:00+01:00", location="51.5,-0.1", place_guess="London",
      taxon=list(id=taxon, name=name, rank="species", parent_id=9000,
                 ancestor_ids=list(9000, taxon), preferred_common_name=name),
      user=list(name="A. Recordist", login="arecordist"),
      sounds=list(list(id=136818, license_code="cc-by-nc",
                       file_url="https://static.inaturalist.org/sounds/136818.wav?1",
                       file_content_type="audio/x-wav", hidden=FALSE))))))
  }
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    if (grepl("taxon_id=1", url, fixed=TRUE)) {
      return(inatResponse(200, page(59940239, 153455, "Oecanthus rileyi")))
    }
    inatResponse(200, page(59947747, 226222, "Oecanthus quadripunctatus"))
  }))

  harvest <- inaturalistR(c("1", "2"), per_page=200, pause=0)

  #One file is one recording, keeping the first observation it was found on
  expect_identical(harvest$recordings$id, "136818")
  expect_identical(harvest$recordings$taxon, "Oecanthus rileyi")
  #but it is about both taxa, which is what the links are for
  expect_equal(nrow(harvest$links), 2)
  expect_identical(harvest$links$subject_id, c("136818", "136818"))
  expect_identical(harvest$links$object_id, c("153455", "226222"))
  expect_identical(sort(harvest$taxa$taxon[harvest$taxa$Rank == "Species"]),
                   c("Oecanthus quadripunctatus", "Oecanthus rileyi"))
})

test_that("a harvest gives the taxa its recordings are of, and the taxa above them", {
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    inatResponse(200, inatPage(1001, 1))
  }))

  harvest <- inaturalistR("47651", per_page=2, pause=0)

  expect_identical(names(harvest), c("recordings", "taxa", "links"))
  expect_identical(names(harvest$taxa), names(getHeaders("taxa")))
  #The taxon the observation was identified as, and the one above it, which is
  #fetched because taxonomiseR() walks the classification by following parents
  expect_identical(harvest$taxa$id, c("9001", "9000"))
  expect_identical(harvest$taxa$taxon, c("Gryllus campestris", "Gryllus"))
  #A rank is capitalised, as the taxa table's columns are
  expect_identical(harvest$taxa$Rank, c("Species", "Genus"))
  expect_identical(harvest$taxa$parent_id, c("9000", "47651"))
  expect_identical(harvest$taxa$source, c("", ""))

  expect_identical(names(harvest$links), names(getHeaders("links")))
  expect_identical(harvest$links$subject_type, "recordings")
  expect_identical(harvest$links$subject_id, "10010")
  #A recording is about a taxon; it does not identify one
  expect_identical(harvest$links$predicate, "http://purl.obolibrary.org/obo/IAO_0000136")
  expect_identical(harvest$links$object_type, "taxa")
  expect_identical(harvest$links$object_id, "9001")
})

test_that("a sound on two observations of one page is one recording", {
  #The same sound on two observations that land on the same page, which the
  #page after cannot catch
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    sound <- function(observation, taxon, name) list(
      id=observation, observed_on="2020-07-01",
      time_observed_at="2020-07-01T14:00:00+01:00",
      created_at="2020-07-02T09:00:00+01:00", location="51.5,-0.1",
      place_guess="London",
      taxon=list(id=taxon, name=name, rank="species", parent_id=9000,
                 ancestor_ids=list(9000, taxon), preferred_common_name=name),
      user=list(name="A. Recordist", login="arecordist"),
      sounds=list(list(id=136818, license_code="cc-by-nc",
                       file_url="https://static.inaturalist.org/sounds/136818.wav?1",
                       file_content_type="audio/x-wav", hidden=FALSE)))
    inatResponse(200, rjson::toJSON(list(total_results=2, page=1, per_page=200,
      results=list(sound(59940239, 153455, "Oecanthus rileyi"),
                   sound(59947747, 226222, "Oecanthus quadripunctatus")))))
  }))

  harvest <- inaturalistR("47651", per_page=200, pause=0)

  expect_identical(harvest$recordings$id, "136818")
  expect_identical(harvest$links$object_id, c("153455", "226222"))
})

test_that("a harvest given a directory streams to it instead of holding it", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    above <- as.numeric(sub(".*[?&]id_above=([0-9]+).*", "\\1", url))
    if (above == 0) return(inatResponse(200, inatPage(c(1001, 1002), 3)))
    inatResponse(200, inatPage(1003, 1))
  }))

  paths <- inaturalistR("47651", per_page=2, pause=0, dir=dir)

  expect_identical(names(paths), c("recordings", "taxa", "links"))
  expect_true(all(vapply(paths, is.character, logical(1))))

  read <- list()
  for (type in names(paths)) {
    readStream(paths[[type]], -1L, function(chunk) read[[type]] <<- chunk)
  }
  #The same tables that holding the harvest in memory would have given
  expect_identical(read$recordings$id, c("10010", "10020", "10030"))
  expect_identical(names(read$recordings), names(getHeaders("recordings")))
  expect_identical(read$links$subject_id, c("10010", "10020", "10030"))
  #A taxon every page names is written once rather than once a page, and the
  #taxon above it after the last page, when the harvest knows it needs it
  expect_identical(read$taxa$id, c("9001", "9000"))
  expect_identical(names(read$taxa), names(getHeaders("taxa")))
})

test_that("an interrupted harvest is taken up above the id it reached", {
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    above <- as.numeric(sub(".*[?&]id_above=([0-9]+).*", "\\1", url))
    if (above < 1002) return(inatResponse(200, inatPage(c(1001, 1002), 3)))
    inatResponse(200, inatPage(1003, 1))
  }))

  #A harvest says the id each page reached, which is what to resume above
  expect_output(inaturalistR("47651", per_page=2, pause=0, verbose=TRUE),
                "resume above 1002")

  resumed <- inaturalistR("47651", per_page=2, pause=0, id_above="1002")

  #What the first harvest would have gone on to give, and none of what it gave
  expect_identical(resumed$recordings$id, "10030")
  expect_identical(resumed$links$subject_id, "10030")
  #A resumed harvest fetches the taxa it needs itself
  expect_identical(resumed$taxa$id, c("9001", "9000"))
})

test_that("iNaturalist harvests check their arguments", {
  expect_error(inaturalistR(character(0)), "taxon_id")
  expect_error(inaturalistR("Orthoptera"), "taxon_id")
  expect_error(inaturalistR("47651", per_page=500), "per_page")
  expect_error(inaturalistR("47651", quality_grade="Research Grade"), "quality_grade")
  expect_error(inaturalistR("47651", id_above="the last one"), "id_above")
  #Each taxon is paged from its own place, so several cannot share a cursor
  expect_error(inaturalistR(c("47651", "50186"), id_above="1002"),
               "one taxon can be harvested above an id")
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
  uploadedTaxa <- NULL
  uploadedLinks <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="iNaturalist", type="recordings",
           inaturalist=list(taxon_id="47651"), process="sourceR"),
      list(name="iNaturalist", type="recordings",
           inaturalist=list(taxon_id="50186"), process="sourceR")),
    inaturalistR=function(taxon_id, ...) {
      if (taxon_id == "50186") stop("nothing came back")
      observations <- inatFixture()$results
      recordings <- inaturalistSounds(observations)
      links <- attr(recordings, "links")
      taxa <- inaturalistTaxa(lapply(observations, `[[`, "taxon"))
      taxa <- taxa[taxa$id %in% links$object_id, ]
      #The taxa above them, as inaturalistTaxaByID() would have fetched them
      above <- inaturalistTaxa(list(
        list(id=48460, name="Life", rank="stateofmatter"),
        list(id=1, name="Animalia", rank="kingdom", parent_id=48460),
        list(id=47120, name="Arthropoda", rank="phylum", parent_id=1),
        list(id=47158, name="Insecta", rank="class", parent_id=47120),
        list(id=184884, name="Pterygota", rank="subclass", parent_id=47158),
        list(id=47651, name="Orthoptera", rank="order", parent_id=184884),
        list(id=50186, name="Cicadidae", rank="family", parent_id=47158),
        list(id=123400, name="Pholidoptera", rank="genus", parent_id=47651)))
      taxa <- rbind(taxa, above[!above$id %in% taxa$id, ])
      list(recordings=recordings, taxa=taxa, links=links)
    },
    uploadTraits=function(db, table) NULL,
    uploadTaxa=function(db, table) uploadedTaxa <<- table,
    uploadLinks=function(db, table) uploadedLinks <<- table,
    uploadRecordings=function(db, table) uploaded <<- table)

  expect_warning(ingestR(db="db"), "Skipping source iNaturalist - nothing came back")

  expect_identical(names(uploaded), names(getHeaders("recordings")))
  expect_identical(uploaded$source, rep("iNaturalist", 7))
  expect_identical(uploaded$id[1], "1654351")

  #One harvest fills three tables, and every one of them is named as
  #iNaturalist's by the source's own sourceR process
  expect_identical(uploadedLinks$source, rep("iNaturalist", 7))
  expect_identical(uploadedLinks$subject_id[1:2], c("1654351", "1654352"))
  #Both sounds of one observation are about the same taxon
  expect_identical(uploadedLinks$object_id[1:2], c("123456", "123456"))
  expect_true(all(uploadedTaxa$source == "iNaturalist"))
  #taxonomiseR() has walked the classification, so a taxon names itself and
  #everything above it
  bushcricket <- uploadedTaxa[uploadedTaxa$id == "123456", ]
  expect_identical(bushcricket$Species, "Pholidoptera griseoaptera")
  expect_identical(bushcricket$Order, "Orthoptera")
  expect_identical(bushcricket$Class, "Insecta")
  expect_identical(bushcricket$Kingdom, "Animalia")
})

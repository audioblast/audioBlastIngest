inatFixture <- function() {
  path <- test_path("fixtures", "inaturalist-page.json")
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  rjson::fromJSON(body)
}

#The recordings of the fixture, one of whose sounds has a licence code that is
#not known here, which is warned of
inatFixtureSounds <- function() {
  expect_warning(inaturalistSounds(inatFixture()$results), "licence that could not be read")
}

inatPage <- function(ids, remaining=length(ids)) {
  observations <- lapply(ids, function(id) {
    list(id=id, observed_on="2020-07-01", time_observed_at="2020-07-01T14:00:00+01:00",
         created_at="2020-07-02T09:00:00+01:00", location="51.5,-0.1",
         place_guess="London, England", obscured=FALSE, taxon_geoprivacy="open",
         quality_grade="research",
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
  data <- inatFixtureSounds()

  expect_identical(names(data), names(getHeaders("recordings")))
  expect_true(all(vapply(data, is.character, logical(1))))
  #A recording is a sound rather than an observation, so an observation with two
  #of them is two recordings. The taken down and audioless sounds are left out,
  #as is an observation carrying no sound. All Rights Reserved sounds, and one
  #whose licence code is not known here, are kept with no licence.
  expect_identical(data$id, c("1654351", "1654352", "1129762", "1129763", "1270513",
                              "1270514", "900002", "274020", "151738", "2165535",
                              "500001"))

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
  expect_identical(bushcricket$license, "https://creativecommons.org/licenses/by-nc/")
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

  #A sound that is All Rights Reserved has no licence, which is what iNaturalist
  #gives, and is otherwise a recording like any other
  reserved <- data[3, ]
  expect_identical(reserved$license, "")
  expect_identical(reserved$file, "https://static.inaturalist.org/sounds/1129762.m4a")
  expect_identical(data$license[4], "")

  #Only one of this observation's two sounds is licensed. The other is All
  #Rights Reserved, and is kept with no licence.
  katydid <- data[5, ]
  expect_identical(katydid$license, "https://creativecommons.org/licenses/by/")
  expect_identical(data$license[6], "")
  #An observer who has given no name is credited by their login
  expect_identical(katydid$author, "rostyslav_yurechko")
  expect_identical(katydid$rights_holder, "rostyslav_yurechko")

  cricket <- data[7, ]
  expect_identical(cricket$id, "900002")
  expect_identical(cricket$taxon, "Gryllus")
  expect_identical(cricket$type, "audio/x-wav")
  #iNaturalist gives an ampersand in a name HTML-escaped, and can give a place
  #so too, and they are decoded. An apostrophe in a name is given as it is.
  expect_identical(cricket$author, "A. Recordist & B. O'Recordist")
  expect_identical(cricket$rights_holder, "A. Recordist & B. O'Recordist")
  expect_identical(cricket$locality, "Monts d'Or, Lyon, France")

  cicada <- data[8, ]
  expect_identical(cicada$Date, "")
  expect_identical(cicada$Time, "")
  expect_identical(cicada$license, "https://creativecommons.org/publicdomain/zero/1.0/")

  #A name above genus is what the community identified the observation as, not a
  #placeholder, so it is kept
  unnamed <- data[9, ]
  expect_identical(unnamed$taxon, "Orthoptera")
  expect_identical(unnamed$Title, "iNat65912878 Orthoptera")
  expect_identical(unnamed$Date, "2020-07-01")
  expect_identical(unnamed$Time, "")
  expect_identical(unnamed$author, "Mathieu P\u00e9lissi\u00e9")
  expect_identical(Encoding(unnamed$author), "UTF-8")
  expect_identical(unnamed$locality, "Vall\u00e9e du Rh\u00f4ne, France")
  expect_identical(unnamed$license, "https://creativecommons.org/licenses/by-nc-sa/")

  #No derivatives is harvested: audioBlast! links to a recording and never
  #copies it, so it never makes a derivative of one
  cricket2 <- data[10, ]
  expect_identical(cricket2$license, "https://creativecommons.org/licenses/by-nd/")
  #A name given as an empty string is no name, so the login is the credit
  expect_identical(cricket2$author, "carbenoid")

  #A licence code that is not known here is warned of, and the sound is kept
  #with no licence rather than with a licence guessed at
  expect_identical(data[11, "license"], "")
  #What looks like a tag in a place is text, and is kept
  expect_identical(data[11, "locality"], "Puno, <Null>, PE-PU, PE")
})

test_that("an empty iNaturalist page has no recordings", {
  data <- inaturalistSounds(list())
  expect_identical(names(data), names(getHeaders("recordings")))
  expect_equal(nrow(data), 0)
  expect_equal(nrow(attr(data, "links")), 0)
  details <- inaturalistDetails(data, attr(data, "observed"))
  expect_identical(names(details), names(getHeaders("details")))
  expect_equal(nrow(details), 0)
  expect_identical(names(inaturalistTaxa(list())), names(getHeaders("taxa")))
})

test_that("each recording has the details of its observation", {
  data <- inatFixtureSounds()
  observed <- attr(data, "observed")
  #A row of its observation's values for each recording that was kept
  expect_equal(nrow(observed), nrow(data))

  details <- inaturalistDetails(data, observed)
  expect_identical(names(details), names(getHeaders("details")))
  expect_true(all(details$type == "recordings"))
  expect_true(all(details$source == ""))
  detail <- function(name) details[details$name == name, c("id", "value", "unit")]

  #Obscured by the observer (both recordings of the Missouri cicadas and of the
  #katydid), for the taxon (the cricket), and kept private by the observer (the
  #Algarve cicada). An open location says nothing, and the cricket's sound that
  #was taken down, which is no recording, has no details either.
  expect_identical(detail("obscured")$id,
                   c("1129762", "1129763", "1270513", "1270514", "900002", "274020"))
  expect_true(all(detail("obscured")$value == "true"))
  expect_identical(detail("geoprivacy")$id, c("1129762", "1129763", "1270513", "1270514", "274020"))
  expect_identical(detail("geoprivacy")$value,
                   c("obscured", "obscured", "obscured", "obscured", "private"))
  expect_identical(detail("taxon_geoprivacy")$id, "900002")
  expect_identical(detail("taxon_geoprivacy")$value, "obscured")

  #The uncertainty of the coordinates iNaturalist gives, in metres: both of the
  #bush-cricket's recordings, whose location is open, and the obscured ones.
  #iNaturalist gives the private cicada one too, but it has no coordinates for
  #it to be the accuracy of.
  accuracy <- detail("public_positional_accuracy")
  expect_identical(accuracy$id,
                   c("1654351", "1654352", "1129762", "1129763", "1270513", "1270514", "900002"))
  expect_identical(accuracy$value, c("12", "12", "28121", "28121", "29433", "29433", "29656"))
  expect_true(all(accuracy$unit == "m"))

  #The grade of every recording iNaturalist gives one, so that the selection a
  #harvest makes is said on each of them. The last observation has none.
  grade <- detail("quality_grade")
  expect_identical(grade$id, data$id[1:10])
  expect_identical(grade$value, c(rep("research", 8), "needs_id", "research"))
  expect_true(all(grade$unit == ""))

  #A private location has no coordinates to give
  expect_identical(data$lat[data$id == "274020"], NA_character_)
  expect_identical(data$lon[data$id == "274020"], NA_character_)

  #Nor does a location given empty or that could not be read, as in a table read
  #back from a stream
  located <- data.frame(id=c("1", "2", "3"), lat=c("51.5", "", NA), lon=c("-0.1", "", NA),
                        stringsAsFactors=FALSE)
  observed <- data.frame(obscured="", geoprivacy="", taxon_geoprivacy="",
                         public_positional_accuracy=c("10", "10", "10"), quality_grade="",
                         stringsAsFactors=FALSE)
  expect_identical(inaturalistDetails(located, observed)$id, "1")
})

test_that("iNaturalist details need no correcting on upload", {
  data <- inatFixtureSounds()
  details <- sourceR("iNaturalist", inaturalistDetails(data, attr(data, "observed")))

  #normaliseDetails() warns of any detail it had to leave out
  expect_warning(normalised <- normaliseDetails(details), regexp=NA)
  expect_identical(normalised, details)
})

test_that("a page says which recording is about which taxon", {
  data <- inatFixtureSounds()
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
  data <- sourceR("iNaturalist", inatFixtureSounds())

  #normaliseRecordings() warns of any value it could not read
  expect_warning(normalised <- normaliseRecordings(data), regexp=NA)
  expect_identical(normalised$id, data$id)
  expect_identical(normalised$Date[1], "2025-08-17")
  expect_identical(normalised$Time[1], "05:00:00")
  expect_identical(normalised$lat[1], "50.7365434086")
  expect_identical(normalised$type[7], "audio/x-wav")
  #What iNaturalist does not hold is uploaded as NULL rather than guessed
  expect_identical(normalised$Date[8], NA_character_)
  expect_identical(normalised$Time[8], NA_character_)
  expect_identical(normalised$country[1], NA_character_)
  expect_identical(normalised$Duration[1], NA_character_)
  expect_identical(normalised$sample_rate[1], NA_character_)
  expect_identical(normalised$channels[1], NA_character_)

  #Normalising recordings that are already normalised leaves them unchanged
  expect_identical(normaliseRecordings(normalised), normalised)
})

test_that("iNaturalist values are normalised", {
  #iNaturalist names a licence but not its version, so no version is added,
  #except to CC0 and the Public Domain Mark (pd), which have only ever had one
  expect_identical(
    inaturalistLicense(c("cc0", "pd", "cc-by", "cc-by-sa", "cc-by-nd", "cc-by-nc",
                         "cc-by-nc-sa", "cc-by-nc-nd", "CC-BY", "PD", "")),
    c("https://creativecommons.org/publicdomain/zero/1.0/",
      "https://creativecommons.org/publicdomain/mark/1.0/",
      "https://creativecommons.org/licenses/by/",
      "https://creativecommons.org/licenses/by-sa/",
      "https://creativecommons.org/licenses/by-nd/",
      "https://creativecommons.org/licenses/by-nc/",
      "https://creativecommons.org/licenses/by-nc-sa/",
      "https://creativecommons.org/licenses/by-nc-nd/",
      "https://creativecommons.org/licenses/by/",
      "https://creativecommons.org/publicdomain/mark/1.0/", ""))
  #All Rights Reserved, which iNaturalist gives as no licence, is not warned of,
  #but a code that is not known here is
  expect_warning(inaturalistLicense(""), regexp=NA)
  expect_warning(expect_identical(inaturalistLicense("cc-by-nc-xx"), ""),
                 "licence that could not be read")
  #A licence URL with no version is still a licence URL, so normalising it
  #leaves it as it is
  expect_identical(httpURL(inaturalistLicense("cc-by-nc")), "https://creativecommons.org/licenses/by-nc/")
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
    inaturalistObserver(c("Klaus Riede", "", "", "Ann &amp; Bill Recordist"),
                        c("klaus16", "carbenoid", "", "annbill")),
    c("Klaus Riede", "carbenoid", "", "Ann & Bill Recordist"))
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
  #No licence is asked for, so All Rights Reserved sounds are harvested too
  expect_false(grepl("sound_license=", urls[1], fixed=TRUE))
  expect_match(urls[1], "&order_by=id&order=asc&id_above=0&per_page=2&", fixed=TRUE)
  #Whether a location is obscured, and the grade, come in the same request
  expect_match(urls[1], "obscured:!t,geoprivacy:!t,taxon_geoprivacy:!t,public_positional_accuracy:!t,",
               fixed=TRUE)
  expect_match(urls[1], "quality_grade:!t,", fixed=TRUE)
  #The next request asks for the observations above the last id of this one
  expect_match(urls[2], "&id_above=1002&", fixed=TRUE)
  expect_match(urls[3], "&taxon_id=50186&", fixed=TRUE)

  #A grade for each recording, and nothing else for an open location with no
  #accuracy
  expect_identical(names(harvest$details), names(getHeaders("details")))
  expect_identical(harvest$details$id, c("10010", "10020", "10030", "20010"))
  expect_true(all(harvest$details$name == "quality_grade"))
  expect_true(all(harvest$details$value == "research"))
})

test_that("every taxon is the taxon_id left out rather than given empty", {
  urls <- character(0)
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    urls <<- c(urls, url)
    inatResponse(200, inatPage(1001, 1))
  }))

  harvest <- inaturalistR("", per_page=200, pause=0)

  #Given empty, the API reads the taxon as 0 and refuses the request
  expect_false(grepl("taxon_id", urls[1], fixed=TRUE))
  expect_match(urls[1], "?sounds=true&quality_grade=research&", fixed=TRUE)
  expect_identical(harvest$recordings$id, "10010")
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
  #and its details once
  expect_identical(harvest$details$id, "10010")
})

test_that("a sound on two observations is one recording about two taxa", {
  #A recording with two taxa singing in it, entered once for each of them. The
  #second observer obscured their location and the first did not.
  page <- function(observation, taxon, name) {
    hidden <- observation == 59947747
    rjson::toJSON(list(total_results=1, page=1, per_page=200, results=list(list(
      id=observation, observed_on="2020-07-01", time_observed_at="2020-07-01T14:00:00+01:00",
      created_at="2020-07-02T09:00:00+01:00", location="51.5,-0.1", place_guess="London",
      obscured=hidden, geoprivacy=if (hidden) "obscured" else "open",
      quality_grade="research",
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
  #Its details are of the observation its columns were read from, so the
  #second observation's obscuring is not said of a location it did not give
  expect_identical(harvest$details$id, "136818")
  expect_identical(harvest$details$name, "quality_grade")
})

test_that("a harvest gives the taxa its recordings are of, and the taxa above them", {
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    inatResponse(200, inatPage(1001, 1))
  }))

  harvest <- inaturalistR("47651", per_page=2, pause=0)

  expect_identical(names(harvest), c("recordings", "details", "taxa", "links"))
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
  #page after cannot catch. The second is obscured for its taxon.
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    sound <- function(observation, taxon, name) list(
      id=observation, observed_on="2020-07-01",
      time_observed_at="2020-07-01T14:00:00+01:00",
      created_at="2020-07-02T09:00:00+01:00", location="51.5,-0.1",
      place_guess="London", obscured=taxon == 226222,
      taxon_geoprivacy=if (taxon == 226222) "obscured" else "open",
      public_positional_accuracy=if (taxon == 226222) 29656 else 10,
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
  #The details of the first observation, whose columns the recording has, and
  #none of the second's
  expect_identical(harvest$details$name, "public_positional_accuracy")
  expect_identical(harvest$details$value, "10")
})

test_that("a harvest given a directory streams to it instead of holding it", {
  dir <- withr::local_tempdir()
  local_mocked_bindings(curl_fetch_memory=inatAPI(function(url) {
    above <- as.numeric(sub(".*[?&]id_above=([0-9]+).*", "\\1", url))
    if (above == 0) return(inatResponse(200, inatPage(c(1001, 1002), 3)))
    inatResponse(200, inatPage(1003, 1))
  }))

  paths <- inaturalistR("47651", per_page=2, pause=0, dir=dir)

  expect_identical(names(paths), c("recordings", "details", "taxa", "links"))
  expect_true(all(vapply(paths, is.character, logical(1))))

  read <- list()
  for (type in names(paths)) {
    readStream(paths[[type]], -1L, function(chunk) read[[type]] <<- chunk)
  }
  #The same tables that holding the harvest in memory would have given
  expect_identical(read$recordings$id, c("10010", "10020", "10030"))
  expect_identical(names(read$recordings), names(getHeaders("recordings")))
  expect_identical(read$links$subject_id, c("10010", "10020", "10030"))
  expect_identical(read$details$id, c("10010", "10020", "10030"))
  expect_identical(names(read$details), names(getHeaders("details")))
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
  #JSON as served for modules/inaturalist/module.php: one source, of every taxon
  json <- paste0('{"data":{"iNaturalist":[',
                 '{"type":"recordings","inaturalist":{"taxon_id":[""]},"process":["sourceR"]}]}}')
  local_mocked_bindings(fromJSON=function(...) rjson::fromJSON(json))

  sources <- getSources()

  expect_length(sources, 1)
  expect_identical(sources[[1]]$name, "iNaturalist")
  expect_identical(sources[[1]]$process, "sourceR")
  expect_identical(sources[[1]]$inaturalist$taxon_id, "")
})

inatSource <- function() {
  list(list(name="iNaturalist", type="recordings",
            inaturalist=list(taxon_id=""), process="sourceR"))
}

test_that("ingestR streams a harvest of every taxon and uploads it from the files", {
  urls <- character(0)
  streamedTo <- NULL
  harvest <- inaturalistR
  local_mocked_bindings(
    getSources=inatSource,
    curl_fetch_memory=inatAPI(function(url) {
      urls <<- c(urls, url)
      inatResponse(200, inatPage(c(1001, 1002), 2))
    }),
    inaturalistR=function(..., dir=NULL) {
      streamedTo <<- dir
      harvest(..., dir=dir)
    },
    uploadTraits=function(db, table) NULL)

  #The harvester and uploaders are iNaturalist's own, so what is checked is
  #what reaches the database. The stub API's classification stops at the genus,
  #which taxonomiseR() says.
  upload <- mockUpload(function(db) {
    expect_warning(ingestR(db=db), "classification stops there")
  })
  statements <- vapply(upload$executed, `[[`, character(1), "sql")
  insert <- function(table) {
    upload$executed[[which(startsWith(statements, paste0("INSERT INTO `", table, "`")))]]
  }

  #Every taxon is harvested, which is the taxon_id left out, and streamed to
  #files, as it will not fit in memory
  expect_false(grepl("taxon_id", urls[1], fixed=TRUE))
  expect_identical(streamedTo, file.path(tempdir(), "harvest-iNaturalist"))

  #The links and details iNaturalist gave before are removed once each, before
  #the harvest's are inserted, and the recordings and taxa are updated by
  #their ids
  for (table in c("links", "details")) {
    deleted <- which(startsWith(statements, paste0("DELETE FROM `", table, "`")))
    expect_length(deleted, 1)
    expect_match(statements[deleted], paste0("DELETE FROM `", table, "` WHERE `source` = ?"),
                 fixed=TRUE)
    expect_identical(upload$executed[[deleted]]$params, list("iNaturalist"))
    expect_lt(deleted, which(startsWith(statements, paste0("INSERT INTO `", table, "`"))))
  }
  expect_false(any(startsWith(statements, "DELETE FROM `recordings`")))
  expect_false(any(startsWith(statements, "DELETE FROM `taxa`")))

  recordings <- boundRows(insert("recordings"))
  expect_identical(vapply(recordings, `[[`, character(1), 1), c("iNaturalist", "iNaturalist"))
  expect_identical(vapply(recordings, `[[`, character(1), 2), c("10010", "10020"))
  taxa <- boundRows(insert("taxa"))
  expect_identical(vapply(taxa, `[[`, character(1), 2), c("9001", "9000"))
  #Each recording is about its taxon, as iNaturalist's link
  links <- boundRows(insert("links"))
  expect_identical(vapply(links, `[[`, character(1), 1), c("iNaturalist", "iNaturalist"))
  expect_identical(vapply(links, `[[`, character(1), 5), c("10010", "10020"))
  expect_identical(vapply(links, `[[`, character(1), 9), c("9001", "9001"))
  #The stub observations are research grade and open, so each recording's one
  #detail is its grade
  details <- boundRows(insert("details"))
  expect_identical(vapply(details, `[[`, character(1), 1), c("iNaturalist", "iNaturalist"))
  expect_identical(vapply(details, `[[`, character(1), 3), c("10010", "10020"))
  expect_identical(vapply(details, `[[`, character(1), 4), c("quality_grade", "quality_grade"))
  expect_identical(vapply(details, `[[`, character(1), 6), c("research", "research"))

  #The files are cleared away once they have been uploaded
  expect_false(dir.exists(file.path(tempdir(), "harvest-iNaturalist")))
})

test_that("ingestR harvests nothing while two sources are named iNaturalist", {
  #One source for each taxon group, both named iNaturalist. Each upload would
  #remove the links the other gave, and either would remove those of a harvest
  #of every taxon.
  harvested <- character(0)
  local_mocked_bindings(
    getSources=function() list(
      list(name="iNaturalist", type="recordings",
           inaturalist=list(taxon_id="47651"), process="sourceR"),
      list(name="iNaturalist", type="recordings",
           inaturalist=list(taxon_id="50186"), process="sourceR")),
    inaturalistR=function(taxon_id, ...) harvested <<- c(harvested, taxon_id),
    uploadTraits=function(db, table) NULL)

  upload <- mockUpload(function(db) {
    expect_error(ingestR(db=db), "More than one source in list_sources is named 'iNaturalist'")
  })

  expect_length(harvested, 0)
  expect_length(upload$executed, 0)
})

test_that("ingestR carries on when the iNaturalist harvest fails, removing nothing", {
  local_mocked_bindings(
    getSources=inatSource,
    inaturalistR=function(taxon_id, ...) {
      stop("iNaturalist request for taxon '' above id 0 failed")
    },
    uploadTraits=function(db, table) NULL)

  upload <- mockUpload(function(db) {
    expect_warning(ingestR(db=db), "Skipping source iNaturalist")
  })

  #The links of the last harvest are kept rather than removed for one that
  #gave nothing
  expect_length(upload$executed, 0)
})

test_that("an iNaturalist harvest waits only for each request to be answered by default", {
  expect_identical(formals(inaturalistR)$pause, 0)
  expect_identical(formals(inaturalistTaxaByID)$pause, 0)
})

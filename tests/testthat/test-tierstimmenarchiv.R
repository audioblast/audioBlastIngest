tsaFixture <- function() {
  path <- test_path("fixtures", "tierstimmenarchiv-page.json")
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  rjson::fromJSON(body)
}

#The records of the fixture that a harvest keeps, which is all of them but the
#one with no audio to fetch
tsaHarvested <- function(audio=TRUE) {
  return(tsaUsable(tsaFresh(tsaFixture(), new.env(hash=TRUE, parent=emptyenv())), audio))
}

tsaRecord <- function(table, id) {
  return(table[table$id == id, ])
}

tsaResponse <- function(status, body) {
  list(status_code=status, content=charToRaw(enc2utf8(body)))
}

test_that("Tierstimmenarchiv records are converted to the recordings format", {
  data <- tsaRecordings(tsaHarvested())

  expect_identical(names(data), names(getHeaders("recordings")))
  expect_true(all(vapply(data, is.character, logical(1))))
  #The record with no audio to fetch is left out
  expect_false("TSA:Acanthis_cannabina_V_1743_10_2" %in% data$id)
  expect_identical(nrow(data), 11L)

  frog <- tsaRecord(data, "TSA:Leptopelis_aubryi_DIG_174_1_1")
  expect_identical(frog$source, "")
  expect_identical(frog$Title, "Leptopelis aubryi - call")
  expect_identical(frog$taxon, "Leptopelis aubryi")
  expect_identical(frog$file, paste0("https://suche.tierstimmenarchiv.de/download.wav",
                                     "?unique_identifier=TSA%3ALeptopelis_aubryi_DIG_174_1_1"))
  expect_identical(frog$author, "R\u00f6del, Mark-Oliver")
  expect_identical(Encoding(frog$author), "UTF-8")
  expect_identical(frog$post_date, "2014-03-05")
  expect_identical(frog$size, "")
  expect_identical(frog$size_raw, "")
  expect_identical(frog$type, "audio/x-wav")
  expect_identical(frog$NonSpecimen, "")
  expect_identical(frog$Date, "2009-09-08")
  #The archive gave no time of day for this one
  expect_identical(frog$Time, "")
  expect_identical(frog$Duration, "5")
  expect_identical(frog$deployment, "")
  expect_identical(frog$lat, "4.691944")
  expect_identical(frog$lon, "9.283333")
  expect_identical(frog$time_of_day, "")
  expect_identical(frog$license, "https://creativecommons.org/licenses/by-nc-sa/")
  expect_identical(frog$info_url, paste0("https://suche.tierstimmenarchiv.de/search/showdetails.html",
                                         "?unique_identifier=TSA%3ALeptopelis_aubryi_DIG_174_1_1"))
  #The recordist is who a CC BY licence asks to be credited
  expect_identical(frog$rights_holder, "R\u00f6del, Mark-Oliver")
  expect_identical(frog$country, "CM")
  expect_identical(frog$locality, "Big Massaka")
  expect_identical(frog$sample_rate, "44100")
  expect_identical(frog$channels, "")
})

test_that("a Tierstimmenarchiv recording is titled by what it is of and what it does", {
  data <- tsaRecordings(tsaHarvested())

  expect_identical(tsaRecord(data, "TSA:Crex_crex_DIG0208_23")$Title, "Crex crex - song")
  #A subspecies is given as a bare epithet, which belongs to the species
  bird <- tsaRecord(data, "J_Orn:Trachyphonus_margaritatus_154765")
  expect_identical(bird$taxon, "Trachyphonus margaritatus somalicus")
  expect_identical(bird$Title, "Trachyphonus margaritatus somalicus - call")
  #An author is written with a space in front of it in some records
  expect_identical(bird$author, "Mahamoud-Issa, Mathieu")

  #A name left open at the species is the genus, as far as it was identified
  bat <- tsaRecord(data, "VN:Myotis_spec_Vtb0001_13")
  expect_identical(bat$taxon, "Myotis")
  expect_identical(bat$Title, "Myotis spec. - echolocation call")

  #"div." is no taxon, so the title falls back to what was recorded
  calibration <- tsaRecord(data, "TSA:Calibration_DIG213_94")
  expect_identical(calibration$taxon, "")
  expect_identical(calibration$Title, "div.")
})

test_that("Tierstimmenarchiv lengths, rates and licences are read", {
  data <- tsaRecordings(tsaHarvested())

  #A length is HH:MM:SS, and the seconds the archive also gives for a few
  #records are used where it has them
  expect_identical(tsaRecord(data, "J_Orn:Trachyphonus_margaritatus_154765")$Duration, "8.469")
  #A length given as 0 seconds is no length, so the clock is read instead
  expect_identical(tsaRecord(data, "VN:Myotis_spec_Vtb0001_13")$Duration, "5")

  #A third of the records that give a sample rate give it in kHz
  expect_identical(tsaRecord(data, "VN:Myotis_spec_Vtb0001_13")$sample_rate, "256000")
  expect_identical(
    tsaRecord(data, "TSA:Pipistrellus_pygmaeus_Teerofenbruecke_20250522_212802")$sample_rate,
    "256000")
  expect_identical(tsaRecord(data, "TSA:Rana_temporaria_DIG0204_21")$sample_rate, "44100")

  #The archive names a licence but not its version, so no version is added. One
  #licence is named three ways, one of them with a full stop where the others
  #have a comma (e.g. TSA:Hippolais_icterina_DIG_28_6_1), and a recording the
  #archive gives no licence, as null or as "", is given none.
  expect_identical(tsaLicense(c("CC BY-SA", "CC BY-NC-SA", "CC BY-NC-SA, no commercial use",
                                "CC BY-NC-SA. no commercial use", "CC BY", "", NA)),
                   c("https://creativecommons.org/licenses/by-sa/",
                     "https://creativecommons.org/licenses/by-nc-sa/",
                     "https://creativecommons.org/licenses/by-nc-sa/",
                     "https://creativecommons.org/licenses/by-nc-sa/",
                     "https://creativecommons.org/licenses/by/", "", ""))
  expect_warning(tsaLicense("Ask the archive"), "licence that could not be read")
})

test_that("Tierstimmenarchiv records with no audio are harvested only when asked for", {
  expect_length(tsaHarvested(audio=TRUE), 11)
  expect_length(tsaHarvested(audio=FALSE), 12)

  data <- tsaRecordings(tsaHarvested(audio=FALSE))
  card <- tsaRecord(data, "TSA:Acanthis_cannabina_V_1743_10_2")
  expect_identical(card$file, "")
  expect_identical(card$type, "")
  expect_identical(card$taxon, "Carduelis cannabina")
})

test_that("a Tierstimmenarchiv time that is not a clock time is kept in words", {
  #The archive's own values: clock times, written in several ways, and the
  #words and mistyped clock times that a few hundred records give instead
  times <- c("11:23", "9:30", "14.20", "morning", "afternoon", "noon", "evening",
             "night", "10.:00", "12:5", "")
  expect_identical(tsaTime(times),
                   c("11:23", "09:30", "14:20", "", "", "", "", "", "", "", ""))
  #noon is a word rather than 12:00, and a mistyped clock time is kept as
  #written rather than guessed at
  expect_identical(tsaTimeOfDay(times),
                   c("", "", "", "morning", "afternoon", "noon", "evening",
                     "night", "10.:00", "12:5", ""))
  #The archive gives no ranges or approximate times yet, but they would be
  #kept as written too, and a placeholder would not
  expect_identical(tsaTimeOfDay(c("7-8 h", "ca. 7:00", "?")), c("7-8 h", "ca. 7:00", ""))

  records <- tsaHarvested()
  ids <- tsaField(records, "unique_identifier")
  given <- c("TSA:Crex_crex_DIG0208_23"="morning",
             "TSA:Rana_temporaria_DIG0204_21"=" 10.:00 ",
             "TSA:Coturnix_coturnix_DIG0210_16"="/N")
  for (id in names(given)) records[[which(ids == id)]]$recording_time <- given[[id]]
  data <- tsaRecordings(records)

  crake <- tsaRecord(data, "TSA:Crex_crex_DIG0208_23")
  expect_identical(crake$Time, "")
  expect_identical(crake$time_of_day, "morning")
  #A clock time stays a clock time, and gives no time of day in words
  woodpecker <- tsaRecord(data, "TSA:Dryocopus_martius_DIG0210_08")
  expect_identical(woodpecker$Time, "08:00")
  expect_identical(woodpecker$time_of_day, "")
  frog <- tsaRecord(data, "TSA:Rana_temporaria_DIG0204_21")
  expect_identical(frog$Time, "")
  expect_identical(frog$time_of_day, "10.:00")
  #"/N" is how some records say they have no value, so it is no time at all
  quail <- tsaRecord(data, "TSA:Coturnix_coturnix_DIG0210_16")
  expect_identical(quail$Time, "")
  expect_identical(quail$time_of_day, "")

  #Kept in words, the time survives the normalising that uploading does
  normalised <- normaliseRecordings(crake)
  expect_identical(normalised$Time, NA_character_)
  expect_identical(normalised$time_of_day, "morning")
})

test_that("a Tierstimmenarchiv record is harvested once", {
  seen <- new.env(hash=TRUE, parent=emptyenv())
  expect_length(tsaFresh(tsaFixture(), seen), 12)
  #The archive answers with the last page again once a harvest runs off the end
  #of a result set, and nothing on it is new
  expect_length(tsaFresh(tsaFixture(), seen), 0)
})

test_that("what a Tierstimmenarchiv record holds besides is kept as details", {
  data <- tsaDetails(tsaHarvested())

  expect_identical(names(data), names(getHeaders("details")))
  expect_true(all(data$type == "recordings"))
  expect_true(all(data$value != ""))

  frog <- data[data$id == "TSA:Leptopelis_aubryi_DIG_174_1_1", ]
  named <- function(name) frog$value[frog$name == name]
  expect_identical(named("sound_type"), "call")
  expect_identical(named("altitude"), "320")
  expect_identical(frog$unit[frog$name == "altitude"], "m")
  expect_identical(named("bit_depth"), "16")
  expect_identical(named("collection"), "TSA")
  #A specimen is the archive's own value, whether it is a collection's number
  #or the recordist's label for an animal
  expect_identical(named("specimen"), "ZFMK 89560")
  expect_true(grepl("chuck call", named("description"), fixed=TRUE))

  #A record can name more than one sound type, and each is a detail of its own
  warbler <- data[data$id == "TSA:Acrocephalus_palustris_V_1837_2_1", ]
  expect_identical(warbler$value[warbler$name == "sound_type"],
                   c("song", "call", "call sequence", "twittering"))
})

test_that("the archive's /N for no value is no value, whichever field it is in", {
  expect_identical(tsaText("/N"), "")
  expect_identical(tsaText(" /N "), "")

  #Part of one of the records that write it, from Reinald Skiba's bat tapes
  bat <- list(
    unique_identifier="TSA:Barbastella_barbastellus_Ski0109_S1_From0663731ms_To0682090ms",
    filename="Barbastella_barbastellus_Ski0109_S1_From0663731ms_To0682090ms",
    species="Barbastella barbastellus", sound_type="echolocation call",
    author="Skiba, Reinald", country="DE", locality="Selbitz",
    scenic_area="/N", habitat="/N", sex="/N", age="/N", specimen="/N",
    background_species="/N", weather="/N")

  details <- tsaDetails(list(bat))
  expect_false("/N" %in% details$value)
  expect_identical(sort(details$name), c("filename", "sound_type"))

  #Nothing says which fields it can be in, so it is dropped from all of them,
  #the recordings table's columns included
  bat$author <- "/N"
  bat$locality <- "/N"
  recording <- tsaRecordings(list(bat))
  expect_identical(recording$author, "")
  expect_identical(recording$rights_holder, "")
  expect_identical(recording$locality, "")
  expect_identical(recording$country, "DE")

  #"/N" in background_species names no taxon behind the bat
  links <- tsaLinks(list(bat))
  expect_identical(links$object_id, "Barbastella barbastellus")
  expect_identical(links$qualifier, "")
})

test_that("the paper a Tierstimmenarchiv recording was used in becomes a reference", {
  data <- tsaReferences(tsaHarvested())

  expect_identical(names(data), names(getHeaders("references")))
  expect_identical(nrow(data), 3L)

  frog <- data[data$id == "10.3897/zse.90.7120", ]
  expect_identical(frog$type, "article")
  expect_identical(frog$author,
                   "Roedel, M-O; Emmrich, M; Penner, J; Schmitz, A; Barej, M")
  expect_identical(frog$year, "2014")
  expect_identical(frog$journal, "Zoosystematics and Evolution")
  expect_identical(frog$volume, "90")
  expect_identical(frog$number, "1")
  expect_identical(frog$pages, "21-31")
  expect_identical(frog$doi, "10.3897/zse.90.7120")
  expect_true(startsWith(frog$title, "The taxonomic status of two West African"))
  #A title can have a full stop in it, and the journal comes after the last one
  expect_true(grepl("L. macrotis", frog$title, fixed=TRUE))

  #A paper with no volume, issue or pages is still a paper
  rails <- data[data$id == "10.1007/s10336-026-02368-7", ]
  expect_identical(rails$journal, "J Ornithol")
  expect_identical(rails$volume, "")
  expect_identical(rails$pages, "")

  #What the note said besides the paper stays a note
  details <- tsaDetails(tsaHarvested())
  frogNotes <- details[details$id == "TSA:Leptopelis_aubryi_DIG_174_1_1" &
                         details$name == "notes", ]
  expect_identical(frogNotes$value, "sv")
})

test_that("a Tierstimmenarchiv note that does not read as a citation is left alone", {
  note <- "Used in: a talk I gave once"
  expect_true(is.na(tsaCitations(note)$id))
  expect_identical(tsaRemarks(note), note)
  expect_identical(nrow(tsaReferences(list(list(unique_identifier="x", notes=note)))), 0L)
})

test_that("Tierstimmenarchiv recordings are linked to what they are about", {
  data <- tsaLinks(tsaHarvested())

  expect_identical(names(data), names(getHeaders("links")))
  expect_true(all(data$subject_type == "recordings"))

  about <- data[data$predicate == "http://purl.obolibrary.org/obo/IAO_0000136", ]
  focal <- about[about$qualifier == "", ]
  #Every recording but the one of "div." is about a taxon
  expect_identical(nrow(focal), 10L)
  expect_identical(focal$object_id[focal$subject_id == "VN:Myotis_spec_Vtb0001_13"], "Myotis")

  #A species audible behind a recording is told from its own by the qualifier
  background <- about[about$qualifier != "", ]
  expect_true(all(background$qualifier ==
                    "https://vocab.audioblast.org/cv/recordingContent#NonFocalTaxa"))
  expect_identical(sort(background$object_id), c("Nyctalus noctula", "Sylvia communis"))
  #"birds" is no taxon, so the recording it is given for has no background link
  expect_false("TSA:Rana_temporaria_DIG0204_21" %in% background$subject_id)

  cited <- data[data$predicate == "http://purl.org/dc/terms/isReferencedBy", ]
  expect_identical(cited$object_type, rep_len("references", nrow(cited)))
  expect_identical(cited$object_id[cited$subject_id == "TSA:Leptopelis_aubryi_DIG_174_1_1"],
                   "10.3897/zse.90.7120")
})

test_that("the taxa a Tierstimmenarchiv page names all have a record", {
  records <- tsaHarvested()
  taxa <- tsaTaxa(records)
  links <- tsaLinks(records)

  expect_identical(names(taxa), names(getHeaders("taxa")))
  expect_identical(setdiff(links$object_id[links$object_type == "taxa"], taxa$id),
                   character(0))
  #A name implies the taxa it sits in
  expect_true(all(c("Trachyphonus", "Trachyphonus margaritatus",
                    "Trachyphonus margaritatus somalicus") %in% taxa$id))
  expect_identical(taxa$Rank[taxa$id == "Trachyphonus margaritatus somalicus"], "Subspecies")
  expect_identical(taxa$parent_id[taxa$id == "Trachyphonus margaritatus somalicus"],
                   "Trachyphonus margaritatus")
  expect_identical(taxa$parent_id[taxa$id == "Trachyphonus"], "")
})

test_that("a Tierstimmenarchiv name is read as a taxon, or as none", {
  expect_identical(
    tsaName(c("Crex crex", "Trachyphonus margaritatus somalicus",
              "Capra hircus f. hircus", "Bos taurus f. taurus",
              "Canis lupus f. dingo hallstromi")),
    c("Crex crex", "Trachyphonus margaritatus somalicus",
      "Capra hircus f. hircus", "Bos taurus f. taurus",
      "Canis lupus f. dingo hallstromi"))
  #A name left open at the species is the genus
  expect_identical(tsaName(c("Myotis spec.", "Acrocephalus sp.", "Acrocephalus sp")),
                   c("Myotis", "Acrocephalus", "Acrocephalus"))
  #What is not a name is no taxon, and neither is a form with no epithet or one
  #with no species
  expect_identical(tsaName(c("div.", "birds", "", NA, "Capra hircus f.", "Capra f. hircus")),
                   rep("", 6))

  #A subspecies is added to its species, and given after a form it is the second
  #word of the form's epithet
  expect_identical(
    tsaTaxon(c("Trachyphonus margaritatus", "Bos taurus f. taurus", "Canis lupus f. dingo",
               "Myotis spec.", "div."),
             c("somalicus", "", "hallstromi", "", "")),
    c("Trachyphonus margaritatus somalicus", "Bos taurus f. taurus",
      "Canis lupus f. dingo hallstromi", "Myotis", ""))
})

test_that("a Tierstimmenarchiv name with an umlaut has it written out", {
  expect_identical(tsaUmlaut("\u00e4\u00f6\u00fc\u00c4\u00d6\u00dc"), "aeoeueAeOeUe")
  expect_identical(tsaName(c("M\u00fclleripicus pulverulentus",
                             "M\u00fclleripicus pulverulentus harterti")),
                   c("Muelleripicus pulverulentus", "Muelleripicus pulverulentus harterti"))
  expect_identical(tsaTaxon("Crex crex", "m\u00fclleri"), "Crex crex muelleri")
  #A name with any other letter outside the Latin alphabet is still no name
  expect_identical(tsaName(c("M\u00e9lleripicus pulverulentus", "Mu\u00dfleripicus pulverulentus")),
                   c("", ""))

  #The title keeps the name as the archive wrote it
  records <- list(list(unique_identifier="TSA:Muelleripicus_pulverulentus_Lue_74_1_1",
                       filename="1", species="M\u00fclleripicus pulverulentus",
                       subspecies="harterti", sound_type="call"))
  data <- tsaRecordings(records)
  expect_identical(data$taxon, "Muelleripicus pulverulentus harterti")
  expect_identical(data$Title, "M\u00fclleripicus pulverulentus harterti - call")
  expect_identical(sort(tsaTaxa(records)$id),
                   c("Muelleripicus", "Muelleripicus pulverulentus",
                     "Muelleripicus pulverulentus harterti"))
})

test_that("a Tierstimmenarchiv form is a taxon of its own inside its species", {
  records <- list(
    list(unique_identifier="TSA:1", filename="1", species="Capra hircus f. hircus",
         sound_type="call"),
    list(unique_identifier="TSA:2", filename="2", species="Bos taurus f. taurus",
         background_species="Capra hircus f. hircus, birds"),
    list(unique_identifier="TSA:3", filename="3", species="Canis lupus f. dingo",
         subspecies="hallstromi", sound_type="howling"))

  data <- tsaRecordings(records)
  expect_identical(data$taxon, c("Capra hircus f. hircus", "Bos taurus f. taurus",
                                 "Canis lupus f. dingo hallstromi"))
  expect_identical(data$Title, c("Capra hircus f. hircus - call", "Bos taurus f. taurus",
                                 "Canis lupus f. dingo hallstromi - howling"))

  taxa <- tsaTaxa(records)
  rank <- function(id) taxa$Rank[taxa$id == id]
  parent <- function(id) taxa$parent_id[taxa$id == id]
  expect_identical(sort(taxa$id),
                   sort(c("Capra", "Capra hircus", "Capra hircus f. hircus",
                          "Bos", "Bos taurus", "Bos taurus f. taurus",
                          "Canis", "Canis lupus", "Canis lupus f. dingo hallstromi")))
  expect_identical(rank("Capra hircus f. hircus"), "Form")
  expect_identical(parent("Capra hircus f. hircus"), "Capra hircus")
  expect_identical(rank("Capra hircus"), "Species")
  expect_identical(parent("Capra hircus"), "Capra")
  expect_identical(rank("Capra"), "Genus")
  #However many words a form's epithet has, it sits in the species
  expect_identical(rank("Canis lupus f. dingo hallstromi"), "Form")
  expect_identical(parent("Canis lupus f. dingo hallstromi"), "Canis lupus")

  #The taxa table has no column for a form, but the form's species and genus
  #have theirs
  walked <- taxonomiseR(sourceR("TSA", taxa))
  form <- walked[walked$id == "Bos taurus f. taurus", ]
  expect_identical(form$Species, "Bos taurus")
  expect_identical(form$Genus, "Bos")

  links <- tsaLinks(records)
  focal <- links[links$qualifier == "", ]
  expect_identical(focal$object_id, data$taxon)
  background <- links[links$qualifier != "", ]
  expect_identical(background$subject_id, "TSA:2")
  expect_identical(background$object_id, "Capra hircus f. hircus")
  expect_identical(setdiff(links$object_id, taxa$id), character(0))
})

test_that("a Tierstimmenarchiv page with nothing on it gives empty tables", {
  makes <- list(recordings=tsaRecordings, details=tsaDetails, taxa=tsaTaxa,
                references=tsaReferences, links=tsaLinks)
  for (type in names(makes)) {
    data <- makes[[type]](list())
    expect_identical(names(data), names(getHeaders(type)))
    expect_identical(nrow(data), 0L)
    expect_true(all(vapply(data, is.character, logical(1))))
  }
})

test_that("a Tierstimmenarchiv search is escaped as it is typed", {
  expect_identical(tsaParameters("species=Anas acuta"), "species=Anas%20acuta")
  expect_identical(tsaParameters("unique_identifier=:"), "unique_identifier=%3A")
  expect_identical(tsaParameters("country=DE&from_year=2000"),
                   "country=DE&from_year=2000")
  expect_identical(tsaParameters("has_coords"), "has_coords=")
  expect_error(tsaParameters(""), "must have a parameter")
})

test_that("a Tierstimmenarchiv recording links to the archive's page for it alone", {
  #A search for TSA:Fulica_atra_M_5_2_1 would list TSA:Fulica_atra_M_5_2_11
  #too, so the link is to the details page, which shows the one recording
  expect_identical(tsaRecordURL(c("TSA:Fulica_atra_M_5_2_1", "")),
                   c(paste0("https://suche.tierstimmenarchiv.de/search/showdetails.html",
                            "?unique_identifier=TSA%3AFulica_atra_M_5_2_1"), ""))
})

test_that("a Tierstimmenarchiv request that will not succeed is not retried", {
  handle <- NULL
  #A search that was not understood answers 200 with an object rather than a
  #page of records, and would answer the same way however often it was asked
  tries <- 0
  local_mocked_bindings(
    curl_fetch_memory=function(url, handle) {
      tries <<- tries + 1
      tsaResponse(200, '{"Warning": "no search parameters found"}')
    })

  expect_error(tsaFetch("species=", 1, 500, TRUE, handle, backoff=c(1, 1)),
               "no search parameters found")
  expect_identical(tries, 1)
})

test_that("a Tierstimmenarchiv request that fails once is retried", {
  tries <- 0
  local_mocked_bindings(
    curl_fetch_memory=function(url, handle) {
      tries <<- tries + 1
      if (tries == 1) return(tsaResponse(503, "busy"))
      return(tsaResponse(200, '[{"unique_identifier": "TSA:x", "filename": "x"}]'))
    })

  records <- tsaFetch("species=Anas acuta", 1, 500, TRUE, NULL, backoff=c(0, 0))
  expect_identical(tries, 2)
  expect_identical(tsaField(records, "unique_identifier"), "TSA:x")
})

test_that("a Tierstimmenarchiv harvest stops when the archive repeats its last page", {
  #Asked for a page after the last one, the archive answers with the last
  #page's records again rather than with none, and goes on answering with them
  #for ever, so a full page is no reason to ask for another
  page <- function(ids) {
    records <- lapply(ids, function(id) {
      list(unique_identifier=id, filename=id, species="Crex crex", sound_type="song")
    })
    return(tsaResponse(200, rjson::toJSON(records)))
  }
  asked <- 0
  local_mocked_bindings(
    curl_fetch_memory=function(url, handle) {
      asked <<- asked + 1
      if (asked == 1) return(page(c("TSA:1", "TSA:2")))
      return(page(c("TSA:3", "TSA:4")))
    })

  harvest <- tierstimmenarchivR("species=Crex crex", per_page=2, pause=0)
  #Two pages of records, then a third that repeats the second and ends it
  expect_identical(asked, 3)
  expect_identical(harvest$recordings$id, c("TSA:1", "TSA:2", "TSA:3", "TSA:4"))
  expect_identical(nrow(harvest$taxa), 2L)
})

test_that("a Tierstimmenarchiv harvest streams each of its tables to a file", {
  local_mocked_bindings(
    curl_fetch_memory=function(url, handle) {
      tsaResponse(200, rjson::toJSON(list(
        list(unique_identifier="TSA:1", filename="1", species="Crex crex",
             sound_type="song", usage_permission="CC BY-SA"))))
    })

  dir <- withr::local_tempdir()
  paths <- tierstimmenarchivR("species=Crex crex", per_page=2, pause=0, dir=dir)
  expect_identical(names(paths), c("recordings", "details", "taxa", "references", "links"))
  for (type in names(paths)) {
    expect_true(file.exists(paths[[type]]))
    rows <- readStream(paths[[type]], 100, function(chunk) NULL)
    expect_identical(rows, c(recordings=1L, details=2L, taxa=2L, references=0L, links=1L)[[type]])
  }
})

tsaSource <- function() {
  list(list(name="TSA", type="recordings",
            tierstimmenarchiv=list(query="unique_identifier=:"), process="sourceR"))
}

test_that("ingestR streams a Tierstimmenarchiv harvest and uploads it from the files", {
  harvest <- list()
  local_mocked_bindings(
    getSources=tsaSource,
    tierstimmenarchivR=function(query, verbose=FALSE, dir=NULL) {
      harvest$query <<- query
      harvest$dir <<- dir
      records <- tsaUsable(tsaFresh(tsaFixture(), new.env(hash=TRUE, parent=emptyenv())), TRUE)
      streamTable(dir, "recordings", tsaRecordings(records))
      return(list(recordings=streamPath(dir, "recordings")))
    },
    uploadStreamed=function(db, source, dir, verbose=FALSE) {
      harvest$source <<- source
      harvest$rows <<- readStream(streamPath(dir, "recordings"), 100, function(chunk) NULL)
    },
    uploadTraits=function(db, table) NULL)

  ingestR(db="db")

  #The module's query reaches the harvester, and what it streamed is what was
  #uploaded, under the source's own name
  expect_identical(harvest$query, "unique_identifier=:")
  expect_identical(harvest$source, "TSA")
  expect_identical(harvest$rows, 11L)
  #The files are cleared away once they have been uploaded
  expect_false(dir.exists(harvest$dir))
})

test_that("ingestR carries on when the Tierstimmenarchiv harvest fails", {
  uploaded <- FALSE
  local_mocked_bindings(
    getSources=tsaSource,
    tierstimmenarchivR=function(query, verbose=FALSE, dir=NULL) {
      stop("Tierstimmenarchiv request for 'unique_identifier=:' page 1 failed")
    },
    uploadStreamed=function(db, source, dir, verbose=FALSE) uploaded <<- TRUE,
    uploadTraits=function(db, table) NULL)

  expect_warning(ingestR(db="db"), "Skipping source TSA")
  expect_false(uploaded)
})

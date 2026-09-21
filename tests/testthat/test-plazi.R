plaziFixture <- function(name) {
  path <- test_path("fixtures", name)
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  body
}

plaziTreatmentXML <- function() plaziFixture("plazi-treatment.xml")
plaziZenodoPage <- function() rjson::fromJSON(plaziFixture("plazi-zenodo-page.json"))

treated <- "5B3A87E1FFAC5D4F48A2FF532C0EF9B1"

test_that("a treatment's sections are converted to the descriptions format", {
  data <- plaziSections(plaziRead(plaziTreatmentXML(), treated), treated)

  expect_identical(names(data), names(getHeaders("descriptions")))
  expect_true(all(vapply(data, is.character, logical(1))))
  #Of the treatment's four sections, nomenclature is the taxon's name rather
  #than what the treatment says about it, and the description and etymology
  #hold only a figure number, so one section is left
  expect_identical(nrow(data), 1L)
  expect_identical(data$topic, "distribution")
  expect_identical(data$source, "")
  #A section is identified within its treatment, so a treatment that is
  #processed again and gains a section does not renumber the others
  expect_identical(data$id, paste0(treated, "#9B89657CFFAD5D4F48F2F91C2C0EF9B1"))
  expect_identical(data$info_url, paste0("https://treatment.plazi.org/id/", treated))
  #normaliseDescriptions() reads the info item, not the harvest
  expect_identical(data$topic_link, "")
})

test_that("a figure's caption is not part of what a section says", {
  data <- plaziSections(plaziRead(plaziTreatmentXML(), treated), treated)

  #The caption of figure 3 is set inside the distribution section because that
  #is where the figure fell on the page, not because it is about distribution
  expect_false(grepl("FIGURE 3.", data$value, fixed=TRUE))
  expect_false(grepl("Scale bars", data$value, fixed=TRUE))
  #What the section does say survives, including the calling song that Plazi
  #has run into it
  expect_true(startsWith(data$value, "Distribution. INDONESIA"))
  expect_true(grepl("peak frequency of 11.7", data$value, fixed=TRUE))
})

test_that("a Zenodo record says which treatment to read and under what licence", {
  hits <- plaziZenodoPage()$hits$hits

  found <- plaziTreatment(hits[[1]])
  expect_identical(found$uuid, treated)
  expect_identical(found$license, "cc-zero")
  #The article the treatment was published in, as a resolvable DOI
  expect_identical(found$article, "https://doi.org/10.11646/zootaxa.5415.1.5")
  expect_identical(found$taxon, "https://www.gbif.org/species/221747866")

  #A record that names no treatment at Plazi cannot be read
  expect_null(plaziTreatment(hits[[3]]))
  expect_null(plaziTreatment(list()))
})

test_that("a taxon GBIF has not matched is named by Plazi's own concept", {
  hit <- plaziZenodoPage()$hits$hits[[1]]
  hit$metadata$related_identifiers <- Filter(
    function(r) !grepl("gbif.org", r$identifier, fixed=TRUE),
    hit$metadata$related_identifiers)

  found <- plaziTreatment(hit)
  expect_identical(found$taxon, paste0("http://taxon-concept.plazi.org/id/", treated))
  expect_identical(found$article, "https://doi.org/10.11646/zootaxa.5415.1.5")
})

test_that("a treatment is harvested as the reference its descriptions cite", {
  treatment <- plaziTreatment(plaziZenodoPage()$hits$hits[[1]])
  reference <- plaziReference(plaziRead(plaziTreatmentXML(), treated), treatment)

  expect_identical(names(reference), names(getHeaders("references")))
  expect_identical(nrow(reference), 1L)
  expect_identical(reference$source, "")
  expect_identical(reference$id, treated)
  #A titled part of a larger work
  expect_identical(reference$type, "incollection")
  #A treatment's title is the name it treats
  expect_identical(reference$title, "Macroxiphus sumatranus")
  expect_identical(reference$year, "2024")
  expect_identical(reference$journal, "Zootaxa 5415 (1)")
  expect_true(startsWith(reference$booktitle, "An account on some katydids"))
  #Its own DOI, not the article's: a treatment is deposited as a publication
  expect_identical(reference$doi, "10.5281/zenodo.10716201")
  expect_identical(reference$info_url, paste0("https://treatment.plazi.org/id/", treated))
})

test_that("a name Plazi could not parse is taken from the printed text", {
  document <- plaziRead(plaziFixture("plazi-undefined.xml"), "undefined")

  #Plazi reads open nomenclature as undefined: a treatment of Gryllus sp.4 is
  #marked up species="undefined-4" and titled "Gryllus undefined-4". Zenodo
  #and GBIF's backbone carry the mangled name too, so the printed text is the
  #only place it survives.
  named <- plaziName(document, "Gryllus undefined-4")
  expect_identical(named$title, "Gryllus sp.4")
  #What Plazi gave is kept, so the correction stays auditable
  expect_true(grepl("Gryllus undefined-4", named$note, fixed=TRUE))

  treatment <- plaziTreatment(plaziZenodoPage()$hits$hits[[1]])
  reference <- plaziReference(document, treatment)
  expect_identical(reference$title, "Gryllus sp.4")
  expect_true(grepl("open nomenclature", reference$note, fixed=TRUE))
})

test_that("a name Plazi parsed is left as it gives it", {
  document <- plaziRead(plaziTreatmentXML(), treated)

  #The printed name carries its authority and Plazi's spacing, which a title
  #should not gain, so only a mangled title is replaced
  named <- plaziName(document, "Macroxiphus sumatranus")
  expect_identical(named$title, "Macroxiphus sumatranus")
  expect_identical(named$note, "")

  #And a mangled title with no printed name to put in its place is kept rather
  #than guessed at
  expect_identical(plaziName(document, "Gryllus undefined-4"),
                   list(title="Gryllus undefined-4", note=""))
})

test_that("a description is about a taxon and rests on the treatment that says it", {
  treatment <- plaziTreatment(plaziZenodoPage()$hits$hits[[1]])
  links <- plaziLinks(c("a", "b"), treatment)

  expect_identical(names(links), names(getHeaders("links")))
  #Two descriptions, each about a taxon and each citing the treatment, and the
  #treatment citing the article once rather than once per description
  expect_identical(nrow(links), 5L)

  about <- links[links$predicate == "http://purl.obolibrary.org/obo/IAO_0000136", ]
  expect_identical(about$subject_id, c("a", "b"))
  expect_true(all(about$object_type == "iri"))
  expect_true(all(about$object_id == "https://www.gbif.org/species/221747866"))

  #The treatment is cited as a record, not as a URL, so that anything else
  #harvested from it cites the same record
  cites <- links[links$subject_type == "descriptions" &
                   links$predicate == "http://purl.org/dc/terms/source", ]
  expect_identical(cites$subject_id, c("a", "b"))
  expect_true(all(cites$object_type == "references"))
  expect_true(all(cites$object_id == treated))

  #And the treatment rests on the article it is part of
  part <- links[links$subject_type == "references", ]
  expect_identical(nrow(part), 1L)
  expect_identical(part$subject_id, treated)
  expect_identical(part$object_type, "iri")
  expect_identical(part$object_id, "https://doi.org/10.11646/zootaxa.5415.1.5")

  #A treatment whose article Zenodo does not give still cites the treatment
  treatment$article <- ""
  expect_identical(nrow(plaziLinks(c("a", "b"), treatment)), 4L)
  expect_identical(nrow(plaziLinks(character(), treatment)), 0L)
})

test_that("the links a harvest gives can be uploaded", {
  treatment <- plaziTreatment(plaziZenodoPage()$hits$hits[[1]])
  links <- plaziLinks(paste0(treated, "#9B89657C"), treatment)

  #normaliseLinks() skips links whose types or predicate it does not know
  expect_silent(normalised <- normaliseLinks(sourceR("Plazi", links)))
  expect_identical(nrow(normalised), 3L)
  expect_true(all(normalised$source == "Plazi"))
  expect_true(all(normalised$subject_source == "Plazi"))
  #A reference is a record audioBlast! holds, so it takes the harvesting
  #source; the object of an iri link is the IRI itself and takes none
  expect_identical(normalised$object_source[normalised$object_type == "references"], "Plazi")
  expect_true(all(normalised$object_source[normalised$object_type == "iri"] == ""))
})

test_that("text broken by the typesetting is put back together", {
  #Plazi reads a treatment from the page it was printed on, so a URL or a DOI
  #can be split on the spaces that fitted it to the column
  expect_identical(plaziText("see https: / / doi. org / 10.5252 / zoosystema 2021 v 43 a 6"),
                   "see https://doi.org/10.5252/zoosystema 2021 v 43 a 6")
  expect_identical(plaziText("at http: // www. iucnredlist. org / info"),
                   "at http://www.iucnredlist.org/info")
  expect_identical(plaziText("doi 10. 11646 / zootaxa.5415.1.5"), "doi 10.11646/zootaxa.5415.1.5")
  #Prose is left as the treatment gives it, and a sentence is not a host name
  expect_identical(plaziText("  a  b\n c "), "a b c")
  expect_identical(plaziText("in the genus. Organs are paired."), "in the genus. Organs are paired.")
})

test_that("a topic that is of a sound is of behaviour", {
  #The Species Profile Model defines nothing acoustic, so behaviour is the
  #nearest thing it does define
  behaviour <- paste0(spm, "Behaviour")
  expect_identical(spmInfoItem("Bioacoustics"), behaviour)
  expect_identical(spmInfoItem("calling song."), behaviour)
  expect_identical(spmInfoItem("Song"), behaviour)
  expect_identical(spmInfoItem("calling song tone comparison."), behaviour)
  expect_identical(spmInfoItem("Stridulation"), behaviour)
  #A callus is a part of a wing, so a topic is not acoustic for holding call
  expect_true(is.na(spmInfoItem("callus")))
})

test_that("topics that name an info item in other words are read", {
  expect_identical(spmInfoItem("diagnosis"), paste0(spm, "DiagnosticDescription"))
  expect_identical(spmInfoItem("biology_ecology"), paste0(spm, "Biology"))
  #The Species Profile Model spells behaviour the British way
  expect_identical(spmInfoItem("behavior."), paste0(spm, "Behaviour"))
  expect_identical(spmInfoItem("Behaviour"), paste0(spm, "Behaviour"))

  #The info items a topic names outright are unchanged
  expect_identical(spmInfoItem("distribution"), paste0(spm, "Distribution"))
  expect_identical(spmInfoItem("Habitat"), paste0(spm, "Habitat"))
  expect_identical(spmInfoItem("diagnostic"), paste0(spm, "DiagnosticDescription"))
  #A treatment's other sections name no info item, and a bare word names none
  expect_true(all(is.na(spmInfoItem(c("etymology", "discussion", "nomenclature",
                                      "remarks", "variation", "")))))
})

test_that("a harvested treatment normalises into the descriptions table", {
  data <- sourceR("Plazi", plaziSections(plaziRead(plaziTreatmentXML(), treated), treated))
  normalised <- normaliseDescriptions(data)

  expect_identical(nrow(normalised), 1L)
  expect_identical(normalised$source, "Plazi")
  expect_identical(normalised$topic, "distribution")
  expect_identical(normalised$topic_link, paste0(spm, "Distribution"))
  expect_identical(normalised$info_url,
                   paste0("https://treatment.plazi.org/id/", treated))
})

test_that("plaziR checks what it is asked to harvest", {
  expect_error(plaziR(query=""), "query must be")
  expect_error(plaziR(licenses=character()), "licenses must be")
  expect_error(plaziR(max=0), "max must be")
  expect_error(plaziR(pause=-1), "pause must be")
})

test_that("only a section that is itself about sound is kept", {
  data <- plaziSections(plaziRead(plaziTreatmentXML(), treated), treated)

  #A treatment is found because a word appears somewhere in it, which makes
  #the treatment acoustic rather than every section of it. This one's
  #distribution section is kept because Plazi ran the calling song into it.
  expect_identical(nrow(data), 1L)
  expect_identical(data$topic, "distribution")
  expect_true(grepl("calling song", data$value, ignore.case=TRUE))
})

test_that("a section that says nothing about sound is left out", {
  #A spider's carapace measurements, from a treatment that is acoustic
  #because it describes a stridulatory organ elsewhere
  carapace <- paste("DESCRIPTION. Holotype male, PNM-18876: TL 55.48. Prosoma.",
                    "Carapace, CL 15.95, CW 13.75, CH 7.2, longer than wide, oblong,",
                    "integument light to dark brown, densely covered with short",
                    "metallic gray to white scales, and with four pairs of dorsal",
                    "weak furrows.")
  expect_false(grepl(plaziAcousticText, carapace, ignore.case=TRUE, perl=TRUE))

  #And the sound-producing organ of the same spider is kept
  lyra <- paste("a reniform lyra on the prolateral maxilla with a row of large",
                "club-shaped stridulatory setae (bacillae)")
  expect_true(grepl(plaziAcousticText, lyra, ignore.case=TRUE, perl=TRUE))

  #A callus is a part of a wing, and a call is a sound
  expect_false(grepl(plaziAcousticText, "the callus is rounded", ignore.case=TRUE, perl=TRUE))
  expect_true(grepl(plaziAcousticText, "the call is a sharp note", ignore.case=TRUE, perl=TRUE))
})

test_that("the terms searched for reach beyond the insects", {
  #Each term is a search of its own, because together they match more than
  #Zenodo's 10,000-record window
  expect_true(length(plaziAcoustic) > 1)
  expect_true(all(nzchar(plaziAcoustic)))
  #An anuran's call, a cicada's organ and a bird's voice are named, not only
  #the orthopteran words
  expect_true(all(c("advertisement call", "tymbal", "vocalization") %in% plaziAcoustic))

  #A term of more than one word is searched for as a phrase
  expect_identical(plaziPhrase("advertisement call"), "\"advertisement call\"")
  expect_identical(plaziPhrase("stridulation"), "stridulation")
  expect_identical(plaziPhrase("\"already quoted\""), "\"already quoted\"")
})

test_that("plaziR takes one search or many", {
  expect_error(plaziR(query=character()), "one or more")
  expect_error(plaziR(query=c("song", NA)), "one or more")
  expect_error(plaziR(query=c("song", "")), "one or more")
})

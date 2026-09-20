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
  data <- plaziSections(plaziTreatmentXML(), treated)

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
  data <- plaziSections(plaziTreatmentXML(), treated)

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

test_that("a description is about a taxon and rests on an article", {
  treatment <- plaziTreatment(plaziZenodoPage()$hits$hits[[1]])
  links <- plaziLinks(c("a", "b"), treatment)

  expect_identical(names(links), names(getHeaders("links")))
  expect_identical(nrow(links), 4L)
  expect_true(all(links$subject_type == "descriptions"))
  #audioBlast! does not hold Plazi's taxa or its articles, so both are named
  #by their IRIs rather than by a record of a module
  expect_true(all(links$object_type == "iri"))
  expect_identical(sort(unique(links$predicate)),
                   c("http://purl.obolibrary.org/obo/IAO_0000136",
                     "http://purl.org/dc/terms/source"))

  about <- links[links$predicate == "http://purl.obolibrary.org/obo/IAO_0000136", ]
  expect_identical(about$subject_id, c("a", "b"))
  expect_true(all(about$object_id == "https://www.gbif.org/species/221747866"))

  #A treatment whose article Zenodo does not give says only what it is about
  treatment$article <- ""
  expect_identical(nrow(plaziLinks(c("a", "b"), treatment)), 2L)
  expect_identical(nrow(plaziLinks(character(), treatment)), 0L)
})

test_that("the links a harvest gives can be uploaded", {
  treatment <- plaziTreatment(plaziZenodoPage()$hits$hits[[1]])
  links <- plaziLinks("5B3A87E1FFAC5D4F48A2FF532C0EF9B1#9B89657C", treatment)

  #normaliseLinks() skips links whose types or predicate it does not know
  expect_silent(normalised <- normaliseLinks(sourceR("Plazi", links)))
  expect_identical(nrow(normalised), 2L)
  expect_true(all(normalised$source == "Plazi"))
  #The object of an iri link is the IRI itself, so it keeps no source
  expect_true(all(normalised$object_source == ""))
  expect_true(all(normalised$subject_source == "Plazi"))
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
  data <- sourceR("Plazi", plaziSections(plaziTreatmentXML(), treated))
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
  expect_error(plaziR(query=c("a", "b")), "query must be")
  expect_error(plaziR(licenses=character()), "licenses must be")
  expect_error(plaziR(max=0), "max must be")
  expect_error(plaziR(pause=-1), "pause must be")
})

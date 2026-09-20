#Reading measured acoustic parameters out of a treatment's prose. The examples
#are sentences taken from real treatments, which is where every one of these
#rules came from: each was written because the harvest got that sentence wrong.

paragraph <- function(heading, text) {
  xml2::read_xml(paste0(
    "<document><treatment><subSubSection type=\"description\">",
    "<paragraph id=\"P1\"><emphasis bold=\"true\">", heading, "</emphasis> ", text,
    "</paragraph></subSubSection></treatment></document>"))
}

measured <- function(heading, text) {
  document <- paragraph(heading, text)
  found <- plaziTraits(document, list(uuid="U", taxon="urn:taxon", article="", doi=""))
  return(found$traits[c("Trait", "Value")])
}

test_that("a mean with a spread and a range beside it is two traits", {
  got <- measured("Stridulatory file.", "tooth density 18.5\u00b11.9 (14.4\u201321.4) teeth/mm.")
  expect_equal(got$Trait, rep("Stridulatory file tooth density (per mm)", 2))
  expect_equal(got$Value, c("18.5\u00b11.9", "14.4-21.4"))
})

test_that("durations are held in seconds and frequencies in kHz", {
  expect_equal(measured("Calling song.", "Each syllable has an average duration of 18.5 ms.")$Value,
               "0.0185")
  expect_equal(measured("Calling song.", "The fundamental frequency is 570 Hz.")$Value, "0.57")
  #1 to 12 a minute is 0.0167 to 0.2 a second. Scaling has to rebuild the value
  #rather than substitute into it, or the 1 it has just written is rewritten.
  expect_equal(measured("Song.", "The echeme repetition rate is 1\u201312 min-1.")$Value,
               "0.0166667-0.2")
})

test_that("each clause of a sentence keeps its own parameter", {
  got <- measured("Calling song.", paste(
    "Average syllable duration is 13.1\u00b12.2 ms,",
    "average silent interval between consecutive syllables is 24.3\u00b12.4 ms",
    "and average syllable period is 37.4\u00b13.3 ms."))
  expect_equal(got$Trait, c("Syllable Duration", "Syllable Interval", "Syllable Period"))
  expect_equal(got$Value, c("0.0131\u00b10.0022", "0.0243\u00b10.0024", "0.0374\u00b10.0033"))
})

test_that("the fuller reading of two overlapping phrases wins", {
  #"echemes last" sits nearer the number, but the interval between echemes is
  #what is being measured
  got <- measured("Song.", "Silent intervals between echemes last 1\u20136 (mean 2.16\u00b11.70) s.")
  expect_equal(unique(got$Trait), "Echeme Interval")
})

test_that("a parameter audioBlast! has no term for does not borrow another's", {
  got <- measured("Advertisement call.",
                  "Fundamental frequency was 1450\u20131650 Hz, dominant frequency was 2950\u20133600 Hz.")
  expect_equal(got$Trait, "Fundamental Frequency")
  expect_equal(got$Value, "1.45-1.65")
})

test_that("pulse trains are counted as nothing, having no term", {
  expect_equal(nrow(measured("Song.", "Echemes contain on average 38\u00b119 pulse trains.")), 0)
})

test_that("a bound is not a measurement", {
  got <- measured("Song.", "Mean peak frequency is 15.84\u00b14.43 kHz, with peak frequencies as high as 26.60 kHz.")
  expect_equal(got$Value, "15.84\u00b14.43")
})

test_that("the unit tells a tooth count from a tooth density", {
  got <- measured("Stridulatory file.", "length 3.20\u20133.90 mm , 81\u2013106 teeth, 45.88 teeth per mm.")
  expect_equal(got$Trait, c("Length Of Stridulatory File",
                            "Number Of Teeth On Stridulatory File",
                            "Stridulatory file tooth density (per mm)"))
})

test_that("a range written out is read", {
  expect_equal(measured("Song.", "Silent intervals between echemes range from 7 to 29 s.")$Value,
               "7-29")
  expect_equal(measured("Stridulatory file.", "the length of the file between 1.4 and 4.3 mm.")$Value,
               "1.4-4.3")
})

test_that("a heading gives the call, the sex and the temperature", {
  conditions <- plaziConditions("Calling song ( 2\u2642 , 30.0\u00b0C) ( Fig. 11 )")
  expect_equal(conditions$call, "Calling song")
  expect_equal(conditions$sex, "Male")
  expect_equal(conditions$temperature, "30.0")
  #The degrees are the number the C follows, not the first number in brackets
  expect_equal(plaziConditions("Calling song (1J, 29.2\u00b11.2\u00b0C)")$temperature, "29.2")
  #A heading that only names a sound names no kind of call
  expect_equal(plaziConditions("Bioacoustics")$call, "")
})

test_that("only a paragraph about a sound is read", {
  expect_equal(nrow(measured("Diagnosis.", "Pronotum 3.5\u20134.2 mm.")), 0)
  expect_equal(nrow(measured("FIGURE 5. Calling song.", "Echeme duration 0.5 s.")), 0)
})

test_that("a taxon is named in full, and an undescribed one from its printed text", {
  named <- xml2::read_xml(paste0(
    "<document><treatment><subSubSection type=\"nomenclature\">",
    "<taxonomicName genus=\"Aglaothorax\" species=\"ovatus\" rank=\"species\">",
    "<emphasis>A. ovatus</emphasis></taxonomicName>",
    "</subSubSection></treatment></document>"))
  expect_equal(plaziTaxonName(named), "Aglaothorax ovatus")

  mangled <- xml2::read_xml(paste0(
    "<document><treatment><subSubSection type=\"nomenclature\">",
    "<taxonomicName genus=\"Gryllus\" species=\"undefined-4\" rank=\"species\">",
    "Gryllus sp.4</taxonomicName>",
    "</subSubSection></treatment></document>"))
  expect_equal(plaziTaxonName(mangled), "Gryllus sp.4")
})

test_that("a trait says what it is about and which treatment it came from", {
  document <- paragraph("Song.", "Mean peak frequency is 14.13\u00b13.02 kHz.")
  found <- plaziTraits(document, list(uuid="U", taxon="urn:taxon", article="", doi=""))
  expect_equal(nrow(found$traits), 1)
  expect_equal(found$links$predicate,
               c("http://purl.obolibrary.org/obo/IAO_0000136",
                 "http://purl.org/dc/terms/source"))
  expect_equal(found$links$object_id, c("urn:taxon", "U"))
  expect_equal(found$links$object_type, c("iri", "references"))
  expect_true(all(found$links$subject_type == "traits"))
  expect_true(all(found$links$subject_id == found$traits$traitID))
})

test_that("traits have the columns uploadTraits expects, and seperatoR reads them", {
  document <- paragraph("Stridulatory file.", "tooth density 18.5\u00b11.9 (14.4\u201321.4) teeth/mm.")
  traits <- plaziTraits(document, list(uuid="U", taxon="t", article="", doi=""))$traits
  expect_equal(names(traits), names(getHeaders("traits")))
  split <- seperatoR(traits)
  #Each of the two is one value, so neither is split again, and each keeps its id
  expect_equal(nrow(split), 2)
  expect_equal(split$min, c(16.6, 14.4))
  expect_equal(split$max, c(20.4, 21.4))
})

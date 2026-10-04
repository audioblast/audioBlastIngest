#Reading the words a treatment renders a call with. Every example here is a
#sentence from a real treatment, including the ones that must NOT be read.

rendering <- function(text, type="description", heading=NULL, taxon="Aglaothorax ovatus") {
  genus <- sub(" .*", "", taxon)
  species <- sub(".* ", "", taxon)
  opening <- if (is.null(heading)) "" else paste0("<emphasis bold=\"true\">", heading, "</emphasis> ")
  xml2::read_xml(paste0(
    "<document><treatment>",
    "<subSubSection type=\"nomenclature\"><paragraph id=\"P0\">",
    "<taxonomicName genus=\"", genus, "\" species=\"", species, "\" rank=\"species\">",
    taxon, "</taxonomicName></paragraph></subSubSection>",
    "<subSubSection type=\"", type, "\"><paragraph id=\"P1\">", opening, text,
    "</paragraph></subSubSection></treatment></document>"))
}

words <- function(...) {
  found <- plaziOnomatopoeia(rendering(...), list(uuid="U", taxon="urn:taxon", article="", doi=""))
  return(found$onomatopoeia$word)
}

test_that("a marked rendering is read", {
  expect_equal(words(paste0(
    "The calling song was onomatopoeically described as ",
    "\u201czic-zic-zic, zic-zic-zic-zic\u201d ( Tinkham 1944 ).")),
    "zic-zic-zic, zic-zic-zic-zic")
  expect_equal(words("Onomatopoeically those notes can be described as \u201ccloc, cloc, cloc,\u201d."),
               "cloc, cloc, cloc")
  expect_equal(words("The young emit a shrill call that can be transcribed as \u201cbouk-bouk\u201d."),
               "bouk-bouk")
})

test_that("an unmarked description of a call is not a rendering", {
  #No marker, though the call is described and a word is quoted
  expect_equal(length(words("This was cited as a \u201charsh series of double chirps\u201d.")), 0)
  #A marker but nothing quoted
  expect_equal(length(words("The call was described onomatopoeically by Tinkham.")), 0)
})

test_that("an etymology explains a name, not a sound", {
  #A catfish named for two states its river once divided
  expect_equal(length(words(
    "The onomatopoeic Chinese sound of this species is \u201c Wu Yue Ni Chang \u201d.",
    type="etymology")), 0)
  #The same, where the treatment gives it as a run-in heading rather than a type
  expect_equal(length(words(
    "gekko from the Malay \u201cgekoq\u201d, onomatopoeic of the call of Gekko gecko.",
    heading="Etymology.")), 0)
  #And a sentence about what something is called, wherever it sits
  expect_equal(length(words(
    "The specific epithet is onomatopoeic, rendered as \u201ctuk-tuk\u201d for its call.")), 0)
})

test_that("a rendering of another taxon's call is not read as this one's", {
  #The treatment of A. nyungwensis renders the call of A. schubotzi
  expect_equal(nrow(plaziOnomatopoeia(
    xml2::read_xml(paste0(
      "<document><treatment>",
      "<subSubSection type=\"nomenclature\"><paragraph id=\"P0\">",
      "<taxonomicName genus=\"Arthroleptis\" species=\"nyungwensis\" rank=\"species\">",
      "Arthroleptis nyungwensis</taxonomicName></paragraph></subSubSection>",
      "<subSubSection type=\"discussion\"><paragraph id=\"P1\">the call of ",
      "<taxonomicName genus=\"Arthroleptis\" species=\"schubotzi\" rank=\"species\">",
      "A. schubotzi</taxonomicName>",
      " has been described onomatopoeically as \u201ccri-cri, cri-cri\u201d.",
      "</paragraph></subSubSection></treatment></document>")),
    list(uuid="U", taxon="urn:taxon", article="", doi=""))$onomatopoeia), 0)
})

test_that("a rendering is credited where the treatment credits it", {
  document <- xml2::read_xml(paste0(
    "<document><treatment>",
    "<subSubSection type=\"nomenclature\"><paragraph id=\"P0\">",
    "<taxonomicName genus=\"Aglaothorax\" species=\"ovatus\" rank=\"species\">",
    "Aglaothorax ovatus</taxonomicName></paragraph></subSubSection>",
    "<subSubSection type=\"description\"><paragraph id=\"P1\">",
    "The calling song was onomatopoeically described as \u201czic-zic\u201d (",
    "<bibRefCitation year=\"1944\" author=\"Tinkham, E. R.\">Tinkham 1944</bibRefCitation>",
    ").</paragraph></subSubSection></treatment></document>"))
  found <- plaziOnomatopoeia(document, list(uuid="U", taxon="urn:taxon", article="", doi=""))
  expect_match(found$onomatopoeia$remarks, "Tinkham 1944")
})

test_that("a rendering is about its taxon and came from its treatment", {
  found <- plaziOnomatopoeia(
    rendering("The song is rendered as \u201ctsip-tsip\u201d."),
    list(uuid="U", taxon="urn:taxon", article="", doi=""))
  expect_equal(found$links$predicate,
               c("http://purl.obolibrary.org/obo/IAO_0000136",
                 "http://purl.org/dc/terms/source"))
  expect_true(all(found$links$subject_type == "onomatopoeia"))
  #A rendering is about the taxon rather than denoting it: it is not one of the
  #names the taxon is known by
  expect_false("http://purl.obolibrary.org/obo/IAO_0000219" %in% found$links$predicate)
})

test_that("a rendering normalises to an onomatopoetic word in no stated language", {
  found <- plaziOnomatopoeia(
    rendering("The song is rendered as \u201ctsip-tsip\u201d."),
    list(uuid="U", taxon="urn:taxon", article="", doi=""))
  expect_equal(names(found$onomatopoeia), names(getHeaders("onomatopoeia")))
  normalised <- normaliseOnomatopoeia(found$onomatopoeia)
  expect_equal(normalised$kind_link, "http://purl.org/olia/olia.owl#OnomatopoeticWord")
  #A treatment does not say what language it renders a call in, and OLiA is the
  #only vocabulary that names onomatopoeia at all
  expect_true(is.na(normalised$language))
})

test_that("a name that imitates the call is a rendering, and says it is a name", {
  #audioBlast! takes acoustic content, so a taxon's names are not its to hold --
  #except one derived from the sound the animal makes, which is acoustic
  found <- plaziOnomatopoeia(
    rendering(paste("Notably, the common name of this fish in Singapore is",
                    "\u201ckekek\u201d, the onomatopoeic moniker linked to the chirping sound."),
              type="discussion"),
    list(uuid="U", taxon="urn:taxon", article="", doi=""))
  expect_equal(found$onomatopoeia$word, "kekek")
  expect_match(found$onomatopoeia$remarks, "a name the taxon is known by")
  #It is still about the taxon rather than denoting it, as every rendering is
  expect_true("http://purl.obolibrary.org/obo/IAO_0000136" %in% found$links$predicate)
  expect_false("http://purl.obolibrary.org/obo/IAO_0000219" %in% found$links$predicate)
})

test_that("a name is read wherever it sits beside the word that marks it", {
  #A rendering follows its marker ("described as zic-zic"), but a name usually
  #precedes it, so a naming sentence is read whole
  expect_equal(words(paste("Popular accounts of the mysterious \u201chaja-pau\u201d, an",
                           "onomatopoeic local name attributed to this species,",
                           "are restricted to the east.")), "haja-pau")
})

test_that("a name called onomatopoeic need not also name the sound", {
  #Calling a name onomatopoeic is the acoustic claim: it is a name because of
  #how the animal sounds. A rendering still has to be of a sound.
  expect_equal(words("Its onomatopoeic local name, \u201cjucurutu\u201d, is used in the region."),
               "jucurutu")
  expect_equal(length(words("It was rendered as \u201czic-zic\u201d in the report.")), 0)
})

test_that("an etymology section still gives up a name, but not a rendering", {
  #The section explains the epithet, and mentions in passing what people call
  #the frog; the name is wanted and the etymology is not
  expect_equal(words(paste("According to Kohn (2002), this species has a common name",
                           "given by Kichwa people: \u201cyaku telele\u201d (telele,",
                           "onomatopoeic word for the calling of some frog species)."),
                     type="etymology"), "yaku telele")
  #but a rendering in the same section is still not read
  expect_equal(length(words("The call is described onomatopoeically as \u201ccri-cri\u201d.",
                            type="etymology")), 0)
})

#Reading the names a treatment says a taxon is known by. The examples are from
#real treatments, including the ones that must not be read as names.

vernacular <- function(text, type="vernacular_names", heading=NULL,
                       taxon="Canis simensis") {
  genus <- sub(" .*", "", taxon)
  species <- sub(".* ", "", taxon)
  opening <- if (is.null(heading)) "" else paste0("<emphasis bold=\"true\">", heading, "</emphasis> ")
  document <- xml2::read_xml(paste0(
    "<document><treatment>",
    "<subSubSection type=\"nomenclature\"><paragraph id=\"P0\">",
    "<taxonomicName genus=\"", genus, "\" species=\"", species, "\" rank=\"species\">",
    taxon, "</taxonomicName></paragraph></subSubSection>",
    "<subSubSection type=\"", type, "\"><paragraph id=\"P1\">", opening, text,
    "</paragraph></subSubSection></treatment></document>"))
  return(plaziVernacularNames(document, list(uuid="U", taxon="urn:taxon", article="", doi="")))
}

test_that("a name of its own is read, in no stated language", {
  got <- vernacular("Ethiopian Wolf")$vernacularnames
  expect_equal(got$vernacularName, "Ethiopian Wolf")
  #The shape of a section is not evidence of a language, so English is not
  #assumed from an unlabelled name
  expect_true(is.na(got$language) || got$language == "")
})

test_that("a labelled list gives each name its language", {
  got <- vernacular(paste("French: Loup d'Abyssinie / German: Athiopien-Wolf /",
                          "Spanish: Lobo etiope"))$vernacularnames
  expect_equal(got$vernacularName, c("Loup d'Abyssinie", "Athiopien-Wolf", "Lobo etiope"))
  expect_equal(got$language, c("fr", "de", "es"))
})

test_that("a label welded on by the scanner does not become part of a name", {
  #Plazi reads these off the page and the slashes come back as letters
  got <- vernacular("French: Ti aenops deTrouessartl German : Trouessan I Spanish : Rinonicteno")$vernacularnames
  expect_false(any(grepl("German", got$vernacularName)))
  expect_equal(got$language[1], "fr")
})

test_that("a list of other names is read as names", {
  got <- vernacular("Ethiopian Wolf Other common names: Simien Fox , Simien Jackal")$vernacularnames
  expect_true(all(c("Simien Fox", "Simien Jackal") %in% got$vernacularName))
})

test_that("a name under a run-in heading is read", {
  got <- vernacular("Common name. Ovate Shieldback.", type="description",
                    heading="Common name.")$vernacularnames
  expect_equal(got$vernacularName, "Ovate Shieldback")
})

test_that("a name of imitative origin is a name, not a rendering", {
  got <- vernacular(paste("Notably, the common name of this fish in Singapore is",
                          "\u201ckekek\u201d, the onomatopoeic moniker linked to the chirping sound."),
                    type="discussion")$vernacularnames
  expect_equal(got$vernacularName, "kekek")
  expect_match(got$remarks, "imitates")
})

test_that("where a genus name came from is not what the animal is called", {
  #Chato, Platus, flat and gekoq are the origins of the genus name and one
  #translation of it; none of them is a name for the gecko
  got <- vernacular(paste("Etymology: A composite word from the Spanish",
                          "\u201cChato\u201d, derived from the Greek \u201cPlatus\u201d, meaning",
                          "\u201cflat\u201d; and gekko from the Malay \u201cgekoq\u201d, onomatopoeic",
                          "of the call of the species Gekko gecko."),
                    type="distribution", taxon="Chatogekko amazonicus")$vernacularnames
  expect_equal(nrow(got), 0)
})

test_that("scanner junk is not a name", {
  expect_equal(nrow(vernacular("Vernacular name: H\u57c3\u514be")$vernacularnames), 0)
  #A name written wholly in another script is still a name
  expect_equal(nrow(vernacular("Chinese: \u4e2d\u534e\u5c71\u96c0")$vernacularnames), 1)
})

test_that("prose split on its commas does not become names", {
  #A clause is not a noun phrase, so a finite verb gives it away
  expect_equal(nrow(vernacular(paste("The name is often misapplied, and the",
                                     "literature is confused as a result."))$vernacularnames), 0)
  #A names section can hold the specimens and papers a name is vouched by
  expect_equal(nrow(vernacular("Ecuador : chignul ( Aulestia 1276 , 1099 , MO!, CAS!)")$vernacularnames),
               1)
})

test_that("a vernacular name denotes its taxon", {
  got <- vernacular("Ethiopian Wolf")
  #Unlike a rendering, a name is read back onto the taxon as one of its names
  expect_true("http://purl.obolibrary.org/obo/IAO_0000219" %in% got$links$predicate)
  expect_false("http://purl.obolibrary.org/obo/IAO_0000136" %in% got$links$predicate)
  expect_true(all(got$links$subject_type == "vernacularnames"))
  expect_true("references" %in% got$links$object_type)
})

test_that("names have the columns uploadVernacularNames expects", {
  got <- vernacular("French: Loup d'Abyssinie")$vernacularnames
  expect_equal(names(got), names(getHeaders("vernacularnames")))
  normalised <- normaliseVernacularNames(got)
  expect_equal(normalised$language, "fr")
})

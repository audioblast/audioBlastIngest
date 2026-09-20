vernacularNamesFixture <- function() {
  return(test_path("fixtures", "vernacularnames.csv"))
}

#Vernacular names as ingestR() reads them from a source
readVernacularNames <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(vernacularNamesFixture(), colClasses="character", encoding="UTF-8"))
  colnames(data) <- names(getHeaders("vernacularnames"))
  return(data)
}

#A vernacular name of a source, filling in whatever isn't given
vernacularName <- function(source="bio.acousti.ca", id="32853", vernacularName="The Long-winged Conehead",
                           language="en", locality="", remarks="") {
  return(data.frame(source, id, vernacularName, language, locality, remarks,
                    stringsAsFactors=FALSE))
}

test_that("vernacular names are normalised", {
  warnings <- character(0)
  vernacular <- withCallingHandlers(normaliseVernacularNames(readVernacularNames()),
                                    warning=function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  expect_length(warnings, 2)
  expect_match(warnings[1], "2 vernacular names have no id or no name")
  expect_match(warnings[2], "1 vernacular names have a language that could not be read")
  expect_identical(names(vernacular), names(getHeaders("vernacularnames")))
  expect_equal(nrow(vernacular), 7)
  expect_identical(unique(vernacular$source), "bio.acousti.ca")
  #Names are HTML at bio.acousti.ca, and the whitespace within one is a space
  expect_identical(vernacular$vernacularName[4], "Uhler's virtuoso katydid")
  expect_identical(vernacular$vernacularName[6], "Bush-cricket & cricket")
  expect_identical(vernacular$remarks[6], "As the label has it")
  expect_identical(vernacular$locality[4], "North America")
})

test_that("a language is a BCP 47 tag, and one that isn't is left out", {
  expect_identical(languageTag(c("en", "FR", "pt-pt", "EN-gb", "zh-hans-cn", "es-419")),
                   c("en", "fr", "pt-PT", "en-GB", "zh-Hans-CN", "es-419"))
  #Anything that isn't a language, or a script or a region, can't be read
  expect_identical(languageTag(c("English", "e", "engl", "en-gbr", "", NA, "en-GB-x-old")),
                   rep(NA_character_, 7))
})

test_that("a name keeps the article a reference wrote it with", {
  vernacular <- suppressWarnings(normaliseVernacularNames(readVernacularNames()))

  expect_identical(vernacular$vernacularName[c(1, 2)],
                   c("The Long-winged Conehead", "le Criquet des pins"))
})

test_that("a language a source doesn't give is uploaded as NULL, not as no language", {
  vernacular <- suppressWarnings(normaliseVernacularNames(readVernacularNames()))

  expect_identical(vernacular$vernacularName[5], "North American hoary bat")
  expect_identical(vernacular$language[5], NA_character_)
  expect_identical(vernacular$locality[5], NA_character_)
})

test_that("normalising vernacular names again changes nothing", {
  vernacular <- suppressWarnings(normaliseVernacularNames(readVernacularNames()))

  expect_identical(normaliseVernacularNames(vernacular), vernacular)
})

test_that("names with no id, or nothing to name a taxon with, are skipped", {
  table <- rbind(
    vernacularName(),
    vernacularName(id="", vernacularName="Nothing to name it by"),
    vernacularName(id="39252", vernacularName=""),
    vernacularName(id="11111", vernacularName="<i></i>"))

  expect_warning(vernacular <- normaliseVernacularNames(table),
                 "3 vernacular names have no id or no name")

  expect_identical(vernacular$vernacularName, "The Long-winged Conehead")
})

test_that("uploadVernacularNames replaces the names of each source in one transaction", {
  table <- rbind(vernacularName(),
                 vernacularName(source="unp", id="7", vernacularName="Field cricket", language="EN"))

  upload <- mockUpload(uploadVernacularNames, table)

  #The names of both sources are deleted, and the new ones inserted by one statement
  expect_identical(upload$calls, c("begin", "execute", "execute", "execute", "commit"))
  expect_identical(upload$executed[[1]]$sql, "DELETE FROM `vernacularnames` WHERE `source` = ?")
  expect_identical(upload$executed[[1]]$params, list("bio.acousti.ca"))
  expect_identical(upload$executed[[2]]$params, list("unp"))
  columns <- names(getHeaders("vernacularnames"))
  expect_identical(upload$executed[[3]]$sql,
                   insertSQL("vernacularnames", columns, columns[-(1:2)], 2))
  rows <- boundRows(upload$executed[[3]])
  expect_identical(rows[[1]], list("bio.acousti.ca", "32853", "The Long-winged Conehead", "en",
                                   NA_character_, NA_character_))
  expect_identical(rows[[2]], list("unp", "7", "Field cricket", "en",
                                   NA_character_, NA_character_))
})

test_that("uploadVernacularNames does nothing when no names can be used", {
  upload <- suppressWarnings(mockUpload(uploadVernacularNames, vernacularName(vernacularName="")))

  expect_length(upload$calls, 0)
})

test_that("links can be made to vernacular names", {
  link <- data.frame(source="bio.acousti.ca", subject_type="vernacularnames", subject_source="",
                     subject_id="32853", predicate="http://purl.obolibrary.org/obo/IAO_0000219",
                     object_type="taxa", object_source="", object_id="58", qualifier="", remarks="",
                     stringsAsFactors=FALSE)

  links <- normaliseLinks(link)

  expect_equal(nrow(links), 1)
  expect_identical(links$subject_source, "bio.acousti.ca")
})

test_that("ingestR uploads vernacular names from vernacularnames sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="vernacularnames", url=vernacularNamesFixture(),
           process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadVernacularNames=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("vernacularnames")))
  expect_equal(nrow(uploaded), 9)
  expect_identical(unique(uploaded$source), "bio.acousti.ca")
  expect_identical(uploaded$vernacularName[1], "The Long-winged Conehead")
})

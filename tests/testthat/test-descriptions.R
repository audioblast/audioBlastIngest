descriptionsFixture <- function() {
  return(test_path("fixtures", "descriptions.csv"))
}

#Descriptions as ingestR() reads them from a source, which gives no topic link
readDescriptions <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(descriptionsFixture(), colClasses="character", encoding="UTF-8"))
  data$topic_link <- rep_len("", nrow(data))
  colnames(data) <- names(getHeaders("descriptions"))
  return(data)
}

test_that("descriptions are made plain text, and empty ones are skipped", {
  expect_warning(descriptions <- normaliseDescriptions(readDescriptions()),
                 "Skipping 1 descriptions with no id or nothing to say")

  expect_identical(names(descriptions), names(getHeaders("descriptions")))
  expect_equal(nrow(descriptions), 2)
  expect_identical(descriptions$id, c("12289", "12289.2"))
  #A node that describes more than one thing numbers the rest
  expect_identical(descriptions$topic, c("behaviour", "morphology"))
  expect_identical(descriptions$value[1],
                   "Males were reported to call 0.3-4.0m up, in shrubs along the banks of small hill-streams.")
  expect_identical(descriptions$value[2], "A small frog, the male 24\u201328mm.")
  expect_identical(descriptions$info_url[1], "https://bio.acousti.ca/node/12289")
  expect_identical(descriptions$topic_link,
                   paste0("http://rs.tdwg.org/ontology/voc/SPMInfoItems#",
                          c("Behaviour", "Morphology")))
})

test_that("a topic names the Species Profile Model info item it is", {
  spm <- "http://rs.tdwg.org/ontology/voc/SPMInfoItems#"

  expect_identical(
    spmInfoItem(c("behaviour", "Behaviour", "BEHAVIOUR", "morphology")),
    paste0(spm, c("Behaviour", "Behaviour", "Behaviour", "Morphology")))
  #However a source spaces, hyphenates or capitalises it
  expect_identical(
    spmInfoItem(c("look alikes", "Look-Alikes", "trophic strategy", "Life Cycle")),
    paste0(spm, c("LookAlikes", "LookAlikes", "TrophicStrategy", "LifeCycle")))
  #And as sources shorten the two descriptions
  expect_identical(
    spmInfoItem(c("diagnostic", "general", "diagnostic description")),
    paste0(spm, c("DiagnosticDescription", "GeneralDescription", "DiagnosticDescription")))
  #And as sources spell them: diagnosis for a diagnostic description,
  #biology_ecology for a section that is of both, behavior for Behaviour
  expect_identical(
    spmInfoItem(c("diagnosis", "biology_ecology", "behavior")),
    paste0(spm, c("DiagnosticDescription", "Biology", "Behaviour")))
  #The model defines nothing acoustic, so a topic that is of a sound is of the
  #behaviour of making it
  expect_identical(spmInfoItem(c("song", "Bioacoustics", "calling song.")),
                   paste0(spm, rep("Behaviour", 3)))
  #A word the model has no item for names nothing
  expect_identical(spmInfoItem(c("etymology", "", NA)), rep(NA_character_, 3))
})

test_that("descriptions from a source that gives no topic link are given one", {
  table <- readDescriptions()
  table <- table[, setdiff(names(table), "topic_link")]

  descriptions <- suppressWarnings(normaliseDescriptions(table))

  expect_identical(names(descriptions), names(getHeaders("descriptions")))
  expect_identical(descriptions$topic_link[1],
                   "http://rs.tdwg.org/ontology/voc/SPMInfoItems#Behaviour")
})

test_that("normalising descriptions again changes nothing", {
  descriptions <- suppressWarnings(normaliseDescriptions(readDescriptions()))

  expect_identical(normaliseDescriptions(descriptions), descriptions)
})

test_that("uploadDescriptions replaces the descriptions of each source in one transaction", {
  table <- suppressWarnings(normaliseDescriptions(readDescriptions()))
  table <- rbind(table, normaliseDescriptions(
    data.frame(source="unp", id="1", topic="behaviour", value="It calls at dusk.",
               info_url="", stringsAsFactors=FALSE)))

  upload <- mockUpload(uploadDescriptions, table)

  expect_identical(upload$calls, c("begin", "execute", "execute", "execute", "commit"))
  expect_identical(upload$executed[[1]]$sql, "DELETE FROM `descriptions` WHERE `source` = ?")
  expect_identical(upload$executed[[1]]$params, list("bio.acousti.ca"))
  expect_identical(upload$executed[[2]]$params, list("unp"))
  columns <- names(getHeaders("descriptions"))
  expect_identical(upload$executed[[3]]$sql,
                   insertSQL("descriptions", columns, columns[-(1:2)], 3))
  rows <- boundRows(upload$executed[[3]])
  expect_identical(rows[[1]][[2]], "12289")
  #A description with no page at its source has none
  expect_identical(rows[[3]][[5]], NA_character_)
})

test_that("uploadDescriptions does nothing when there is nothing to say", {
  table <- data.frame(source="bio.acousti.ca", id="1", topic="behaviour", value="",
                      info_url="", stringsAsFactors=FALSE)

  upload <- suppressWarnings(mockUpload(uploadDescriptions, table))

  expect_length(upload$calls, 0)
})

test_that("ingestR uploads descriptions from descriptions sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="descriptions", url=descriptionsFixture(),
           process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadDescriptions=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("descriptions")))
  expect_equal(nrow(uploaded), 3)
  expect_identical(unique(uploaded$source), "bio.acousti.ca")
})

test_that("a link can say what a description is about and what it rests on", {
  table <- data.frame(source="bio.acousti.ca", subject_type="descriptions", subject_source="",
                      subject_id="12289",
                      predicate=c("http://purl.obolibrary.org/obo/IAO_0000136",
                                  "http://purl.org/dc/terms/source"),
                      object_type=c("taxa", "references"), object_source="",
                      object_id=c("58", "12290"), qualifier="", remarks="",
                      stringsAsFactors=FALSE)

  links <- normaliseLinks(table)

  expect_equal(nrow(links), 2)
  expect_identical(links$subject_type, rep("descriptions", 2))
  expect_identical(links$subject_source, rep("bio.acousti.ca", 2))
})

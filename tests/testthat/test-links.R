linksFixture <- function() {
  return(test_path("fixtures", "links.csv"))
}

#Links as ingestR() reads them from a source, which says nothing about what
#established them
readLinks <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(linksFixture(), colClasses="character", encoding="UTF-8"))
  data$reference <- rep_len("", nrow(data))
  colnames(data) <- names(getHeaders("links"))
  return(data)
}

#A link from a source, filling in whatever isn't given
link <- function(source="bio.acousti.ca", subject_type="references", subject_source="",
                 subject_id="1", predicate="http://purl.org/dc/terms/relation",
                 object_type="taxa", object_source="", object_id="2", qualifier="", remarks="",
                 reference="") {
  return(data.frame(source, subject_type, subject_source, subject_id, predicate, object_type,
                    object_source, object_id, qualifier, remarks, reference,
                    stringsAsFactors=FALSE))
}

test_that("links get their source's records and an id, and repeats are left out", {
  links <- normaliseLinks(readLinks())

  expect_identical(names(links), c(names(getHeaders("links")), "id"))
  #The repeat of the Fig. 2 oscillogram link is left out, but the Fig. 3 one,
  #which differs only in its remarks, is another link
  expect_equal(nrow(links), 6)
  expect_identical(links$subject_id, c("58028", "58028", "5702", "58311", "XC123", "58028"))
  expect_identical(links$remarks[c(2, 6)], c("Fig. 2", "Fig. 3"))
  expect_identical(links$source, rep("bio.acousti.ca", 6))
  #Records without a source are the linking source's own, but terms have none
  expect_identical(links$subject_source, rep("bio.acousti.ca", 6))
  expect_identical(links$object_source,
                   c("bio.acousti.ca", "bio.acousti.ca", "bio.acousti.ca", "", "xeno-canto", "bio.acousti.ca"))
  expect_identical(links$predicate[4], "http://purl.obolibrary.org/obo/IAO_0000136")
  expect_identical(links$remarks[3], "p. 110")

  #An id is the SHA-1 of the link's subject, predicate, object, qualifier and
  #remarks, separated by line breaks, so it is the same at every ingest
  expect_identical(links$id[2], "b47be25a7355820cb062e26b0fb892c441feb763")
  expect_true(all(grepl("^[0-9a-f]{40}$", links$id)))
  expect_false(anyDuplicated(links$id) > 0)
})

test_that("links with unknown types or predicates, or without ids, are skipped with a warning", {
  table <- rbind(
    link(subject_id="1"),
    link(subject_id="2", subject_type="books"),
    link(subject_id="3", predicate="http://example.org/likes"),
    link(subject_id="4", object_id=""))

  expect_warning(links <- normaliseLinks(table), "Skipping 3 links with unknown types or predicates")

  expect_identical(links$subject_id, "1")
})

test_that("uploadLinks replaces the links of each source in one transaction", {
  table <- rbind(readLinks(), link(source="audioblast", predicate="http://www.w3.org/2004/02/skos/core#exactMatch",
                                   subject_type="recordings", subject_source="xeno-canto", object_type="recordings"))

  upload <- mockUpload(uploadLinks, table)

  #The links of both sources are deleted, and the new ones inserted by one statement
  expect_identical(upload$calls, c("begin", "execute", "execute", "execute", "commit"))
  expect_identical(upload$executed[[1]]$sql, "DELETE FROM `links` WHERE `source` = ?")
  expect_identical(upload$executed[[1]]$params, list("bio.acousti.ca"))
  expect_identical(upload$executed[[2]]$params, list("audioblast"))
  #reference says what established a link, which is a link of its own
  columns <- c("source", "id", setdiff(names(getHeaders("links"))[-1], "reference"))
  expect_identical(
    upload$executed[[3]]$sql,
    insertSQL("links", columns, columns[-(1:2)], 7))
  rows <- boundRows(upload$executed[[3]])
  expect_length(rows, 7)
  expect_identical(rows[[7]][c(1, 3:9)], list("audioblast", "recordings", "xeno-canto", "1",
                                             "http://www.w3.org/2004/02/skos/core#exactMatch",
                                             "recordings", "audioblast", "2"))
  #Empty qualifiers and remarks are uploaded as NULL
  expect_identical(rows[[3]][[which(columns == "qualifier")]], NA_character_)
  expect_identical(rows[[3]][[which(columns == "remarks")]], "p. 110")
})

test_that("uploadLinks does nothing when no links can be used", {
  upload <- suppressWarnings(mockUpload(uploadLinks, link(predicate="http://example.org/likes")))

  expect_length(upload$calls, 0)
})

test_that("ingestR uploads links from links sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="links", url=linksFixture(), process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadLinks=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("links")))
  expect_equal(nrow(uploaded), 7)
  expect_identical(unique(uploaded$source), "bio.acousti.ca")
  expect_identical(uploaded$object_id[5], "123")
})

#A link from a source, filling in whatever isn't given
cited <- function(source="bio.acousti.ca", subject_type="taxa", subject_source="",
                  subject_id="5702", predicate="http://purl.org/dc/terms/relation",
                  object_type="taxa", object_source="", object_id="399", qualifier="",
                  remarks="", reference="") {
  return(data.frame(source, subject_type, subject_source, subject_id, predicate, object_type,
                    object_source, object_id, qualifier, remarks, reference,
                    stringsAsFactors=FALSE))
}

test_that("a link that gives a reference is cited by a link of its own", {
  table <- cited(reference="58356")

  links <- normaliseLinks(table)
  citations <- citedBy(links)

  expect_equal(nrow(citations), 1)
  #The link is the subject, so what established it is said of the link itself
  expect_identical(citations$subject_type, "links")
  expect_identical(citations$subject_id, links$id)
  expect_identical(citations$predicate, "http://purl.org/dc/terms/source")
  expect_identical(citations$object_type, "references")
  expect_identical(citations$object_id, "58356")
  #A citation is a link like any other, with an id of its own
  expect_true(grepl("^[0-9a-f]{40}$", citations$id))
  expect_false(citations$id == links$id)
  #And the source that gave the link holds both ends
  expect_identical(citations$source, "bio.acousti.ca")
  expect_identical(citations$subject_source, "bio.acousti.ca")
  expect_identical(citations$object_source, "bio.acousti.ca")
})

test_that("a link with no reference is cited by nothing", {
  links <- normaliseLinks(rbind(cited(subject_id="1"), cited(subject_id="2")))

  expect_equal(nrow(citedBy(links)), 0)
})

test_that("uploadLinks uploads a citation beside the link it is of", {
  table <- rbind(cited(subject_id="5702", reference="58356"),
                 cited(subject_id="399"))

  upload <- mockUpload(uploadLinks, table)

  columns <- c("source", "id", setdiff(names(getHeaders("links"))[-1], "reference"))
  expect_false(is.element("reference", columns))
  expect_identical(upload$executed[[2]]$sql, insertSQL("links", columns, columns[-(1:2)], 3))
  rows <- boundRows(upload$executed[[2]])
  expect_length(rows, 3)
  types <- vapply(rows, `[[`, character(1), which(columns == "subject_type"))
  expect_identical(sort(types), c("links", "taxa", "taxa"))
  citation <- rows[[which(types == "links")]]
  expect_identical(citation[[which(columns == "object_id")]], "58356")
  expect_identical(citation[[which(columns == "predicate")]], "http://purl.org/dc/terms/source")
})

test_that("links sources that give no reference are given the column", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="links", url=test_path("fixtures", "links.csv"), process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadLinks=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("links")))
  expect_identical(unique(uploaded$reference), "")
})

test_that("a relationship from the vocabulary's interactions is one links can give", {
  table <- rbind(
    cited(subject_type="taxa", subject_id="8132",
          predicate="https://vocab.audioblast.org/cv/interaction#AcousticallyOrientatingParasiteOf",
          object_type="taxa", object_id="399", reference="58356"),
    #Another vocabulary's terms say what a link is of, not what it is
    cited(subject_id="1", predicate="https://vocab.audioblast.org/cv/referenceContent#Oscillogram"),
    cited(subject_id="2", predicate="https://vocab.audioblast.org/cv/interaction"))

  expect_warning(links <- normaliseLinks(table), "Skipping 2 links")

  expect_equal(nrow(links), 1)
  expect_identical(links$subject_id, "8132")
  expect_equal(nrow(citedBy(links)), 1)
})

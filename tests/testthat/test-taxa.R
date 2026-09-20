#A taxonomy of the given taxa: each is id, taxon, Rank, parent_id
taxonomy <- function(..., source="bio.acousti.ca") {
  taxa <- do.call(rbind, lapply(list(...), function(taxon) {
    data.frame(source=source, id=taxon[1], taxon=taxon[2], "Unit name 1"="", "Unit name 2"="",
               "Unit name 3"="", "Unit name 4"="", Rank=taxon[3], parent_id=taxon[4],
               parent_taxon="", stringsAsFactors=FALSE, check.names=FALSE)
  }))
  names(taxa) <- names(getHeaders("taxa"))
  return(taxa)
}

animalia <- c("1", "Animalia", "Kingdom", "0")
insecta <- c("2", "Insecta", "Class", "1")
orthoptera <- c("3", "Orthoptera", "Order", "2")
gryllidae <- c("4", "Gryllidae", "Family", "3")
gryllus <- c("5", "Gryllus", "Genus", "4")
campestris <- c("6", "Gryllus campestris", "Species", "5")

test_that("a taxon is named at its own rank and at the ranks above it", {
  taxa <- taxonomy(animalia, insecta, orthoptera, gryllidae, gryllus, campestris,
                   #An unranked taxon between a species and its subspecies
                   c("7", "Gryllus campestris group", "", "6"),
                   c("8", "Gryllus campestris minor", "Subspecies", "7"))

  out <- taxonomiseR(taxa)

  expect_identical(names(out)[1:5], c("source", "id", "taxon", "parent_id", "Rank"))
  cricket <- out[out$id == "6", ]
  expect_identical(cricket$Species, "Gryllus campestris")
  expect_identical(cricket$Genus, "Gryllus")
  expect_identical(cricket$Family, "Gryllidae")
  expect_identical(cricket$Order, "Orthoptera")
  expect_identical(cricket$Class, "Insecta")
  expect_identical(cricket$Kingdom, "Animalia")
  #A taxon above a taxon of a rank it doesn't have leaves that rank empty
  expect_identical(out[out$id == "5", "Species"], NA_character_)
  #An unranked taxon is passed through, and has no column of its own
  expect_false(is.element("", names(out)))
  expect_identical(out[out$id == "8", "Species"], "Gryllus campestris")
  expect_identical(out[out$id == "8", "Subspecies"], "Gryllus campestris minor")
})

test_that("a taxon names itself at its own rank, whatever a source nests it in", {
  #bio.acousti.ca has species whose parent is another species
  taxa <- taxonomy(animalia, insecta, orthoptera, gryllidae, gryllus, campestris,
                   c("9", "Gryllus bimaculatus", "Species", "6"))

  out <- taxonomiseR(taxa)

  expect_identical(out[out$id == "9", "Species"], "Gryllus bimaculatus")
  expect_identical(out[out$id == "9", "Genus"], "Gryllus")
})

test_that("taxa keep the source that gave them", {
  taxa <- rbind(taxonomy(animalia, insecta),
                taxonomy(c("1", "Animalia", "Kingdom", "0"), source="xeno-canto"))

  out <- taxonomiseR(taxa)

  expect_identical(out$source, c("bio.acousti.ca", "bio.acousti.ca", "xeno-canto"))
})

test_that("a missing parent or a taxon that is its own ancestor ends the walk", {
  taxa <- taxonomy(c("1", "Gryllus campestris", "Species", "404"),
                   c("2", "Gryllus", "Genus", "3"),
                   c("3", "Gryllidae", "Family", "2"))

  out <- taxonomiseR(taxa)

  expect_equal(nrow(out), 3)
  expect_identical(out[out$id == "1", "Species"], "Gryllus campestris")
  #The genus and family are each other's parents, so each keeps both names
  expect_identical(out[out$id == "2", "Genus"], "Gryllus")
  expect_identical(out[out$id == "2", "Family"], "Gryllidae")
})

test_that("uploadTaxa gives a source the ranks it doesn't use", {
  table <- taxonomiseR(taxonomy(animalia, insecta))

  upload <- mockUpload(uploadTaxa, table)

  columns <- c("source", "id", "taxon", "parent_id", "Rank", "Kingdom",
               "Subkingdom", "Phylum", "Subphylum", "Class", "Order",
               "Suborder", "Infraorder", "Superfamily", "Family", "Subfamily",
               "Tribe", "Subtribe", "Genus", "Subgenus", "Species", "Subspecies")
  expect_identical(upload$executed[[1]]$sql, insertSQL("taxa", columns, columns[-(1:2)], 2))
  rows <- boundRows(upload$executed[[1]])
  expect_identical(rows[[2]][[which(columns == "Class")]], "Insecta")
  expect_identical(rows[[2]][[which(columns == "Species")]], NA_character_)
})

test_that("uploadTaxa warns about a rank the taxa table has no column for", {
  table <- taxonomiseR(taxonomy(c("1", "Insecta", "Superorder", "0")))

  expect_warning(mockUpload(uploadTaxa, table),
                 "The taxa table has no column for the rank Superorder, so it is left out",
                 fixed=TRUE)
})

test_that("ingestR uploads taxa from taxa sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="taxa", url=test_path("fixtures", "taxa.txt"),
           process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadTaxa=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(uploaded$id, c("1", "2", "3"))
  expect_identical(unique(uploaded$source), "bio.acousti.ca")
  expect_identical(uploaded$Family, c(NA, NA, "Gryllidae"))
})

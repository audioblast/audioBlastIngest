plaziFixture <- function(name) {
  path <- test_path("fixtures", name)
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  body
}
treated <- "5B3A87E1FFAC5D4F48A2FF532C0EF9B1"

test_that("a treatment gives the classification it puts its taxon in", {
  document <- plaziRead(plaziFixture("plazi-treatment.xml"), treated)
  taxa <- plaziTaxa(document, list(uuid=treated))

  expect_identical(names(taxa), names(getHeaders("taxa")))
  #Kingdom down to the treated taxon, each inside the one above it
  expect_identical(taxa$Rank,
                   c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"))
  expect_identical(taxa$taxon[1], "Animalia")
  expect_identical(taxa$parent_id[1], "")
  for (i in 2:nrow(taxa)) expect_identical(taxa$parent_id[i], taxa$id[i - 1])

  #A species is its binomial, because Plazi's species attribute is the epithet
  #alone: affinis is a species of Neoconocephalus and one of Poecilimon
  last <- taxa[nrow(taxa), ]
  expect_identical(last$taxon, "Macroxiphus sumatranus")
  expect_identical(last$`Unit name 1`, "Macroxiphus")
  expect_identical(last$`Unit name 2`, "sumatranus")
  #A rank above the genus names itself and nothing else
  expect_identical(taxa$`Unit name 1`[taxa$Rank == "Family"], "Tettigoniidae")
  expect_identical(taxa$`Unit name 2`[taxa$Rank == "Family"], "")
})

test_that("an epithet Plazi could not parse comes from the printed name", {
  document <- plaziRead(plaziFixture("plazi-undefined.xml"), "undefined")
  taxa <- plaziTaxa(document, list(uuid="undefined"))

  #Marked up species="undefined-4", printed "Gryllus sp.4"
  last <- taxa[nrow(taxa), ]
  expect_false(grepl("undefined", last$taxon, fixed=TRUE))
  expect_identical(last$taxon, "Gryllus sp.4")
  expect_identical(last$`Unit name 2`, "sp.4")
})

test_that("a name is written once however many treatments name it", {
  seen <- new.env(hash=TRUE, parent=emptyenv())
  document <- plaziRead(plaziFixture("plazi-treatment.xml"), treated)
  taxa <- plaziTaxa(document, list(uuid=treated))

  first <- plaziFreshTaxa(taxa, seen)
  expect_identical(nrow(first), nrow(taxa))
  #The same treatment again adds nothing
  expect_identical(nrow(plaziFreshTaxa(taxa, seen)), 0L)
  expect_identical(plaziDisagreed(seen), 0L)
})

test_that("treatments that disagree about a parent are counted, not silently dropped", {
  seen <- new.env(hash=TRUE, parent=emptyenv())
  one <- getHeaders("taxa")
  one[1, ] <- list(source="", id="Mecopoda", taxon="Mecopoda", `Unit name 1`="Mecopoda",
                   `Unit name 2`="", `Unit name 3`="", `Unit name 4`="",
                   Rank="Genus", parent_id="Tettigoniidae", parent_taxon="Tettigoniidae")
  other <- one
  other$parent_id <- "Phaneropteridae"

  expect_identical(nrow(plaziFreshTaxa(one, seen)), 1L)
  #Written once, and the paper that disagrees is counted
  expect_identical(nrow(plaziFreshTaxa(other, seen)), 0L)
  expect_identical(plaziDisagreed(seen), 1L)
  #Counting the same disagreement twice does not make it two
  plaziFreshTaxa(other, seen)
  expect_identical(plaziDisagreed(seen), 1L)
})

test_that("a description is about a taxon audioBLAST holds", {
  document <- plaziRead(plaziFixture("plazi-treatment.xml"), treated)
  taxa <- plaziTaxa(document, list(uuid=treated))
  treatment <- list(uuid=treated, article="", taxon="https://www.gbif.org/species/221747866")
  links <- plaziLinks("a", treatment, taxa)

  about <- links[links$predicate == "http://purl.obolibrary.org/obo/IAO_0000136", ]
  expect_identical(about$object_type, "taxa")
  expect_identical(about$object_id, "Macroxiphus sumatranus")

  #And GBIF says which taxon that row is, of the taxon rather than of the
  #description, which is what it was always about
  same <- links[links$predicate == "http://www.w3.org/2004/02/skos/core#exactMatch", ]
  expect_identical(same$subject_type, "taxa")
  expect_identical(same$subject_id, "Macroxiphus sumatranus")
  expect_identical(same$object_id, "https://www.gbif.org/species/221747866")

  #Where Plazi named no taxon there is still the IRI to point at
  bare <- plaziLinks("a", treatment, getHeaders("taxa"))
  expect_identical(bare$object_type[1], "iri")
})

test_that("the taxa a harvest gives can be uploaded and walked", {
  document <- plaziRead(plaziFixture("plazi-treatment.xml"), treated)
  taxa <- sourceR("Plazi", plaziTaxa(document, list(uuid=treated)))

  #taxonomiseR() walks parent_id to give each taxon a column for every rank
  walked <- taxonomiseR(taxa)
  expect_identical(nrow(walked), nrow(taxa))
  species <- walked[walked$Rank == "Species", ]
  expect_identical(species$Family, "Tettigoniidae")
  expect_identical(species$Order, "Orthoptera")
  expect_identical(species$Genus, "Macroxiphus")

  #And taxa is a type a streamed harvest can upload
  expect_true("taxa" %in% names(streamUploads))
})

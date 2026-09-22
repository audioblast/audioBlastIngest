#The fixture holds what ChecklistBank answered for a handful of audioBLAST!'s
#taxa on 2026-09-21, against the Catalogue of Life's release COL26.9, keeping
#only the fields the import reads. The taxa are the ones the matching has to get
#right: two sources that disagree about a classification and two that disagree
#about a rank, a synonym, a homonym, an undescribed species, a doubted
#determination, a rank the taxonomy hasn't, a taxon of a rank the taxa table has
#no column for, and rows of a source that kept the taxonomy's ids.
colFixture <- function() {
  path <- test_path("fixtures", "col-taxa.json")
  body <- rawToChar(readBin(path, "raw", file.size(path)))
  Encoding(body) <- "UTF-8"
  return(rjson::fromJSON(body))
}

#Replays the recorded answers, keeping the paths asked for. A path that was not
#recorded is a request the import should not be making. The answers are pairs
#rather than keyed by path, because the release is asked for at the dataset
#itself, whose path is empty, and an empty name is no name.
colReplay <- function(fixture) {
  asked <- character()
  paths <- vapply(fixture$responses, function(pair) pair$path, character(1))
  fetch <- function(path) {
    asked <<- c(asked, path)
    at <- match(path, paths)
    if (is.na(at)) stop("Unexpected request: ", path)
    answer <- fixture$responses[[at]]$response
    if (isTRUE(answer$missing)) return(NULL)
    return(answer)
  }
  return(list(fetch=fetch, asked=function() asked))
}

colTaxaFrame <- function(fixture) {
  taxa <- as.data.frame(lapply(fixture$taxa, unlist), stringsAsFactors=FALSE)
  return(taxa)
}

colRun <- function() {
  fixture <- colFixture()
  replay <- colReplay(fixture)
  harvest <- colHarvest(colTaxaFrame(fixture), replay$fetch)
  harvest$asked <- replay$asked()
  harvest$outcomes <- attr(harvest$links, "outcomes")
  return(harvest)
}

#What a row was matched to, by the id it has in its own source
matchedTo <- function(harvest, source, id) {
  outcomes <- harvest$outcomes
  return(outcomes$matched[outcomes$source == source & outcomes$id == id])
}

outcomeOf <- function(harvest, source, id) {
  outcomes <- harvest$outcomes
  return(outcomes$outcome[outcomes$source == source & outcomes$id == id])
}

test_that("sources that disagree about a classification still reach one taxon", {
  harvest <- colRun()

  #bio.acousti.ca puts Aepyceros in Aepycerotinae and iNaturalist in
  #Antilopinae, and they are the same animal either way
  expect_identical(matchedTo(harvest, "bio.acousti.ca", "7785"), "PQQ")
  expect_identical(matchedTo(harvest, "iNaturalist", "42277"), "PQQ")
  #Neither source is told it is wrong, and nothing here says which is right
  expect_identical(outcomeOf(harvest, "bio.acousti.ca", "7785"), "matched")
  expect_identical(outcomeOf(harvest, "iNaturalist", "42277"), "matched")
})

test_that("sources that disagree about a rank reach one taxon, and the disagreement is kept", {
  harvest <- colRun()

  #bio.acousti.ca has Hyracoidea as a family and iNaturalist as an order
  expect_identical(matchedTo(harvest, "bio.acousti.ca", "5975"), "3DH")
  expect_identical(matchedTo(harvest, "iNaturalist", "43083"), "3DH")
  #The rank is not sent as a filter that would have lost the match; it is asked
  #for again without one, and what the taxonomy ranks it is put in the remarks
  links <- harvest$links
  family <- links$remarks[links$subject_source == "bio.acousti.ca" & links$subject_id == "5975"]
  expect_match(family, "the Catalogue of Life ranks it order, the source family", fixed=TRUE)
  expect_true(is.element("match/nameusage?q=Hyracoidea", harvest$asked))
})

test_that("a synonym is followed to the taxon it is a synonym of", {
  harvest <- colRun()

  #bio.acousti.ca's Seiurus motacilla and taxonBot's are the same warbler as
  #the taxonomy's Parkesia motacilla, and all three must meet at it
  expect_identical(matchedTo(harvest, "bio.acousti.ca", "8185"), "763KT")
  expect_identical(matchedTo(harvest, "taxonBot", "4WB6H"), "763KT")
  expect_identical(outcomeOf(harvest, "bio.acousti.ca", "8185"), "matched through a synonym")
  links <- harvest$links
  said <- links$remarks[links$subject_source == "bio.acousti.ca" & links$subject_id == "8185"]
  expect_match(said, "a synonym in the Catalogue of Life of Parkesia motacilla", fixed=TRUE)
})

test_that("a homonym is told apart by the classification its source gives", {
  harvest <- colRun()

  #Morus is a mulberry and a gannet. The matcher answers with the mulberry when
  #it is asked on its own, so a bioacoustic archive's Morus must be asked for
  #under its family.
  expect_identical(matchedTo(harvest, "bio.acousti.ca", "99001"), "5VCR")
  expect_true(is.element("match/nameusage?q=Morus&rank=genus&family=Sulidae", harvest$asked))
})

test_that("a name that is not a determination is never looked up", {
  harvest <- colRun()

  #An undescribed species and a determination its author doubted
  expect_identical(outcomeOf(harvest, "bio.acousti.ca", "798"), "not a determination")
  expect_identical(outcomeOf(harvest, "bio.acousti.ca", "176"), "not a determination")
  expect_identical(matchedTo(harvest, "bio.acousti.ca", "798"), "")
  #The taxonomy takes the question mark out of Ephippiger ?ephippiger and
  #answers with the species, so asking at all would turn a doubt into a
  #statement that the two are the same
  expect_false(any(grepl("Ephippiger", harvest$asked)))
  expect_false(any(grepl("urchip", harvest$asked)))
})

test_that("a rank the taxonomy does not rank taxa by is not looked up", {
  harvest <- colRun()

  #iNaturalist's complex is a group of species too alike to tell apart, and the
  #species of the same name is a narrower thing
  expect_identical(outcomeOf(harvest, "iNaturalist", "1563720"),
                   "rank complex is not one the Catalogue of Life ranks taxa by")
  expect_false(any(grepl("Anaxipha", harvest$asked)))
})

test_that("the taxa imported are a tree, including the ranks the taxa table cannot hold", {
  harvest <- colRun()
  taxa <- harvest$taxa
  by <- function(id) taxa[taxa$id == id, ]

  #Acridoidea is a superfamily, and no source can place it: the taxa table has
  #no column for a superfamily, an infraorder or a suborder. Its parent can.
  expect_identical(by("8NKG2")$taxon, "Acridoidea")
  expect_identical(by("8NKG2")$Rank, "Superfamily")
  expect_identical(by("8NKG2")$parent_id, "8NKFS")
  expect_identical(by("8NKFS")$taxon, "Acrididea")
  expect_identical(by("8NKFS")$Rank, "Infraorder")
  expect_identical(by("8NKFS")$parent_id, "8NKFL")
  expect_identical(by("8NKFL")$taxon, "Caelifera")
  expect_identical(by("8NKFL")$Rank, "Suborder")
  #The chain runs all the way up, and the taxon at the top has no parent
  expect_identical(by("CJBKK")$taxon, "Orthoptera")
  expect_identical(by("CS5HF")$taxon, "Eukaryota")
  expect_identical(by("CS5HF")$parent_id, "")
  #A taxon reached by more than one row is held once
  expect_identical(sum(taxa$id == "N"), 1L)
  expect_identical(by("N")$taxon, "Animalia")
  #Ranks are titled as the taxa table names its columns, whatever case the
  #taxonomy writes them in
  expect_true(all(taxa$Rank == "" | grepl("^[A-Z][a-z]+$", taxa$Rank)))
  #The source is left empty for sourceR() to fill, as every harvest does
  expect_true(all(taxa$source == ""))
})

test_that("a source that kept the taxonomy's ids is looked up by them, stale ones included", {
  harvest <- colRun()

  #An id that is still the taxon it was is taken as it stands, by one request
  #for the name usage and one for its classification
  expect_identical(matchedTo(harvest, "taxonBot", "322KY"), "322KY")
  expect_true(is.element("nameusage/322KY", harvest$asked))
  expect_false(any(grepl("q=Cryptospiza", harvest$asked)))
  #An id that has become a synonym since it was read in is followed to the
  #taxon it is now a synonym of, so that the row still meets the other sources
  expect_identical(matchedTo(harvest, "taxonBot", "5B475"), "3T2Q8")
  expect_identical(outcomeOf(harvest, "taxonBot", "5B475"), "matched through a synonym")
  #The classification asked for is the accepted taxon's, not the synonym's
  expect_true(is.element("taxon/3T2Q8/classification", harvest$asked))
  expect_false(is.element("taxon/5B475/classification", harvest$asked))
})

test_that("every row is linked to the taxon it is and every taxon to the catalogue", {
  harvest <- colRun()
  links <- harvest$links
  exactMatch <- "http://www.w3.org/2004/02/skos/core#exactMatch"

  expect_true(all(links$predicate == exactMatch))
  #A row of another source is linked to a row of this import, so a client
  #resolves it inside audioBLAST! rather than leaving for ChecklistBank
  inside <- links[links$object_type == "taxa", ]
  expect_identical(sort(unique(inside$subject_source)),
                   c("bio.acousti.ca", "iNaturalist", "taxonBot"))
  expect_true(all(is.element(inside$object_id, harvest$taxa$id)))
  #The object names no source, so that the source this is uploaded under fills
  #it in, and the subject names the source holding the row
  expect_true(all(inside$object_source == ""))
  expect_true(all(nzchar(inside$subject_source)))
  #Each imported taxon is linked to the catalogue's own address for it as well,
  #so that what audioBLAST! holds joins to anything else citing the same taxon
  outward <- links[links$object_type == "iri", ]
  expect_identical(nrow(outward), nrow(harvest$taxa))
  expect_true(all(outward$subject_source == ""))
  expect_identical(outward$object_id[outward$subject_id == "PQQ"],
                   "https://api.checklistbank.org/dataset/3LR/taxon/PQQ")
  #Every link says which release it was matched against, so that a later import
  #can tell a taxon that has moved from a match that was wrong
  expect_true(all(grepl("COL26.9$", links$remarks)))
})

test_that("a second import leaves the rows the first one wrote alone", {
  fixture <- colFixture()
  #On every import after the first, the taxa read back from audioBLAST! include
  #the rows this import wrote last time. They are the taxonomy itself: looking
  #them up would match each to itself, write a link saying a taxon is the same
  #taxon as itself, and spend a request apiece finding that out.
  taxa <- colTaxaFrame(fixture)
  spine <- taxa[1, ]
  spine$source <- "CoL"; spine$id <- "PQQ"; spine$taxon <- "Aepyceros"

  first <- colReplay(fixture)
  without <- colHarvest(taxa, first$fetch)
  again <- colReplay(fixture)
  with <- colHarvest(rbind(taxa, spine), again$fetch)

  #The spine row is not looked up, not matched and not linked
  expect_false(is.element("CoL", attr(with$links, "outcomes")$source))
  inside <- with$links[with$links$object_type == "taxa", ]
  #No link whose subject is a row of this import, which is what a link from a
  #taxon to itself would be. A row of another source that kept the taxonomy's
  #id does have subject_id == object_id, and is a different record for all that
  expect_false(any(inside$subject_source == "CoL"))
  expect_true(any(inside$subject_source == "taxonBot" & inside$subject_id == inside$object_id))
  #and it costs nothing: the same requests, and the same import, either way
  expect_identical(again$asked(), first$asked())
  expect_identical(with$taxa, without$taxa)
  expect_identical(with$links, without$links)
})

test_that("the taxa and the links are what the uploaders take", {
  harvest <- colRun()

  taxa <- suppressWarnings(taxonomiseR(sourceR("CoL", harvest$taxa)))
  expect_identical(unique(taxa$source), "CoL")
  #The flat columns are filled where the taxa table has one for the rank
  cricket <- taxa[taxa$id == "8NKG2", ]
  expect_identical(cricket$Superfamily, "Acridoidea")
  expect_identical(cricket$Order, "Orthoptera")
  expect_identical(cricket$Class, "Insecta")
  #and the tree holds the ranks it has no column for
  expect_identical(cricket$parent_id, "8NKFS")
  #A rank with no column is reported rather than quietly dropped
  expect_warning(mockUpload(uploadTaxa, taxa),
                 "The taxa table has no column for the rank", fixed=TRUE)

  #normaliseLinks keeps every link: the predicate and both types are ones it
  #knows, and the empty object source is filled in with the uploading source
  links <- normaliseLinks(sourceR("CoL", harvest$links))
  expect_identical(nrow(links), nrow(harvest$links))
  inside <- links[links$object_type == "taxa", ]
  expect_true(all(inside$object_source == "CoL"))
  expect_true(all(nzchar(links$id)))
})

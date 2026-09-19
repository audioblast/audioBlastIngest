vocabFixture <- function(name) {
  path <- test_path("fixtures", paste0("vocab-", name, ".jsonld"))
  return(readBin(path, "raw", file.size(path)))
}
vocabJSON <- function(name) {
  body <- rawToChar(vocabFixture(name))
  Encoding(body) <- "UTF-8"
  return(rjson::fromJSON(body))
}
fixtureCallTypes <- function() vocabTermsJSONLD(vocabJSON("callType"))
fixtureTerms <- function() vocabTermsJSONLD(vocabJSON("site"))
callType <- function(term) paste0("https://vocab.audioblast.org/cv/callType#", term)
abv <- function(term) paste0("https://vocab.audioblast.org/", term)

test_that("a vocabulary's terms are found by their names and synonyms", {
  callTypes <- fixtureCallTypes()
  expect_identical(callTypes[["calling song"]], callType("CallingSong"))
  #Synonyms are given as alternative labels, as a list or a single value
  expect_identical(callTypes[["proclamation song"]], callType("CallingSong"))
  expect_identical(callTypes[["serenade song"]], callType("CourtshipSong"))
  expect_identical(callTypes[["aggressive song"]], callType("RivalryCall"))
  #and deprecated terms, such as synonyms, are left out
  expect_false(callType("ProclamationSong") %in% callTypes)

  terms <- fixtureTerms()
  expect_identical(terms[["pattern of component calls"]], abv("CallPattern"))
  expect_identical(terms[["call type"]], abv("CallType"))
  expect_false(abv("") %in% terms)
})

test_that("names are matched regardless of case and spacing, and call types with call or song", {
  expect_identical(labelKey(c(" Echeme  Duration ", "PEAK frequency")), c("echeme duration", "peak frequency"))
  expect_identical(callTypeKey(c("Calling Call", "Calling Song", "Rivalry Call", "Call", "Flight Noise")),
                   c("calling song", "calling song", "rivalry song", "song", "flight noise"))
})

test_that("call types are split into the type, part and qualifiers", {
  parsed <- parseCallType(c("Courtship Call B", "Calling Call (Night)", "Courtship Call B Echeme A",
                            "Calling Call (section a)", "Territorial II", "Calling Song Type I",
                            "Calling Call (Echeme Sequence)", "Call", "", NA))
  expect_identical(parsed$type, c("Courtship Call", "Calling Call", "Courtship Call", "Calling Call", "Territorial",
                                  "Calling Song", "Calling Call", "Call", NA, NA))
  expect_identical(parsed$part, c("B", NA, "B", "a", NA, NA, NA, NA, NA, NA))
  expect_identical(parsed$qualifier, c(NA, "Night", "Echeme A", NA, "II", "Type I", "Echeme Sequence", NA, NA, NA))
  expect_identical(nrow(parseCallType(character(0))), 0L)
})

#Traits with the columns of the traits table, and every other column empty
traitsTable <- function(...) {
  values <- data.frame(..., stringsAsFactors=FALSE, check.names=FALSE)
  table <- as.data.frame(lapply(setNames(nm=names(getHeaders("traits"))), function(column) rep_len("", nrow(values))),
                         stringsAsFactors=FALSE, check.names=FALSE)
  table[names(values)] <- values
  return(table)
}

test_that("traits are linked to the vocabulary's terms", {
  traits <- traitsTable(
    traitID=as.character(1:5),
    Trait=c("Echeme Duration", "Echeme Duration", "Pattern of Component Calls", "Syllable Duration", "Echeme Duration"),
    Ontology.Link=c(abv("EchemeDuration"), "", "", "", "http://vocab.audioblast.org/Frequency"),
    Value=c("5-10", "<15", "AB", "0.08", "3"),
    Call.Type=c("Courtship Call B", "Calling Call (Night)", "Courtship Call", "Call", "Advertisement Call"))

  linked <- linkTraits(traits, fixtureCallTypes(), fixtureTerms())

  expect_identical(linked$Call.Type.Link, c(callType("CourtshipSong"), callType("CallingSong"), callType("CourtshipSong"), NA, NA))
  expect_identical(linked$Call.Part, c("B", NA, NA, NA, NA))
  expect_identical(linked$Call.Qualifier, c(NA, "Night", NA, NA, NA))
  #Only traits without a link are linked, by their names
  expect_identical(linked$Ontology.Link, c(abv("EchemeDuration"), abv("EchemeDuration"), abv("CallPattern"), "",
                                           "http://vocab.audioblast.org/Frequency"))
  #A pattern without separators gets them
  expect_identical(linked$Value, c("5-10", "<15", "A:B", "0.08", "3"))
  #and the call type as written is kept
  expect_identical(linked$Call.Type, traits$Call.Type)
})

test_that("vocabularies are read from vocab.audioblast.org as JSON-LD", {
  urls <- character(0)
  local_mocked_bindings(curl_fetch_memory=function(url, handle) {
    urls <<- c(urls, url)
    list(status_code=200, content=vocabFixture("callType"))
  })

  terms <- vocabTerms("cv/callType")

  expect_identical(urls, "https://vocab.audioblast.org/cv/callType")
  expect_identical(terms, fixtureCallTypes())
})

test_that("a vocabulary that can't be read is an error", {
  local_mocked_bindings(curl_fetch_memory=function(url, handle) list(status_code=503, content=charToRaw("down")))

  expect_error(vocabTerms(), "https://vocab.audioblast.org/ could not be read (HTTP 503)", fixed=TRUE)
})

#Runs ingestR on a traits source laid out as BioAcoustica's traits.txt, with the
#vocabulary read by vocab, returning the traits it uploads
ingestTraits <- function(vocab) {
  csv <- tempfile(fileext=".csv")
  source <- data.frame(traitID=c("39548", "39549", "39550"), taxonID="102",
                       "Taxonomic name"="Stenobothrus rubicundulus",
                       Trait=c("Echeme Duration", "Pattern of Component Calls", "Song Structure"),
                       "Ontology Link"=c(abv("EchemeDuration"), "", ""), Value=c("5", "ABC", "Echeme"),
                       "Call Type"=c("Courtship Call B", "Courtship Call", "Courtship Call C"), Sex="Male",
                       Temperature="", Reference="", Cascade="0", "Annotation ID"="", check.names=FALSE)
  write.csv(source, csv, row.names=FALSE)

  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(list(name="bio.acousti.ca", type="traits", url=csv, process=list("sourceR"))),
    vocabTerms=vocab,
    uploadTraits=function(db, table) uploaded <<- table)
  ingestR(db="db")
  unlink(csv)
  return(uploaded[order(uploaded$traitID), ])
}

test_that("ingestR links traits to the vocabulary before uploading them", {
  uploaded <- ingestTraits(function(vocabulary="", ...) {
    if (vocabulary == "cv/callType") fixtureCallTypes() else fixtureTerms()
  })

  expect_identical(uploaded$source, rep("bio.acousti.ca", 3))
  expect_identical(uploaded$Call.Type.Link, rep(callType("CourtshipSong"), 3))
  expect_identical(uploaded$Call.Part, c("B", NA, "C"))
  expect_identical(uploaded$Ontology.Link, c(abv("EchemeDuration"), abv("CallPattern"), ""))
  expect_identical(uploaded$Value, c("5", "A:B:C", "Echeme"))
})

test_that("ingestR uploads traits unlinked when the vocabulary can't be read", {
  expect_warning(
    uploaded <- ingestTraits(function(...) stop("https://vocab.audioblast.org/ could not be read (HTTP 503)")),
    "Traits not linked to vocab.audioblast.org")

  expect_identical(uploaded$traitID, c("39548", "39549", "39550"))
  expect_identical(uploaded$Call.Type.Link, c("", "", ""))
  expect_identical(uploaded$Value, c("5", "ABC", "Echeme"))
})

test_that("uploadTraits uploads the call's part, link and qualifier, and NULL for none", {
  table <- traitsTable(source="bio.acousti.ca", traitID=c("1", "2"), Call.Part=c("B", ""),
                       Call.Type.Link=c(callType("CourtshipSong"), ""))
  #Traits from before these columns were added don't have them
  old <- table[1:13]

  upload <- mockUpload(uploadTraits, table)
  uploadOld <- mockUpload(uploadTraits, old)

  rows <- boundRows(upload$executed[[1]])
  expect_identical(rows[[1]][14:16], list("B", callType("CourtshipSong"), NA_character_))
  expect_identical(rows[[2]][14:16], list(NA_character_, NA_character_, NA_character_))
  expect_identical(upload$executed[[1]]$sql, uploadOld$executed[[1]]$sql)
  expect_identical(boundRows(uploadOld$executed[[1]])[[1]][14:16], list(NA_character_, NA_character_, NA_character_))
})

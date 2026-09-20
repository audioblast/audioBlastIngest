osfFixture <- function() {
  rjson::fromJSON(file=test_path("fixtures", "orthoptera-page.json"))
}

test_that("all four indirect OSF links resolve using recorded public responses", {
  fixture <- rjson::fromJSON(file=test_path("fixtures", "orthoptera-indirect.json"))
  paths <- character()
  fetch <- function(path) {
    paths <<- c(paths, path)
    response <- fixture$responses[[path]]
    if (is.null(response)) stop("Unexpected request: ", path)
    response
  }
  lookup <- function(id) fetch(paste0("otus/", id, "?extend[]=taxon_name"))$data$taxon_name$cached
  resolver <- orthopteraRelatedTaxa(fetch, lookup)
  data <- orthopteraRecordings(fixture$sounds, lookup, relatedTaxa=resolver)
  expect_identical(data$id, c("125", "126", "199", "208"))
  expect_identical(data$taxon, c("Chorthippus (Glyptobothrus) eisentrauti",
                               "Chorthippus (Glyptobothrus) eisentrauti",
                               "Chorthippus (Altichorthippus) intermedius", "Parasubria vittipes"))
  expect_true(all(grepl("/otus/[0-9]+$", data$info_url)))
  paths <- character()
  invisible(lapply(fixture$sounds, resolver))
  expect_length(paths, 0)
})

test_that("events page through both occurrence types and reject mixed taxa", {
  sound <- list(name="field recording", conveyances=list(list(
    conveyance_object_type="CollectingEvent", conveyance_object_id=10)))
  paths <- character()
  fetch <- function(path) {
    paths <<- c(paths, path)
    if (grepl("collection_object_query", path)) return(list(data=list(), total_pages="0"))
    list(data=list(list(otu_id=if (grepl("page=1$", path)) 1 else 2)), total_pages="2")
  }
  resolver <- orthopteraRelatedTaxa(fetch, function(id) paste("Taxon", id))
  expect_identical(resolver(sound), character())
  expect_length(paths, 3)
  expect_match(paths[3], "page=2$")
  #Missing identifications must not make an event appear unambiguous.
  resolver <- orthopteraRelatedTaxa(function(path) list(
    data=list(list(otu_id=1), list()), total_pages="1"), function(id) "Taxon")
  expect_identical(resolver(sound), character())
})

test_that("title fallback requires a unique exact accepted taxon", {
  sound <- list(name="Parasubria vittipes 4 field recording", conveyances=list(list(
    conveyance_object_type="CollectingEvent", conveyance_object_id=10)))
  candidates <- list(list(id=1, taxon_name=list(cached="Parasubria vittipes", cached_is_valid=TRUE)))
  fetch <- function(path) list(data=if (startsWith(path, "otus?")) candidates else list(), total_pages="1")
  resolve <- function() orthopteraRelatedTaxa(fetch, function(id) "unused")(sound)
  expect_identical(resolve(), "1")
  candidates[[1]]$taxon_name$cached_is_valid <- FALSE
  expect_identical(resolve(), character())
  candidates[[1]]$taxon_name$cached_is_valid <- TRUE
  candidates[[1]]$taxon_name$cached <- "Different species"
  expect_identical(resolve(), character())
  candidates[[1]]$taxon_name$cached <- "Parasubria vittipes"
  candidates <- c(candidates, candidates)
  expect_identical(resolve(), character())
  sound$name <- "Parasubria vittipes subspecies 4 field recording"
  expect_identical(resolve(), character())
})

test_that("indirect lookups fail on incomplete pagination", {
  expect_error(orthopteraPages("dwc_occurrences?x=1", function(path)
    list(data=list(), total_pages="2")), "empty.*lookup page")
  expect_error(orthopteraPages("dwc_occurrences?x=1", function(path)
    list(data=list(), total_pages=NULL)), "pagination")
})

test_that("OSF maps live response fields without inventing recording metadata", {
  data <- orthopteraRecordings(osfFixture(), function(id) {
    c(`804734`="Aglaothorax segnis", `810653`="Stethophyma grossum")[[id]]
  })
  expect_identical(names(data), names(getHeaders("recordings")))
  expect_true(all(vapply(data, is.character, logical(1))))
  expect_identical(data$id, c("44", "62"))
  expect_identical(data$taxon, c("Aglaothorax segnis", "Stethophyma grossum"))
  expect_identical(data$file[1], "https://sfg.taxonworks.org/s/klud2b")
  expect_match(data$author[1], "Jeffrey A. Cole", fixed=TRUE)
  expect_identical(data$post_date[1], "2025-08-27")
  expect_identical(data$Duration[1], "10.014")
  expect_identical(data$info_url[1], "https://orthoptera.speciesfile.org/otus/804734")
  expect_true(all(data[c("Date", "Time", "lat", "lon", "license", "type")] == ""))
})

test_that("empty, unavailable and multiply linked sounds are handled", {
  empty <- orthopteraRecordings(list(), function(id) stop("unexpected lookup"))
  expect_equal(nrow(empty), 0)
  expect_identical(names(empty), names(getHeaders("recordings")))
  sounds <- osfFixture()
  sounds[[1]]$sound_file <- NULL
  sounds[[2]]$metadata$error <- "Missing sound file"
  expect_equal(nrow(orthopteraRecordings(sounds, function(id) stop("unexpected lookup"))), 0)
  sound <- osfFixture()[1]
  sound[[1]]$conveyances <- c(sound[[1]]$conveyances,
    list(list(conveyance_object_type="Otu", conveyance_object_id=123),
         list(conveyance_object_type="CollectionObject", conveyance_object_id=456)))
  data <- orthopteraRecordings(sound, function(id) paste("Taxon", id))
  expect_identical(data$taxon, "Taxon 804734;Taxon 123")
  sound[[1]]$conveyances <- NULL
  expect_identical(orthopteraRecordings(sound, function(id) stop("lookup"))$taxon, "")
})

test_that("paging deduplicates sounds and caches OTUs", {
  paths <- character()
  local_mocked_bindings(orthopteraFetch=function(path, ...) {
    paths <<- c(paths, path)
    if (startsWith(path, "otus/")) return(list(data=list(id=804734, taxon_name=list(cached="Aglaothorax segnis"))))
    sound <- osfFixture()[1]
    if (grepl("page=2", path)) sound <- c(sound, sound)
    list(data=sound, total_pages="2")
  })
  data <- orthopteraSpeciesFileR(pause=0)
  expect_identical(data$id, "44")
  expect_length(paths, 3)
  expect_equal(sum(startsWith(paths, "otus/")), 1)
  expect_match(paths[3], "page=2", fixed=TRUE)
})

test_that("OSF rejects invalid arguments and incomplete pagination", {
  expect_error(orthopteraSpeciesFileR(per_page=1.5), "per_page")
  expect_error(orthopteraSpeciesFileR(token=""), "token")
  expect_error(orthopteraSpeciesFileR(pause=-1), "pause")
  local_mocked_bindings(orthopteraFetch=function(...) list(data=list(), total_pages=NULL))
  expect_error(orthopteraSpeciesFileR(pause=0), "pagination")
})

test_that("specimen recordings use only accepted determinations and cache lookups", {
  paths <- character()
  local_mocked_bindings(orthopteraFetch=function(path, ...) {
    paths <<- c(paths, path)
    if (startsWith(path, "otus/")) return(list(data=list(id=123, taxon_name=list(cached="Accepted taxon"))))
    if (startsWith(path, "collection_objects/")) return(list(data=list(id=456,
      taxon_determinations=list(list(position=2, otu_id=999), list(position=1, otu_id=123)))))
    sound <- osfFixture()[1]
    sound[[1]]$conveyances <- list(list(conveyance_object_type="CollectionObject", conveyance_object_id=456))
    list(data=c(sound, sound), total_pages="1")
  })
  data <- orthopteraSpeciesFileR(pause=0)
  expect_identical(data$taxon, "Accepted taxon")
  expect_identical(data$info_url, "https://orthoptera.speciesfile.org/otus/123")
  expect_length(paths, 3)
  expect_false(any(grepl("999", paths)))
})

test_that("OSF HTTP errors retry selectively and parse pagination", {
  calls <- 0
  local_mocked_bindings(curl_fetch_memory=function(...) {
    calls <<- calls + 1
    if (calls == 1) return(list(status_code=503))
    list(status_code=200, content=charToRaw("[]"),
         headers=charToRaw("HTTP/2 200\r\nPagination-Total-Pages: 0\r\n"))
  })
  response <- orthopteraFetch("sounds?page=1", "public", NULL, backoff=0)
  expect_identical(response$data, list())
  expect_identical(response$total_pages, "0")
  expect_equal(calls, 2)
  local_mocked_bindings(curl_fetch_memory=function(...) list(status_code=401))
  expect_error(orthopteraFetch("sounds", "public", NULL, backoff=numeric()), "HTTP 401")
  local_mocked_bindings(curl_fetch_memory=function(...) list(status_code=200, content=charToRaw("<html>")))
  expect_error(orthopteraFetch("sounds", "public", NULL, backoff=numeric()), "invalid JSON")
})

test_that("ingestR integrates OSF and skips failed harvests", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(list(name="orthoptera-speciesfile", type="recordings",
                                    orthoptera=list(pause=0), process="sourceR")),
    orthopteraSpeciesFileR=function(...) orthopteraRecordings(osfFixture(), function(id) "Taxon"),
    uploadTraits=function(...) NULL,
    uploadRecordings=function(db, table) uploaded <<- table)
  ingestR(db="db")
  expect_identical(uploaded$source, rep("orthoptera-speciesfile", 2))
  uploaded <- NULL
  local_mocked_bindings(orthopteraSpeciesFileR=function(...) stop("harvest failed"))
  expect_warning(ingestR(db="db"), "Skipping source orthoptera-speciesfile - harvest failed")
  expect_null(uploaded)
})

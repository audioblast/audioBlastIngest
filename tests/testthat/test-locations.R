locationsFixture <- function() {
  return(test_path("fixtures", "locations.csv"))
}

#Locations as ingestR() reads them from a source
readLocations <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(locationsFixture(), colClasses="character", encoding="UTF-8"))
  colnames(data) <- names(getHeaders("locations"))
  return(data)
}

test_that("locations are normalised", {
  normalised <- normaliseLocations(readLocations())

  expect_identical(names(normalised), names(getHeaders("locations")))
  expect_identical(normalised$source, rep("bio.acousti.ca", 3))
  #A place is named as its source names it, double space and all
  expect_identical(normalised$name,
                   c("Chapman's Pool, Dorset", "Corsica: Corse-du-Sud; 20km N of Ajaccio",
                     "BMNH Acoustic  Laboratory"))
  #What a source calls a continent or an island is left as it is
  expect_identical(normalised$continent, rep("Europe", 3))
  expect_identical(normalised$island, c(NA, "Corsica", NA))
  #A country is read whether it is coded or named, but a place that is no
  #country, such as the city this one gives, can't be
  expect_identical(normalised$countryCode, c("GB", "FR", NA))
  expect_identical(normalised$decimalLatitude, c("50.60165359291455", "42.12814780545614", NA))
  expect_identical(normalised$coordinateUncertaintyInMeters, c(NA, "1000", NA))
  expect_identical(normalised$minimumElevationInMeters, c(NA, "20", NA))
  expect_identical(normalised$maximumElevationInMeters, c(NA, "1796", NA))
  expect_identical(normalised$info_url, c("https://bio.acousti.ca/node/11150",
                                          "https://bio.acousti.ca/node/11155", NA))
})

test_that("normalising locations again changes nothing", {
  normalised <- normaliseLocations(readLocations())

  expect_identical(normaliseLocations(normalised), normalised)
})

test_that("elevations are read above and below sea level", {
  expect_identical(
    decimalNumber(c("1796", "-410", "0", "12.5", " 20 ", "", "sea level", NA)),
    c("1796", "-410", "0", "12.5", "20", NA, NA, NA))
})

test_that("ingestR uploads locations from locations sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="locations", url=locationsFixture(), process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadLocations=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("locations")))
  expect_equal(nrow(uploaded), 3)
  expect_identical(unique(uploaded$source), "bio.acousti.ca")
  expect_identical(uploaded$id, c("11150", "11155", "11164"))
})

test_that("a link can say which place a record is from", {
  table <- data.frame(source="bio.acousti.ca", subject_type="recordings", subject_source="",
                      subject_id="8961", predicate="http://rs.tdwg.org/dwc/iri/inDescribedPlace",
                      object_type="locations", object_source="", object_id="11150",
                      qualifier="", remarks="", stringsAsFactors=FALSE)

  links <- normaliseLinks(table)

  expect_equal(nrow(links), 1)
  expect_identical(links$object_type, "locations")
  expect_identical(links$object_source, "bio.acousti.ca")
})

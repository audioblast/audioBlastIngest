#Specimens with the given values, and every other column empty
specimensTable <- function(...) {
  values <- data.frame(..., stringsAsFactors=FALSE, check.names=FALSE)
  columns <- names(getHeaders("specimens"))
  table <- as.data.frame(lapply(setNames(nm=columns), function(column) rep_len("", nrow(values))),
                         stringsAsFactors=FALSE, check.names=FALSE)
  table[names(values)] <- values
  return(table)
}

#Specimens as ingestR() reads them from a source
readSpecimens <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(test_path("fixtures", "specimens.csv"),
                                   colClasses="character", encoding="UTF-8"))
  colnames(data) <- names(getHeaders("specimens"))
  return(data)
}

test_that("specimens are normalised", {
  normalised <- normaliseSpecimens(readSpecimens())

  expect_identical(names(normalised), names(getHeaders("specimens")))
  expect_identical(normalised$source, rep("bio.acousti.ca", 3))
  expect_identical(normalised$scientificName,
                   c("Conocephalus discolor", "Platycleis affinis", "Gryllus campestris"))
  #What a source calls a sex or a life stage is left as it is
  expect_identical(normalised$sex, c("Male", "Female", NA))
  expect_identical(normalised$basisOfRecord,
                   c("LivingSpecimen", "HumanObservation", "PreservedSpecimen"))
  expect_identical(normalised$eventDate, c("1962-08-18", "1958-08-03", NA))
  expect_identical(normalised$dateIdentified, c(NA, "1958", NA))
  expect_identical(normalised$individualCount, c("1", "2", NA))
  #A country is read whether the source codes it or names it
  expect_identical(normalised$countryCode, c("GB", "ES", "ES"))
  expect_identical(normalised$decimalLatitude, c("50.6016535929146", NA, NA))
  expect_identical(normalised$decimalLongitude, c("-1.95290565490723", NA, NA))
  #Remarks are HTML at bio.acousti.ca
  expect_identical(normalised$occurrenceRemarks,
                   c("On reeds.", "Caught at light & released", NA))
  expect_identical(normalised$info_url, c("https://bio.acousti.ca/node/11158", NA, NA))
})

test_that("normalising specimens again changes nothing", {
  normalised <- normaliseSpecimens(readSpecimens())

  expect_identical(normaliseSpecimens(normalised), normalised)
})

test_that("specimen dates that can't be read are warned of and left out", {
  table <- specimensTable(id=c("1", "2", "3"),
                          eventDate=c("18/08/1962", "18/08/1962", "1962-08-18"))

  expect_warning(
    normalised <- normaliseSpecimens(table),
    '2 specimens have a eventDate that could not be read, so it is left out, e.g. "18/08/1962"',
    fixed=TRUE)
  expect_identical(normalised$eventDate, c(NA, NA, "1962-08-18"))
})

test_that("ingestR uploads specimens from specimens sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="specimens",
           url=test_path("fixtures", "specimens.csv"), process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadSpecimens=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("specimens")))
  expect_equal(nrow(uploaded), 3)
  expect_identical(unique(uploaded$source), "bio.acousti.ca")
  expect_identical(uploaded$catalogNumber[1], "15?")
})

test_that("counts and country codes are normalised", {
  expect_identical(
    wholeNumber(c("1", "12", "2.0", "1e3", "0", "-1", "1.5", "", "many", NA)),
    c("1", "12", "2", "1000", NA, NA, NA, NA, NA, NA))
  #Codes in whatever case, and the names sources give countries; an alpha-3
  #code is neither, so it is left out rather than guessed at
  expect_identical(
    countryCode(c("GB", "es", " za ", "GBR", "Spain", "", NA)),
    c("GB", "ES", "ZA", NA, "ES", NA, NA))
})

onomatopoeiaFixture <- function() {
  return(test_path("fixtures", "onomatopoeia.csv"))
}

#Onomatopoeia as ingestR() reads them from a source, which gives every column
#but the last: kind_link is read from the kind rather than given
readOnomatopoeia <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(onomatopoeiaFixture(), colClasses="character", encoding="UTF-8"))
  headers <- names(getHeaders("onomatopoeia"))
  data[[headers[length(headers)]]] <- rep_len("", nrow(data))
  colnames(data) <- headers
  return(data)
}

onomatopoeticWord <- "http://purl.org/olia/olia.owl#OnomatopoeticWord"

test_that("onomatopoeia are normalised", {
  warnings <- character(0)
  onomatopoeia <- withCallingHandlers(normaliseOnomatopoeia(readOnomatopoeia()),
                                      warning=function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  expect_length(warnings, 2)
  expect_match(warnings[1], "2 onomatopoeia have no id or no word")
  expect_match(warnings[2], "1 onomatopoeia have a language that could not be read")
  expect_identical(names(onomatopoeia), names(getHeaders("onomatopoeia")))
  expect_equal(nrow(onomatopoeia), 10)
  expect_identical(unique(onomatopoeia$source), "bio.acousti.ca")
  #Words are HTML at bio.acousti.ca, and the whitespace within one is a space
  expect_identical(onomatopoeia$word[9], "wuff wuff")
  expect_identical(onomatopoeia$remarks[9], "Sound of wings")
  expect_identical(onomatopoeia$info_url[9], NA_character_)
  #A word is kept as its source writes it, in the script it is written in
  expect_identical(onomatopoeia$word[6], "m\u00e9\u00e9-m\u00e9\u00e9")
})

test_that("a kind names the class OLiA gives it, and one it has no class for names none", {
  onomatopoeia <- suppressWarnings(normaliseOnomatopoeia(readOnomatopoeia()))

  #An imitation and a verb lexicalised from one are both onomatopoetic words;
  #what differs is their part of speech, and the source's own word keeps it
  expect_identical(onomatopoeia$kind[1:2], c("imitation", "onomatopoeia verb"))
  expect_identical(onomatopoeia$kind_link[1:2], rep(onomatopoeticWord, 2))
  #Nothing names a mnemonic or a line of musical notation, so they take no IRI
  expect_identical(onomatopoeia$kind[7:8], c("musical notation", "mnemonic"))
  expect_identical(onomatopoeia$kind_link[7:8], rep(NA_character_, 2))
})

test_that("a kind is matched however it is spaced, hyphenated or capitalised", {
  expect_identical(onomatopoeiaType(c("imitation", "Onomatopoeia Verb", "onomatopoeia-verb",
                                      "IDEOPHONE")),
                   c(onomatopoeticWord, onomatopoeticWord, onomatopoeticWord,
                     "http://purl.org/olia/olia.owl#Ideophone"))
  expect_identical(onomatopoeiaType(c("mnemonic", "musical notation", "", NA)),
                   rep(NA_character_, 4))
})

test_that("a language is a BCP 47 tag, and one a source doesn't give is uploaded as NULL", {
  onomatopoeia <- suppressWarnings(normaliseOnomatopoeia(readOnomatopoeia()))

  expect_identical(onomatopoeia$language[1], "en-GB")
  expect_identical(onomatopoeia$language[6], "el")
  #A rendering the site records no language for keeps none, and musical
  #notation is in no language at all
  expect_identical(onomatopoeia$language[5], NA_character_)
  expect_identical(onomatopoeia$locality[5], "Lokele tribe of the Congo")
  expect_identical(onomatopoeia$language[7], NA_character_)
  #One whose language cannot be read is left without rather than guessed at
  expect_identical(onomatopoeia$language[10], NA_character_)
})

test_that("the sex and life stage a rendering belongs to are kept", {
  onomatopoeia <- suppressWarnings(normaliseOnomatopoeia(readOnomatopoeia()))

  expect_identical(onomatopoeia$sex[3], "male")
  expect_identical(onomatopoeia$lifeStage[3], NA_character_)
  expect_identical(onomatopoeia$lifeStage[4], "juvenile")
  expect_identical(onomatopoeia$sex[4], NA_character_)
})

test_that("normalising onomatopoeia again changes nothing", {
  onomatopoeia <- suppressWarnings(normaliseOnomatopoeia(readOnomatopoeia()))

  expect_identical(normaliseOnomatopoeia(onomatopoeia), onomatopoeia)
})

test_that("renderings with no id, or no word, are skipped", {
  table <- readOnomatopoeia()

  expect_warning(onomatopoeia <- normaliseOnomatopoeia(table[table$id %in% c("39252", ""), ]),
                 "2 onomatopoeia have no id or no word")
  expect_equal(nrow(onomatopoeia), 0)
})

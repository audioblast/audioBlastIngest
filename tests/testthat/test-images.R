#Images as ingestR() reads them from a source
readImages <- function(source="bio.acousti.ca") {
  data <- sourceR(source, read.csv(test_path("fixtures", "images.csv"),
                                   colClasses="character", encoding="UTF-8"))
  colnames(data) <- names(getHeaders("images"))
  return(data)
}

test_that("images are normalised", {
  warnings <- character(0)
  images <- withCallingHandlers(normaliseImages(readImages()), warning=function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  expect_length(warnings, 2)
  expect_match(warnings[1], "Skipping 2 images with no id or no file to show")
  expect_match(warnings[2], "1 images have a post_date that could not be read")
  expect_identical(names(images), names(getHeaders("images")))
  expect_equal(nrow(images), 5)
  expect_identical(unique(images$source), "bio.acousti.ca")
  expect_identical(images$id, c("134", "135", "15254", "20603", "30667"))
})

test_that("an image is published with the licence it is under", {
  images <- suppressWarnings(normaliseImages(readImages()))

  expect_identical(images$license[1], "https://creativecommons.org/licenses/by-nc-sa/4.0/")
  expect_identical(images$license[3], "https://creativecommons.org/licenses/by/4.0/")
  #An image whose source gives no licence keeps none rather than being given one
  expect_identical(images$license[4], NA_character_)
})

test_that("what an image is, and what it is of, are kept as the source says them", {
  images <- suppressWarnings(normaliseImages(readImages()))

  expect_identical(images$subtype[1:3],
                   c("Original metadata scan", "Original trace scan", "Photograph"))
  expect_identical(images$subtype[4], NA_character_)
  expect_identical(images$creator[3], "Ashleigh Whiffin")
  expect_identical(images$creator[1], NA_character_)
  expect_identical(images$title[1], "399-3_Conocephalus_discolor_409_meta.jpg")
  #A figure's title is its legend, and its caption is HTML at bio.acousti.ca
  expect_identical(images$title[3], "Morphological comparision of Supersonus spp. habitus.")
  expect_identical(images$caption[3], "(A, B) Male and female of S. aequoreus.")
  expect_identical(images$caption[1], NA_character_)
})

test_that("a file, its type and its size are read, and what can't be read is left out", {
  images <- suppressWarnings(normaliseImages(readImages()))

  expect_identical(images$file[1],
                   "https://bio.acousti.ca/sites/default/files/399-3_Conocephalus_discolor_409_meta.jpg")
  expect_identical(images$type, c("image/jpeg", "image/jpeg", "image/jpeg", "image/png", "image/jpeg"))
  expect_identical(images$size_raw[1], "910718")
  expect_identical(images$width[1:2], c("1412", "1352"))
  #A size of no bytes and a width of fewer than no pixels are not measurements
  expect_identical(images$size_raw[4:5], rep(NA_character_, 2))
  expect_identical(images$width[5], NA_character_)
  expect_identical(images$height[5], "600")
  expect_identical(images$post_date[1], "2019-09-11")
  expect_identical(images$post_date[5], NA_character_)
})

test_that("images with no id, or no file to show, are skipped", {
  table <- readImages()

  expect_warning(images <- normaliseImages(table[table$id %in% c("40001", ""), ]),
                 "Skipping 2 images with no id or no file to show")
  expect_equal(nrow(images), 0)
})

test_that("normalising images again changes nothing", {
  images <- suppressWarnings(normaliseImages(readImages()))

  expect_identical(suppressWarnings(normaliseImages(images)), images)
})

test_that("uploading images replaces the ones a source gave before", {
  upload <- suppressWarnings(mockUpload(uploadImages, readImages()))

  expect_identical(upload$calls, c("begin", "execute", "execute", "commit"))
  expect_match(upload$executed[[1]]$sql, "^DELETE FROM `images` WHERE `source` = \\?$")
  expect_identical(upload$executed[[1]]$params, list("bio.acousti.ca"))
  expect_match(upload$executed[[2]]$sql, "^INSERT INTO `images`")

  rows <- boundRows(upload$executed[[2]])
  expect_length(rows, 5)
  expect_identical(vapply(rows, function(row) row[[2]], character(1)),
                   c("134", "135", "15254", "20603", "30667"))
})

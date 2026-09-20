#Traits with the given values, and every other column empty
traitsTable <- function(...) {
  values <- data.frame(..., stringsAsFactors=FALSE, check.names=FALSE)
  columns <- names(getHeaders("traits"))
  table <- as.data.frame(lapply(setNames(nm=columns), function(column) rep_len("", nrow(values))),
                         stringsAsFactors=FALSE, check.names=FALSE)
  table[names(values)] <- values
  return(table)
}

test_that("a value holding several values becomes a row for each, with its own id", {
  table <- traitsTable(
    traitID=c("2170", "2136", "5987", "39548"),
    Trait=c("Time Of Day Of Call", "Peak Frequency (kHz)", "Secondary resonator", "Echeme Duration"),
    Value=c("Evening; Night", "20; 50", "neck; h1; pre-mirror ", "0.4"))

  out <- seperatoR(table)

  expect_equal(nrow(out), 8)
  #The first value keeps the trait's id, so the ids that links give still work
  expect_identical(out$traitID,
                   c("2170", "2170.2", "2136", "2136.2", "5987", "5987.2", "5987.3", "39548"))
  expect_identical(out$Value,
                   c("Evening", "Night", "20", "50", "neck", "h1", "pre-mirror", "0.4"))
  expect_false(anyDuplicated(out$traitID) > 0)
  #The rest of the trait is repeated for each of its values
  expect_identical(out$Trait[1:2], rep("Time Of Day Of Call", 2))
})

test_that("a value that isn't several values is left alone", {
  table <- traitsTable(traitID=c("1", "2", "3", "4", "5"),
                       Value=c("Present", "", "a;", ";", "a;;b"))

  out <- seperatoR(table)

  expect_identical(out$traitID, c("1", "2", "3", "4", "5", "5.2"))
  expect_identical(out$Value, c("Present", "", "a", "", "a", "b"))
})

test_that("values written as a spread or a range give their ends", {
  table <- traitsTable(
    traitID=as.character(1:8),
    Value=c("4 \u00b1 0.5", "4\u00b10.5", "4-6", "0-6", "-5", "Sunrise-0900", "1-2-3", "Present"))

  out <- seperatoR(table)

  expect_identical(out$min, c(3.5, 3.5, 4, 0, NA, NA, NA, NA))
  expect_identical(out$max, c(4.5, 4.5, 6, 6, NA, NA, NA, NA))
  #The value itself is kept as the source wrote it
  expect_identical(out$Value[1], "4 \u00b1 0.5")
})

test_that("each of several values keeps its own range", {
  table <- traitsTable(traitID="1", Value="4-6; 10-12")

  out <- seperatoR(table)

  expect_identical(out$min, c(4, 10))
  expect_identical(out$max, c(6, 12))
})

test_that("a table with no traits is returned with the range columns", {
  out <- seperatoR(traitsTable()[0, , drop=FALSE])

  expect_equal(nrow(out), 0)
  expect_true(all(is.element(c("traitID", "Value", "min", "max"), names(out))))
})

test_that("uploadTraits uploads a value of each of a trait's values", {
  table <- seperatoR(traitsTable(traitID=c("2170", "39548"), Value=c("Evening; Night", "0.4")))

  upload <- mockUpload(uploadTraits, table)

  columns <- names(getHeaders("traits"))
  expect_identical(upload$executed[[1]]$sql, insertSQL("traits", columns, columns[-(1:2)], 3))
  rows <- boundRows(upload$executed[[1]])
  expect_identical(vapply(rows, `[[`, character(1), 2), c("2170", "2170.2", "39548"))
  #min and max have no column in the traits table yet, so they aren't uploaded
  expect_length(rows[[1]], length(columns))
})

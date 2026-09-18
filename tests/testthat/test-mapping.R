test_that("sources' columns are mapped to the standard columns", {
  source <- list(type="recordings", mapping=list(id="nid", Title="title"))
  data <- data.frame(nid=c("58428", "58427"), title=c("Song", "Pond"), taxon=c("Gryllus campestris", ""))

  mapped <- colmap(source, data)

  expect_identical(names(mapped), names(getHeaders("recordings")))
  expect_identical(mapped$id, c("58428", "58427"))
  expect_identical(mapped$Title, c("Song", "Pond"))
  expect_identical(mapped$taxon, c("Gryllus campestris", ""))
  expect_identical(mapped$license, c("", ""))
})

test_that("overrides can include the values of other columns", {
  source <- list(type="recordings", mapping=list(id="nid"),
                 override=list(info_url="https://bio.acousti.ca/node/{id}", deployment="", Duration=300))
  data <- data.frame(nid=c("58428", "58427"))

  mapped <- colmap(source, data)

  expect_identical(mapped$info_url, c("https://bio.acousti.ca/node/58428", "https://bio.acousti.ca/node/58427"))
  expect_identical(mapped$deployment, c("", ""))
  expect_equal(mapped$Duration, c(300, 300))
})

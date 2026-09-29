test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("ingestR replaces the recordings of a source read from a file, and only adds to a harvest's", {
  file <- withr::local_tempfile(fileext=".csv")
  write.csv(data.frame(id=c("1", "2"), Title=c("a", "b")), file, row.names=FALSE)
  harvested <- data.frame(id="3", Title="c")
  local_mocked_bindings(
    #A source read from a file, one that is harvested, and one that is both
    getSources=function() list(
      list(name="file", type="recordings", url=file, process=list("sourceR")),
      list(name="harvest", type="recordings", inaturalist=list(taxon_id=1), process=list("sourceR")),
      list(name="both", type="recordings", url=file, process=list("sourceR")),
      list(name="both", type="recordings", inaturalist=list(taxon_id=2), process=list("sourceR"))),
    inaturalistR=function(taxon_id, ...) list(recordings=harvested),
    uploadTraits=function(db, table) NULL,
    uploadRecordings=function(db, table, replace=FALSE) {
      uploaded[[length(uploaded) + 1]] <<- list(sources=unique(table$source), replace=replace)
    })
  uploaded <- list()

  ingestR(db="db")

  #A harvest can stop short without failing, so a source that is harvested,
  #even in part, only ever has recordings added
  expect_identical(uploaded, list(list(sources="file", replace=TRUE),
                                  list(sources=c("harvest", "both"), replace=FALSE)))
})

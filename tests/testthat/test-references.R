referencesFixture <- function() {
  return(test_path("fixtures", "references.bib"))
}

test_that("BibTeX is read into the references format", {
  data <- bibtexR(referencesFixture())

  expect_identical(names(data), names(getHeaders("references")))
  expect_true(all(vapply(data, is.character, logical(1))))
  #Comment, preamble and string entries are not references, and the repeat of
  #102 is merged into its first entry
  expect_identical(data$id, c("101", "102", "103", "Smith:2020"))
  expect_identical(data$source, rep("", 4))
  expect_identical(data$type, c("article", "mastersthesis", "book", "article"))

  frog <- data[1, ]
  expect_identical(frog$title, "Calls of the t\u00fangara frog (Engystomops pustulosus) in \u201curban\u201d ponds")
  expect_identical(frog$author, paste(
    "N\u00fa\u00f1ez, Jos\u00e9; de la Cruz, Ana\u00efs; Carvalho, William Douglas;",
    "World Wildlife Fund; L\u00f3pez-Baucells, Adri\u00e0;",
    "Warcha\u0142owska-\u015aliwa, El\u017cbieta"))
  expect_identical(frog$journal, "Bioacoustics & Ecology")
  expect_identical(frog$year, "2021")
  expect_identical(frog$month, "Jan-09-2021")
  expect_identical(frog$volume, "12")
  expect_identical(frog$pages, "1-10")
  expect_identical(frog$abstract, paste0(
    "A male's call peaks at 5 kHz \u00b1 0.2 kHz, for ~2 s.\n",
    "It is 50% longer in town.\n\n",
    "Compare Ragge & Reynolds (p < 0.05)."))
  expect_identical(frog$keywords, "frogs, urban noise")
  expect_identical(frog$issn, "0000-0000, 1111-1111")
  expect_identical(frog$doi, "10.1234/ABC.101")
  expect_identical(frog$url, "http://example.org/view?id=101&lang=en&q=a%20b")
  expect_identical(frog$editor, "")

  thesis <- data[2, ]
  #The school is only in the first of the repeated entries
  expect_identical(thesis$school, "University of Missouri\u2013Columbia")
  expect_identical(thesis$type_of_work, "phd")
  expect_identical(thesis$author, "Frederick, Katherine H")

  book <- data[3, ]
  expect_identical(book$title, "Insect DNA and the \u00d6resund bridge")
  expect_identical(book$author, "Kearton, Richard")
  expect_identical(book$editor, "von Frisch, Karl; Zola, \u00c9mile")
  expect_identical(book$publisher, "Cassell and Company, Limited")
  expect_identical(book$address, "London")
  expect_identical(book$isbn, "978-0-00-000000-2")

  smith <- data[4, ]
  expect_identical(smith$author, "Smith, Jr, John; O'Neil, Mary; Nowak, \u0141ukasz")
  expect_identical(smith$title, "Calls at 20 \u03bcm, \"quoted\" and 5 m\u00b2 of H\u2082O")
  expect_identical(smith$year, "2020")
  #Macros are not expanded
  expect_identical(smith$month, "jan")
  expect_identical(smith$journal, "jasa")
  expect_identical(smith$note, "Contains {braces}, 50 Hz at 25\u00b0C, US $153 and italics emphasis")
})

test_that("BibTeX is read with Windows line endings and a byte order mark", {
  path <- tempfile(fileext=".bib")
  bibtex <- "@article{1,\r\n\ttitle = {Line\r\n\tbreaks},\r\n\tabstract = {<p>One</p>\r\n<p>Two</p>\r\n}\r\n}\r\n"
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(bibtex)), path)

  data <- bibtexR(path)
  unlink(path)

  expect_identical(data$id, "1")
  expect_identical(data$title, "Line breaks")
  expect_identical(data$abstract, "One\n\nTwo")
})

test_that("BibTeX that cannot be read is skipped with a warning", {
  path <- tempfile(fileext=".bib")
  writeLines(c(
    "@article{title = {No key}}",
    "@article(2, title = {Parentheses})",
    "@article{3, title = {Kept}, broken}"), path)

  warnings <- character(0)
  data <- withCallingHandlers(bibtexR(path), warning=function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  expect_identical(data$id, "3")
  expect_identical(data$title, "Kept")
  expect_length(warnings, 3)
  expect_match(warnings[1], "not @type{key, ...} at: @article(2", fixed=TRUE)
  expect_match(warnings[2], "without a key: title = {No key}", fixed=TRUE)
  expect_match(warnings[3], "without a value in entry 3", fixed=TRUE)

  writeLines("@article{4, title = {Unbalanced}", path)
  expect_error(bibtexR(path), "unbalanced")
  unlink(path)
})

test_that("an empty BibTeX file has no references", {
  path <- tempfile(fileext=".bib")
  writeLines("", path)
  data <- bibtexR(path)
  unlink(path)

  expect_identical(names(data), names(getHeaders("references")))
  expect_equal(nrow(data), 0)
})

test_that("names are listed surname first", {
  expect_identical(
    bibtexNames(c(
      "Charles Robert Darwin and Karl von Frisch",
      "von Frisch, Karl AND Darwin, Charles",
      "Jean de~La~Fontaine and Ludwig {van} Beethoven",
      "{\\'E}mile Zola and {\\L}ukasz Nowak and {\\'e}douard de la Poussin",
      "Brand and {Barnes and Noble} and Aristotle",
      "Smith, Jr., John and , Madonna",
      "")),
    c(
      "Darwin, Charles Robert; von Frisch, Karl",
      "von Frisch, Karl; Darwin, Charles",
      "de La Fontaine, Jean; Beethoven, Ludwig van",
      "Zola, \u00c9mile; Nowak, \u0141ukasz; \u00e9douard de la Poussin",
      "Brand; Barnes and Noble; Aristotle",
      "Smith, Jr., John; Madonna",
      ""))
})

test_that("LaTeX and HTML are converted to text", {
  expect_identical(
    bibtexText(c(
      "{\\c c}a{\\u g}{\\v s}{\\H o}{\\k a}{\\r a}{\\=a}{\\^o}\\'{\\i}{\\\"\\i}\\`e",
      "{\\ss}{\\o}{\\ae}{\\OE}{\\l}{\\textendash}{\\textemdash}{\\textquoteright}",
      "50\\% of \\#1 \\& \\$2 \\_x \\~{} \\{\\textbackslash\\}",
      "$\\alpha$ $\\#$ $^{-1}$ $_{12}$ $x+y$ \\unknown US $5",
      "a~b ~2 {Protected} \\emph{emphasis} {\\bf bold}",
      "A&amp;B &#233;&#xE9; &unknown; &lt;i&gt; &#0; <b>bold</b>",
      "<p>One</p><p>Two<br/>Three</p>",
      "e\u0301")),
    c(
      "\u00e7a\u011f\u0161\u0151\u0105\u00e5\u0101\u00f4\u00ed\u00ef\u00e8",
      "\u00df\u00f8\u00e6\u0152\u0142\u2013\u2014\u2019",
      "50% of #1 & $2 _x ~ {\\}",
      "\u03b1 # \u207b\u00b9 \u2081\u2082 $x+y$ \\unknown US $5",
      "a b ~2 Protected emphasis bold",
      "A&B \u00e9\u00e9 &unknown; <i> &#0; bold",
      "One\n\nTwo\nThree",
      "\u00e9"))
  expect_identical(
    bibtexLink(c(" http://example.org/a\\_b\\%20c?d=1\\&e=$\\#$f ", "10.1000/a\\~{}b")),
    c("http://example.org/a_b%20c?d=1&e=#f", "10.1000/a~b"))
})

test_that("uploadReferences uploads every column, with NULL for empty values", {
  table <- sourceR("bio.acousti.ca", bibtexR(referencesFixture()))

  upload <- mockUpload(uploadReferences, table)

  #All four references are inserted by one statement
  expect_length(upload$executed, 1)
  columns <- names(getHeaders("references"))
  sql <- upload$executed[[1]]$sql
  expect_match(sql, "^INSERT INTO `references` \\(`source`, `id`, `type`, `title`, ")
  #Every column but the key is updated for references already there
  expect_match(sql, "ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `title` = VALUES(`title`), ", fixed=TRUE)
  placeholders <- lengths(regmatches(sql, gregexpr("?", sql, fixed=TRUE)))
  expect_length(upload$executed[[1]]$params, placeholders)
  rows <- boundRows(upload$executed[[1]])
  expect_length(rows, 4)
  expect_identical(rows[[1]][1:3], list("bio.acousti.ca", "101", "article"))
  expect_identical(rows[[1]][[which(columns == "doi")]], "10.1234/ABC.101")
  #Missing fields are uploaded as NULL
  expect_identical(rows[[1]][[which(columns == "editor")]], NA_character_)
})

test_that("ingestR uploads references from BibTeX sources", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="bio.acousti.ca", type="references", url=referencesFixture(), process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadReferences=function(db, table) uploaded <<- table)

  ingestR(db="db")

  expect_identical(names(uploaded), names(getHeaders("references")))
  expect_identical(uploaded$source, rep("bio.acousti.ca", 4))
  expect_identical(uploaded$id, c("101", "102", "103", "Smith:2020"))
})

test_that("ingestR carries on when a references source cannot be read", {
  uploaded <- NULL
  local_mocked_bindings(
    getSources=function() list(
      list(name="missing", type="references", url=file.path(tempdir(), "missing.bib"), process="sourceR"),
      list(name="bio.acousti.ca", type="references", url=referencesFixture(), process="sourceR")),
    uploadTraits=function(db, table) NULL,
    uploadReferences=function(db, table) uploaded <<- table)

  expect_warning(ingestR(db="db"), "Skipping source missing")

  expect_identical(unique(uploaded$source), "bio.acousti.ca")
})

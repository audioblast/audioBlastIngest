#' Read references from BibTeX
#'
#' Reads a BibTeX file, such as the bibliography exported from BioAcoustica,
#' into the audioBlast! references format. The key of each entry is its id.
#'
#' Field values are converted to plain text from LaTeX, and from the HTML that
#' some exports (e.g. the Drupal Biblio module) use in titles and abstracts.
#' Authors and editors are listed surname first and separated by semicolons,
#' e.g. `Darwin, Charles; von Frisch, Karl`. DOIs are given without a resolver,
#' e.g. `10.1093/database/bav054`. The type column is the type of entry, e.g.
#' `article`, so the BibTeX type field (e.g. the type of a thesis) is given as
#' `type_of_work`.
#'
#' Exports can repeat an entry, sometimes without some of its fields, so
#' repeats are merged, keeping the first value given for each field. Fields
#' without a references column are ignored, as are comment, preamble and string
#' entries (string macros are not expanded). Entries that cannot be read are
#' skipped with a warning.
#'
#' @param file Path or URL of a BibTeX file, encoded as UTF-8.
#' @return Data frame of references, with an empty source column (see
#'   sourceR()).
#' @examples
#' \dontrun{
#' references <- sourceR("bio.acousti.ca", bibtexR("references.bib"))
#' uploadReferences(db, references)
#' }
#' @importFrom stringi stri_trans_nfc
#' @export
bibtexR <- function(file) {
  text <- paste(readLines(file, encoding="UTF-8", warn=FALSE), collapse="\n")
  bibtex <- bibtexEntries(sub("^\ufeff", "", text))
  #As in BibTeX, the first of a repeated field is used
  fields <- bibtex$fields[!duplicated(bibtex$fields[, c("entry", "name")]), ]

  columns <- names(getHeaders("references"))
  data <- as.data.frame(
    matrix("", nrow=nrow(bibtex$entries), ncol=length(columns), dimnames=list(NULL, columns)),
    stringsAsFactors=FALSE)
  data$id <- bibtex$entries$id
  data$type <- bibtex$entries$type
  for (column in columns[-(1:3)]) {
    rows <- fields$name == if (column == "type_of_work") "type" else column
    data[fields$entry[rows], column] <- fields$value[rows]
  }

  #Merge repeated entries, keeping the first value given for each field
  if (anyDuplicated(data$id)) {
    ids <- data$id
    merged <- data[!duplicated(ids), ]
    for (column in columns) {
      given <- data[[column]] != ""
      value <- data[[column]][given][match(merged$id, ids[given])]
      merged[[column]] <- ifelse(is.na(value), "", value)
    }
    data <- merged
  }

  for (column in columns[-(1:3)]) {
    if (column %in% c("author", "editor")) {
      data[[column]] <- bibtexNames(data[[column]])
    } else if (column %in% c("doi", "url", "attachments")) {
      data[[column]] <- bibtexLink(data[[column]])
    } else {
      data[[column]] <- bibtexText(data[[column]])
    }
  }
  data$doi <- sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", data$doi, ignore.case=TRUE, perl=TRUE)

  rownames(data) <- NULL
  return(data)
}

#Splits BibTeX into entries, with their type and id (key), and the fields of
#the entries, with their name and value
bibtexEntries <- function(text) {
  chars <- strsplit(text, "")[[1]]
  span <- function(from, to) {
    return(vapply(seq_along(from), function(i) {
      if (to[i] < from[i]) "" else paste(chars[from[i]:to[i]], collapse="")
    }, character(1)))
  }
  snippet <- function(from) gsub("\\s+", " ", span(from, pmin(from + 29, length(chars))))

  #Brace depth after each character
  depth <- cumsum(chars == "{") - cumsum(chars == "}")
  if (length(chars) > 0 && (any(depth < 0) || depth[length(depth)] != 0)) {
    stop("BibTeX braces are unbalanced")
  }

  #Entries are @type{...}
  at <- which(chars == "@" & depth == 0)
  opens <- which(chars == "{" & depth == 1)
  open <- opens[findInterval(at, opens) + 1]
  type <- tolower(trimws(span(at + 1, ifelse(is.na(open), at, open - 1))))
  for (i in which(!grepl("^[a-z]+$", type))) {
    warning(paste("Skipping BibTeX entry that is not @type{key, ...} at:", snippet(at[i])))
  }
  entry <- grepl("^[a-z]+$", type) & !type %in% c("comment", "preamble", "string")
  type <- type[entry]
  open <- open[entry]
  closes <- which(chars == "}" & depth == 0)
  close <- closes[findInterval(open, closes) + 1]

  #Commas, equals and hashes in an entry separate its key, fields, and the
  #names and parts of values, unless they are in the braces or quotes of a value
  marks <- which(chars %in% c(",", "=", "#") & depth == 1)
  markEntry <- findInterval(marks, open)
  quotes <- cumsum(chars == "\"" & depth == 1)
  outside <- marks < c(0, close)[markEntry + 1] &
    (quotes[marks] - c(0, quotes[open])[markEntry + 1]) %% 2 == 0
  marks <- marks[outside]
  markEntry <- markEntry[outside]

  #An entry is its key, then fields, separated by commas
  commas <- chars[marks] == ","
  start <- c(open, marks[commas]) + 1
  segmentEntry <- c(seq_along(open), markEntry[commas])
  o <- order(start)
  start <- start[o]
  segmentEntry <- segmentEntry[o]
  nextEntry <- c(segmentEntry, NA)[-1]
  end <- ifelse(!is.na(nextEntry) & nextEntry == segmentEntry, c(start, NA)[-1] - 2, close[segmentEntry] - 1)
  key <- !duplicated(segmentEntry)

  id <- trimws(span(start[key], end[key]))
  hasKey <- grepl("^[^[:space:]\"#%'(),={}]+$", id)
  for (i in which(!hasKey)) {
    warning(paste("Skipping BibTeX entry without a key:", snippet(start[key][i])))
  }

  #A field is name = value
  equals <- marks[chars[marks] == "="]
  field <- findInterval(equals, start)
  first <- !duplicated(field) & !key[field]
  equals <- equals[first]
  field <- field[first]
  filled <- c(0, cumsum(!chars %in% c(" ", "\t", "\n", "\r", "\f")))
  for (i in which(filled[end + 1] > filled[start] & !key & hasKey[segmentEntry])) {
    if (!i %in% field) {
      warning(paste("Skipping BibTeX field without a value in entry", id[segmentEntry[i]]))
    }
  }

  #A value is braced, quoted or bare parts, joined by hashes
  hashes <- marks[chars[marks] == "#"]
  hashField <- findInterval(hashes, start)
  equalsOf <- rep(NA_integer_, length(start))
  equalsOf[field] <- equals
  joins <- !is.na(equalsOf[hashField]) & hashes > equalsOf[hashField]
  bound <- c(equals, hashes[joins])
  boundField <- c(field, hashField[joins])
  o <- order(bound)
  bound <- bound[o]
  boundField <- boundField[o]
  nextField <- c(boundField, NA)[-1]
  parts <- trimws(span(bound + 1, ifelse(!is.na(nextField) & nextField == boundField, c(bound, NA)[-1] - 1, end[boundField])))
  enclosed <- (startsWith(parts, "{") & endsWith(parts, "}")) |
    (nchar(parts) > 1 & startsWith(parts, "\"") & endsWith(parts, "\""))
  parts[enclosed] <- substr(parts[enclosed], 2, nchar(parts[enclosed]) - 1)
  values <- vapply(split(parts, factor(boundField, levels=field)), paste, character(1),
                   collapse="", USE.NAMES=FALSE)

  fields <- data.frame(
    entry=cumsum(hasKey)[segmentEntry[field]],
    name=tolower(trimws(span(start[field], equals - 1))),
    value=trimws(gsub("\\s+", " ", values)),
    stringsAsFactors=FALSE)
  return(list(
    entries=data.frame(type=type[hasKey], id=id[hasKey], stringsAsFactors=FALSE),
    fields=fields[hasKey[segmentEntry[field]], ]))
}

#Converts LaTeX, and HTML, to plain text with composed accented letters
bibtexText <- function(x) {
  return(stri_trans_nfc(html2text(latex2text(x))))
}

#Unescapes DOIs and URLs, which are otherwise left as they are
bibtexLink <- function(x) {
  x <- gsub("$\\#$", "#", x, fixed=TRUE)
  x <- gsub("\\\\([~^])\\{\\}", "\\1", x, perl=TRUE)
  x <- gsub("\\\\([&%#$_{}~])", "\\1", x, perl=TRUE)
  return(trimws(x))
}

#Lists names surname first, separated by semicolons
bibtexNames <- function(x) {
  names <- lapply(x, function(value) {
    names <- trimws(splitOutsideBraces(value, "(?i)\\s+and\\s+"))
    return(names[names != ""])
  })
  name <- unlist(names, use.names=FALSE)
  if (length(name) == 0) return(rep_len("", length(x)))

  #Ties join the words of names, and commas separate von Last, Jr and First
  parts <- lapply(gsub("(?<!\\\\)~", " ", name, perl=TRUE), function(n) {
    return(trimws(splitOutsideBraces(n, ",")))
  })
  words <- lapply(parts, function(p) {
    words <- splitOutsideBraces(p[1], "\\s+")
    return(words[words != ""])
  })
  lower <- split(bibtexLowercase(unlist(words, use.names=FALSE)),
                 factor(rep(seq_along(words), lengths(words)), levels=seq_along(words)))

  #Surname, Jr and First for each name
  raw <- vapply(seq_along(name), function(i) {
    w <- words[[i]]
    p <- parts[[i]]
    if (length(p) > 1) {
      first <- paste(p[-seq_len(min(length(p) - 1, 2))], collapse=", ")
      return(c(paste(w, collapse=" "), if (length(p) > 2) p[2] else "", first))
    }
    #In First von Last, the surname starts at the first lower case word (the
    #von part, e.g. van) or else is the last word
    n <- length(w)
    if (n == 0) return(c("", "", ""))
    von <- which(lower[[i]][-n])
    start <- if (length(von) > 0) min(von) else n
    return(c(paste(w[start:n], collapse=" "), "", paste(w[seq_len(start - 1)], collapse=" ")))
  }, character(3))
  text <- matrix(bibtexText(raw), nrow=3)
  formatted <- apply(text, 2, function(t) paste(t[t != ""], collapse=", "))

  joined <- split(formatted, factor(rep(seq_along(x), lengths(names)), levels=seq_along(x)))
  return(vapply(joined, function(f) paste(f[f != ""], collapse="; "), character(1), USE.NAMES=FALSE))
}

#Whether words start in lower case, as the von parts of names do. Words in
#braces, e.g. {Van}, have no case, but accented letters, e.g. {\'e}, do.
bibtexLowercase <- function(words) {
  caseless <- grepl("^\\{(?!\\\\)", words, perl=TRUE)
  return(!caseless & grepl("^[^\\p{L}]*\\p{Ll}", latex2text(words), perl=TRUE))
}

#Splits a string where pattern matches outside of braces
splitOutsideBraces <- function(x, pattern) {
  match <- gregexpr(pattern, x, perl=TRUE)[[1]]
  if (match[1] == -1) return(x)
  starts <- as.integer(match)
  ends <- starts + attr(match, "match.length") - 1
  if (grepl("{", x, fixed=TRUE)) {
    chars <- strsplit(x, "")[[1]]
    outside <- (cumsum(chars == "{") - cumsum(chars == "}"))[starts] == 0
    starts <- starts[outside]
    ends <- ends[outside]
  }
  return(substring(x, c(1, ends + 1), c(starts - 1, nchar(x))))
}

#Converts LaTeX, e.g. {\'e}, {\textendash} and \&, to plain text
latex2text <- function(x) {
  #Line breaks and ties (~) are spaces, but a tilde before a number, e.g. ~2
  #months, is kept as some exports (e.g. Biblio) do not escape tildes
  x <- gsub("\\\\\\\\", " ", x)
  x <- gsub("(?<!\\\\)~(?![0-9])", " ", x, perl=TRUE)
  x <- gsub("\\\\([~^])\\{\\}", "\\1", x, perl=TRUE)

  #Accents, e.g. \'e, \'{e}, {\'\i}, \c c and \c{c}
  base <- "(?:\\\\([ij])(?![A-Za-z])|([A-Za-z]))"
  for (command in names(latexAccents)) {
    if (grepl("^[A-Za-z]$", command)) {
      pattern <- paste0("\\\\", command, "(?:\\s*\\{\\s*", base, "\\s*\\}|\\s+", base, ")")
    } else {
      pattern <- paste0("\\\\\\", command, "\\s*(?:\\{\\s*", base, "\\s*\\}|", base, ")")
    }
    x <- gsub(pattern, paste0("\\1\\2\\3\\4", latexAccents[[command]]), x, perl=TRUE)
  }

  #A single command, superscript or subscript in math mode, e.g. $\#$ or
  #$^{2}$. Other dollar signs are left as they are not always escaped.
  x <- latexReplace(x, "\\$(\\\\[A-Za-z]+|\\\\[#$%&_{}]|[\\^_]\\{[^{}$]*\\}|[\\^_]\\\\[A-Za-z]+|[\\^_][^{}$\\\\\\s])\\$", latexMath)

  #Text commands, e.g. {\ss} and {\textendash}
  x <- latexReplace(x, paste0("\\\\(", paste(names(latexSymbols), collapse="|"), ")(?![A-Za-z])(?:\\{\\}|\\s*)"), function(command) {
    return(unname(latexSymbols[sub("^\\\\([A-Za-z]+).*$", "\\1", command)]))
  })

  #Font declarations, e.g. {\it text}, and commands, e.g. \emph{text}, leaving
  #the text. Other commands without an argument are kept.
  x <- gsub("\\\\(em|it|bf|sc|rm|sf|tt|sl|up|md|itshape|bfseries|scshape|upshape|slshape|mdseries|normalfont)(?![A-Za-z])\\s*", "", x, perl=TRUE)
  x <- gsub("\\\\[A-Za-z]+\\s*(?=\\{)", "", x, perl=TRUE)

  #Escaped characters and spacing
  x <- gsub("\\{", "\ue000", x, fixed=TRUE)
  x <- gsub("\\}", "\ue001", x, fixed=TRUE)
  x <- gsub("\\\\([&%#$_])", "\\1", x, perl=TRUE)
  x <- gsub("\\\\[ ,;:]", " ", x, perl=TRUE)
  x <- gsub("\\\\[-/!]", "", x, perl=TRUE)

  x <- gsub("[{}]", "", x, perl=TRUE)
  x <- gsub("\ue000", "{", x, fixed=TRUE)
  x <- gsub("\ue001", "}", x, fixed=TRUE)
  x <- gsub("\ue002", "\\", x, fixed=TRUE)
  return(x)
}

#Replaces each match of pattern with the result of replacement for it
latexReplace <- function(x, pattern, replacement) {
  matches <- gregexpr(pattern, x, perl=TRUE)
  regmatches(x, matches) <- lapply(regmatches(x, matches), function(m) {
    return(vapply(m, replacement, character(1), USE.NAMES=FALSE))
  })
  return(x)
}

#The text for math, e.g. $\mu$, $^{2}$ or $_{2}$, or the math if it is not known
latexMath <- function(math) {
  math <- substr(math, 2, nchar(math) - 1)
  script <- substr(math, 1, 1)
  if (script %in% c("^", "_")) {
    text <- sub("^\\{(.*)\\}$", "\\1", substring(math, 2))
    if (script == "^" && text == "\\circ") return("\u00b0")
    chars <- strsplit(text, "")[[1]]
    scripts <- if (script == "^") latexSuperscripts else latexSubscripts
    if (length(chars) > 0 && all(chars %in% names(scripts))) {
      return(paste(scripts[chars], collapse=""))
    }
    return(if (grepl("\\", text, fixed=TRUE)) paste0("$", math, "$") else text)
  }
  symbol <- latexMathSymbols[substring(math, 2)]
  return(if (is.na(symbol)) paste0("$", math, "$") else unname(symbol))
}

#Converts HTML, e.g. <p>, <i> and &ndash;, to plain text
html2text <- function(x) {
  x <- gsub("<!--.*?-->", "", x, perl=TRUE)
  x <- gsub("<br\\s*/?>", "\n", x, ignore.case=TRUE, perl=TRUE)
  x <- gsub("</?(p|div)(\\s[^<>]*)?>", "\n\n", x, ignore.case=TRUE, perl=TRUE)
  x <- gsub("</?[A-Za-z][A-Za-z0-9:-]*(\\s[^<>]*)?/?>", "", x, perl=TRUE)

  x <- latexReplace(x, "&(#[0-9]{1,7}|#[xX][0-9A-Fa-f]{1,6}|[A-Za-z][A-Za-z0-9]{1,31});", function(reference) {
    name <- substr(reference, 2, nchar(reference) - 1)
    if (grepl("^#[xX]", name)) {
      code <- strtoi(substring(name, 3), 16L)
    } else if (startsWith(name, "#")) {
      code <- as.integer(substring(name, 2))
    } else {
      code <- unname(htmlEntities[name])
    }
    if (is.na(code) || code == 0 || code > 1114111 || (code >= 55296 && code <= 57343)) {
      return(reference)
    }
    return(intToUtf8(code))
  })

  #Spaces, including non-breaking and thin spaces, become plain spaces
  x <- gsub("\\h+", " ", x, perl=TRUE)
  x <- gsub(" ?\n ?", "\n", x, perl=TRUE)
  x <- gsub("\n{3,}", "\n\n", x, perl=TRUE)
  return(trimws(x))
}

#LaTeX accent commands, and the combining marks for them
latexAccents <- c(
  "'"="\u0301", "`"="\u0300", "^"="\u0302", "\""="\u0308", "~"="\u0303",
  "="="\u0304", "."="\u0307", u="\u0306", v="\u030c", H="\u030b", c="\u0327",
  k="\u0328", r="\u030a", d="\u0323", b="\u0331"
)

#LaTeX text commands, and the characters for them. Braces and backslashes are
#placeholders until other braces have been removed.
latexSymbols <- c(
  i="\u0131", j="\u0237", l="\u0142", L="\u0141", o="\u00f8", O="\u00d8",
  ae="\u00e6", AE="\u00c6", oe="\u0153", OE="\u0152", aa="\u00e5", AA="\u00c5",
  ss="\u00df", SS="SS", dh="\u00f0", DH="\u00d0", th="\u00fe", TH="\u00de",
  dj="\u0111", DJ="\u0110", ng="\u014b", NG="\u014a", S="\u00a7", P="\u00b6",
  pounds="\u00a3", copyright="\u00a9", dag="\u2020", ddag="\u2021",
  ldots="\u2026", dots="\u2026", TeX="TeX", LaTeX="LaTeX", BibTeX="BibTeX",
  textexclamdown="\u00a1", textcent="\u00a2", textsterling="\u00a3",
  textyen="\u00a5", textbrokenbar="\u00a6", textsection="\u00a7",
  textasciidieresis="\u00a8", textcopyright="\u00a9", textordfeminine="\u00aa",
  guillemotleft="\u00ab", textlnot="\u00ac", textregistered="\u00ae",
  textasciimacron="\u00af", textdegree="\u00b0", textpm="\u00b1",
  texttwosuperior="\u00b2", textthreesuperior="\u00b3", textasciiacute="\u00b4",
  textmu="\u00b5", textparagraph="\u00b6", textperiodcentered="\u00b7",
  textonesuperior="\u00b9", textordmasculine="\u00ba", guillemotright="\u00bb",
  textonequarter="\u00bc", textonehalf="\u00bd", textthreequarters="\u00be",
  textquestiondown="\u00bf", texttimes="\u00d7", textdiv="\u00f7",
  textflorin="\u0192", textasciicircum="^", textacutedbl="\u02dd",
  textasciitilde="~", textendash="\u2013", textemdash="\u2014",
  textbardbl="\u2016", textunderscore="_", textquoteleft="\u2018",
  textquoteright="\u2019", quotesinglbase="\u201a", textquotedblleft="\u201c",
  textquotedblright="\u201d", quotedblbase="\u201e", textquotedbl="\"",
  textquotesingle="'", textdagger="\u2020", textdaggerdbl="\u2021",
  textbullet="\u2022", textellipsis="\u2026", textperthousand="\u2030",
  guilsinglleft="\u2039", guilsinglright="\u203a",
  textfractionsolidus="\u2044", texteuro="\u20ac", euro="\u20ac",
  textcelsius="\u2103", textnumero="\u2116", textcircledP="\u2117",
  textservicemark="\u2120", texttrademark="\u2122", textohm="\u03a9",
  textestimated="\u212e", textleftarrow="\u2190", textuparrow="\u2191",
  textrightarrow="\u2192", textdownarrow="\u2193", textlangle="\u27e8",
  textrangle="\u27e9", textvisiblespace="\u2423", textbar="|", textless="<",
  textgreater=">", textasteriskcentered="*", textbackslash="\ue002",
  textbraceleft="\ue000", textbraceright="\ue001"
)

#LaTeX math mode commands, and the characters for them
latexMathSymbols <- c(
  "#"="#", "$"="$", "%"="%", "&"="&", "_"="_", "{"="\ue000", "}"="\ue001",
  alpha="\u03b1", beta="\u03b2", gamma="\u03b3", delta="\u03b4",
  epsilon="\u03b5", varepsilon="\u03b5", zeta="\u03b6", eta="\u03b7",
  theta="\u03b8", vartheta="\u03d1", iota="\u03b9", kappa="\u03ba",
  lambda="\u03bb", mu="\u03bc", nu="\u03bd", xi="\u03be", pi="\u03c0",
  varpi="\u03d6", rho="\u03c1", varrho="\u03f1", sigma="\u03c3",
  varsigma="\u03c2", tau="\u03c4", upsilon="\u03c5", phi="\u03c6",
  varphi="\u03c6", chi="\u03c7", psi="\u03c8", omega="\u03c9", Gamma="\u0393",
  Delta="\u0394", Theta="\u0398", Lambda="\u039b", Xi="\u039e", Pi="\u03a0",
  Sigma="\u03a3", Upsilon="\u03a5", Phi="\u03a6", Psi="\u03a8", Omega="\u03a9",
  infty="\u221e", pm="\u00b1", mp="\u2213", times="\u00d7", div="\u00f7",
  cdot="\u00b7", sim="~", approx="\u2248", simeq="\u2243", propto="\u221d",
  equiv="\u2261", leq="\u2264", le="\u2264", geq="\u2265", ge="\u2265",
  neq="\u2260", ne="\u2260", ll="\u226a", gg="\u226b", circ="\u2218",
  degree="\u00b0", prime="\u2032", partial="\u2202", nabla="\u2207",
  sqrt="\u221a", sum="\u2211", prod="\u220f", int="\u222b", ell="\u2113",
  hbar="\u210f", to="\u2192", rightarrow="\u2192", leftarrow="\u2190",
  uparrow="\u2191", downarrow="\u2193", leftrightarrow="\u2194",
  Rightarrow="\u21d2", Leftarrow="\u21d0", ldots="\u2026", cdots="\u22ef",
  backslash="\ue002", lbrace="\ue000", rbrace="\ue001"
)

latexSuperscripts <- c(
  "0"="\u2070", "1"="\u00b9", "2"="\u00b2", "3"="\u00b3", "4"="\u2074",
  "5"="\u2075", "6"="\u2076", "7"="\u2077", "8"="\u2078", "9"="\u2079",
  "+"="\u207a", "-"="\u207b", "="="\u207c", "("="\u207d", ")"="\u207e",
  n="\u207f", i="\u2071"
)

latexSubscripts <- c(
  "0"="\u2080", "1"="\u2081", "2"="\u2082", "3"="\u2083", "4"="\u2084",
  "5"="\u2085", "6"="\u2086", "7"="\u2087", "8"="\u2088", "9"="\u2089",
  "+"="\u208a", "-"="\u208b", "="="\u208c", "("="\u208d", ")"="\u208e"
)

#HTML 4 named character references (and &apos;), and their code points
htmlEntities <- c(
  quot=34L, amp=38L, apos=39L, lt=60L, gt=62L, nbsp=160L, iexcl=161L, cent=162L,
  pound=163L, curren=164L, yen=165L, brvbar=166L, sect=167L, uml=168L,
  copy=169L, ordf=170L, laquo=171L, not=172L, shy=173L, reg=174L, macr=175L,
  deg=176L, plusmn=177L, sup2=178L, sup3=179L, acute=180L, micro=181L,
  para=182L, middot=183L, cedil=184L, sup1=185L, ordm=186L, raquo=187L,
  frac14=188L, frac12=189L, frac34=190L, iquest=191L, Agrave=192L, Aacute=193L,
  Acirc=194L, Atilde=195L, Auml=196L, Aring=197L, AElig=198L, Ccedil=199L,
  Egrave=200L, Eacute=201L, Ecirc=202L, Euml=203L, Igrave=204L, Iacute=205L,
  Icirc=206L, Iuml=207L, ETH=208L, Ntilde=209L, Ograve=210L, Oacute=211L,
  Ocirc=212L, Otilde=213L, Ouml=214L, times=215L, Oslash=216L, Ugrave=217L,
  Uacute=218L, Ucirc=219L, Uuml=220L, Yacute=221L, THORN=222L, szlig=223L,
  agrave=224L, aacute=225L, acirc=226L, atilde=227L, auml=228L, aring=229L,
  aelig=230L, ccedil=231L, egrave=232L, eacute=233L, ecirc=234L, euml=235L,
  igrave=236L, iacute=237L, icirc=238L, iuml=239L, eth=240L, ntilde=241L,
  ograve=242L, oacute=243L, ocirc=244L, otilde=245L, ouml=246L, divide=247L,
  oslash=248L, ugrave=249L, uacute=250L, ucirc=251L, uuml=252L, yacute=253L,
  thorn=254L, yuml=255L, OElig=338L, oelig=339L, Scaron=352L, scaron=353L,
  Yuml=376L, fnof=402L, circ=710L, tilde=732L, Alpha=913L, Beta=914L,
  Gamma=915L, Delta=916L, Epsilon=917L, Zeta=918L, Eta=919L, Theta=920L,
  Iota=921L, Kappa=922L, Lambda=923L, Mu=924L, Nu=925L, Xi=926L, Omicron=927L,
  Pi=928L, Rho=929L, Sigma=931L, Tau=932L, Upsilon=933L, Phi=934L, Chi=935L,
  Psi=936L, Omega=937L, alpha=945L, beta=946L, gamma=947L, delta=948L,
  epsilon=949L, zeta=950L, eta=951L, theta=952L, iota=953L, kappa=954L,
  lambda=955L, mu=956L, nu=957L, xi=958L, omicron=959L, pi=960L, rho=961L,
  sigmaf=962L, sigma=963L, tau=964L, upsilon=965L, phi=966L, chi=967L, psi=968L,
  omega=969L, thetasym=977L, upsih=978L, piv=982L, ensp=8194L, emsp=8195L,
  thinsp=8201L, zwnj=8204L, zwj=8205L, lrm=8206L, rlm=8207L, ndash=8211L,
  mdash=8212L, lsquo=8216L, rsquo=8217L, sbquo=8218L, ldquo=8220L, rdquo=8221L,
  bdquo=8222L, dagger=8224L, Dagger=8225L, bull=8226L, hellip=8230L,
  permil=8240L, prime=8242L, Prime=8243L, lsaquo=8249L, rsaquo=8250L,
  oline=8254L, frasl=8260L, euro=8364L, image=8465L, weierp=8472L, real=8476L,
  trade=8482L, alefsym=8501L, larr=8592L, uarr=8593L, rarr=8594L, darr=8595L,
  harr=8596L, crarr=8629L, lArr=8656L, uArr=8657L, rArr=8658L, dArr=8659L,
  hArr=8660L, forall=8704L, part=8706L, exist=8707L, empty=8709L, nabla=8711L,
  isin=8712L, notin=8713L, ni=8715L, prod=8719L, sum=8721L, minus=8722L,
  lowast=8727L, radic=8730L, prop=8733L, infin=8734L, ang=8736L, and=8743L,
  or=8744L, cap=8745L, cup=8746L, int=8747L, there4=8756L, sim=8764L,
  cong=8773L, asymp=8776L, ne=8800L, equiv=8801L, le=8804L, ge=8805L, sub=8834L,
  sup=8835L, nsub=8836L, sube=8838L, supe=8839L, oplus=8853L, otimes=8855L,
  perp=8869L, sdot=8901L, lceil=8968L, rceil=8969L, lfloor=8970L, rfloor=8971L,
  lang=9001L, rang=9002L, loz=9674L, spades=9824L, clubs=9827L, hearts=9829L,
  diams=9830L
)

#Turns a trait value that holds several values, separated by semicolons, into a
#row for each of them: a Time Of Day Of Call of "Evening; Night" is a value of
#Evening and a value of Night. The first of them keeps the trait's id and the
#rest take it with the number of the value added, e.g. 2170 and 2170.2, so that
#one value is not lost to another that shares its id, and the ids that sources
#and links already give stay as they are.
#
#A value written as a number and a spread (4 \u00b1 0.5), or as a range (4-6),
#also gets the ends of that range as min and max. Values that aren't written
#that way leave them empty, as do ends that are the wrong way round: those are
#either typed backwards (0.06-0.01) or a time of day that passes midnight
#(1630-0300), and a range of two numbers says neither.
seperatoR <- function(input) {
  values <- strsplit(as.character(input$Value), ";", fixed=TRUE)
  values <- lapply(values, function(value) {
    if (length(value) < 2) {
      return(if (length(value) == 0) "" else value)
    }
    #A value that is only semicolons, or has one of them at an end, is one value
    value <- trimws(value)
    return(if (any(value != "")) value[value != ""] else "")
  })

  output <- input[rep(seq_len(nrow(input)), lengths(values)), , drop=FALSE]
  output$Value <- as.character(unlist(values, use.names=FALSE))
  part <- sequence(lengths(values))
  output$traitID <- ifelse(part == 1, as.character(output$traitID),
                           paste0(output$traitID, ".", part))
  rownames(output) <- NULL

  spread <- rangeEnds(output$Value, "\u00b1")
  range <- rangeEnds(output$Value, "-")
  output$min <- ifelse(is.na(spread[1, ]), range[1, ], spread[1, ] - spread[2, ])
  output$max <- ifelse(is.na(spread[1, ]), range[2, ], spread[1, ] + spread[2, ])
  backwards <- !is.na(output$min) & output$min > output$max
  output$min[backwards] <- NA
  output$max[backwards] <- NA
  return(output)
}

#The two numbers that values are written as, separated by separator, as the
#rows of a matrix; NA for values that aren't two numbers
rangeEnds <- function(x, separator) {
  return(vapply(strsplit(trimws(as.character(x)), separator, fixed=TRUE), function(ends) {
    ends <- suppressWarnings(as.numeric(ends))
    if (length(ends) != 2 || anyNA(ends)) {
      return(c(NA_real_, NA_real_))
    }
    return(ends)
  }, numeric(2)))
}

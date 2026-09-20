#Normalises a data frame of specimens (see uploadSpecimens()), so that each
#column holds one form of value whichever source a specimen came from. The
#columns are named after the Darwin Core terms they hold. Values are trimmed,
#and values that can't be read are set to NA, which is uploaded as NULL:
#
#* eventDate, when the specimen was collected or observed, and dateIdentified
#  are ISO 8601 dates: YYYY-MM-DD, or YYYY-MM or YYYY when only the month or
#  year is known (see isoDate()).
#* individualCount is a whole number of individuals greater than 0.
#* decimalLatitude and decimalLongitude are decimal degrees in range, and
#  countryCode is an ISO 3166-1 alpha-2 code.
#* occurrenceRemarks is plain text, as sources often hold it as HTML.
#* info_url is an http(s) URL.
#
#The other columns are as the source gives them, so that a source keeps its own
#terms for what it holds (e.g. Male or male for sex).
#
#Normalising specimens that are already normalised leaves them unchanged.
normaliseSpecimens <- function(table) {
  columns <- names(getHeaders("specimens"))
  table <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE, check.names=FALSE)

  for (column in c("eventDate", "dateIdentified")) {
    dates <- isoDate(table[[column]])
    warnUnread("specimens", column, table[[column]], dates)
    table[[column]] <- dates
  }

  table$individualCount <- wholeNumber(table$individualCount)
  table$decimalLatitude <- coordinate(table$decimalLatitude, 90)
  table$decimalLongitude <- coordinate(table$decimalLongitude, 180)
  table$countryCode <- countryCode(table$countryCode)
  table$occurrenceRemarks <- html2text(table$occurrenceRemarks)
  table$info_url <- httpURL(table$info_url)
  for (column in columns[-(1:2)]) {
    table[[column]][!is.na(table[[column]]) & table[[column]] == ""] <- NA
  }
  return(table)
}

#Normalises a data frame of locations (see uploadLocations()), so that each
#column holds one form of value whichever source a place came from. The columns
#are named after the Darwin Core terms they hold. Values are trimmed, and values
#that can't be read are set to NA, which is uploaded as NULL:
#
#* decimalLatitude and decimalLongitude are decimal degrees in range, and
#  countryCode is an ISO 3166-1 alpha-2 code.
#* coordinateUncertaintyInMeters is a distance greater than 0, and the
#  elevations are numbers of metres, which are below 0 below sea level.
#* info_url is an http(s) URL.
#
#The other columns are as the source gives them, so that a source keeps its own
#names for the places and the units it puts them in.
#
#Normalising locations that are already normalised leaves them unchanged.
normaliseLocations <- function(table) {
  columns <- names(getHeaders("locations"))
  table <- as.data.frame(
    lapply(table[columns], function(x) trimws(ifelse(is.na(x), "", as.character(x)))),
    stringsAsFactors=FALSE, check.names=FALSE)

  table$decimalLatitude <- coordinate(table$decimalLatitude, 90)
  table$decimalLongitude <- coordinate(table$decimalLongitude, 180)
  table$countryCode <- countryCode(table$countryCode)
  table$coordinateUncertaintyInMeters <- positiveNumber(table$coordinateUncertaintyInMeters)
  for (column in c("minimumElevationInMeters", "maximumElevationInMeters")) {
    table[[column]] <- decimalNumber(table[[column]])
  }
  table$info_url <- httpURL(table$info_url)
  for (column in columns[-(1:2)]) {
    table[[column]][!is.na(table[[column]]) & table[[column]] == ""] <- NA
  }
  return(table)
}

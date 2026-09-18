#Normalises a data frame of recordings (see uploadRecordings()), so that each
#column holds one form of value whichever source a recording came from.
#Recordings from sources set up before columns were added to the end of the
#recordings table are given the missing columns. Values are trimmed, and values
#that can't be read are set to NA, which is uploaded as NULL:
#
#* Date is an ISO 8601 date: YYYY-MM-DD, or YYYY-MM or YYYY when only the month
#  or year is known (see isoDate()). post_date, the date a recording was
#  uploaded, is given in the same way.
#* Time is a 24-hour clock time, HH:MM or HH:MM:SS (see clockTime()). A time
#  that isn't a clock time, such as "morning" or "1300-1400", is moved to
#  time_of_day, which describes the time of day in words.
#* Duration is a number of seconds greater than 0, size_raw a number of bytes,
#  and lat and lon are decimal degrees in range.
#* type is a lower case MIME type, with the other names of the WAV and MP3
#  types replaced by audio/x-wav and audio/mpeg, as most sources give them.
#* license and info_url are http(s) URLs.
#
#Normalising recordings that are already normalised leaves them unchanged.
normaliseRecordings <- function(table) {
  columns <- names(getHeaders("recordings"))
  for (column in setdiff(columns, names(table))) {
    table[[column]] <- rep_len("", nrow(table))
  }
  table <- as.data.frame(lapply(table[columns], function(x) trimws(as.character(x))),
                         stringsAsFactors=FALSE, check.names=FALSE)

  for (column in c("Date", "post_date")) {
    dates <- isoDate(table[[column]])
    warnUnread(column, table[[column]], dates)
    table[[column]] <- dates
  }

  times <- clockTime(table$Time)
  described <- is.na(times) & !unknownTime(table$Time)
  table$time_of_day[described] <- joinText(table$time_of_day[described], table$Time[described])
  table$Time <- times

  table$Duration <- positiveNumber(table$Duration)
  table$size_raw <- ifelse(grepl("^[0-9]+$", table$size_raw), table$size_raw, NA_character_)
  table$lat <- coordinate(table$lat, 90)
  table$lon <- coordinate(table$lon, 180)
  table$type <- mimeType(table$type)
  table$license <- httpURL(table$license)
  table$info_url <- httpURL(table$info_url)
  for (column in c("time_of_day", "device")) {
    table[[column]][!is.na(table[[column]]) & table[[column]] == ""] <- NA
  }
  return(table)
}

#ISO 8601 dates (YYYY-MM-DD, or YYYY-MM or YYYY when only the month or year is
#known) of dates written in one of those ways, of dates followed by a time
#(e.g. 2021-05-24 20:48) and of dates written like "Sunday, January 19, 2003"
#or "January 19, 2003"; NA for anything else
isoDate <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  out <- rep(NA_character_, length(x))

  ymd <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}([ T]|$)", x)
  out[ymd] <- calendarDate(substr(x[ymd], 1, 4), substr(x[ymd], 6, 7), substr(x[ymd], 9, 10))

  ym <- grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", x) & !startsWith(x, "0000")
  out[ym] <- x[ym]
  y <- grepl("^[0-9]{4}$", x) & x != "0000"
  out[y] <- x[y]

  #Month D, YYYY, perhaps after the day of the week
  parts <- regmatches(x, regexec("^(?:[A-Za-z]+, *)?([A-Za-z]+) +([0-9]{1,2}), *([0-9]{4})$", x, perl=TRUE))
  mdy <- lengths(parts) == 4
  if (any(mdy)) {
    parts <- matrix(unlist(parts[mdy]), nrow=4)
    month <- match(tolower(parts[2, ]), tolower(month.name))
    abbreviated <- is.na(month)
    month[abbreviated] <- match(tolower(parts[2, abbreviated]), tolower(month.abb))
    out[mdy] <- calendarDate(parts[4, ], month, parts[3, ])
  }
  return(out)
}

#YYYY-MM-DD of years (as four digits), months and days, or NA where there is
#no such day
calendarDate <- function(year, month, day) {
  date <- sprintf("%s-%02d-%02d", year, as.integer(month), as.integer(day))
  valid <- year != "0000" & !is.na(as.Date(date, format="%Y-%m-%d", optional=TRUE))
  return(ifelse(valid, date, NA_character_))
}

#24-hour clock times (HH:MM, or HH:MM:SS where seconds are given) of times
#written as H:MM, HH:MM, H.MM, HH.MM, HH:MM:SS or HHMM, or as a fraction of a
#day, which is how spreadsheets store times (0.75 is 18:00); NA for anything
#else
clockTime <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  out <- rep(NA_character_, length(x))

  parts <- regmatches(x, regexec("^([0-9]{1,2})[:.]([0-9]{2})(:([0-9]{2}))?$", x))
  clock <- lengths(parts) == 5
  if (any(clock)) {
    parts <- matrix(unlist(parts[clock]), nrow=5)
    out[clock] <- formatClock(as.numeric(parts[2, ]), as.numeric(parts[3, ]),
                              suppressWarnings(as.numeric(parts[5, ])))
  }

  hhmm <- is.na(out) & grepl("^[0-9]{4}$", x)
  out[hhmm] <- formatClock(as.numeric(substr(x[hhmm], 1, 2)), as.numeric(substr(x[hhmm], 3, 4)))

  #A fraction of a day is rounded to the minute. 0 could be midnight, but is
  #more often a time that was left empty, so it isn't read.
  fraction <- is.na(out) & grepl("^0?\\.[0-9]+$", x)
  minutes <- round(as.numeric(x[fraction]) * 1440)
  out[fraction] <- ifelse(minutes > 0 & minutes < 1440,
                          formatClock(minutes %/% 60, minutes %% 60), NA_character_)
  return(out)
}

#HH:MM of hours and minutes, with :SS added where seconds are given, or NA
#where they aren't a time of day
formatClock <- function(hours, minutes, seconds=rep(NA_real_, length(hours))) {
  valid <- !is.na(hours) & !is.na(minutes) & hours < 24 & minutes < 60 &
    (is.na(seconds) | seconds < 60)
  out <- rep(NA_character_, length(hours))
  out[valid] <- sprintf("%02d:%02d", as.integer(hours[valid]), as.integer(minutes[valid]))
  withSeconds <- valid & !is.na(seconds)
  out[withSeconds] <- paste0(out[withSeconds], sprintf(":%02d", as.integer(seconds[withSeconds])))
  return(out)
}

#Whether times say nothing about when a recording was made: missing, empty, a
#placeholder such as "?", or 0 (see clockTime())
unknownTime <- function(x) {
  x <- tolower(trimws(as.character(x)))
  return(is.na(x) | x %in% c("?", "-", "na", "n/a", "unknown") | grepl("^0*\\.?0*$", x))
}

#Text added to other text, which may be missing or empty, unless it is the same
joinText <- function(text, addition) {
  return(ifelse(is.na(text) | text == "", addition,
                ifelse(text == addition, text, paste(text, addition, sep="; "))))
}

#Numbers greater than 0, rounded to three decimal places and written without
#trailing zeros, or NA
positiveNumber <- function(x) {
  value <- round(suppressWarnings(as.numeric(x)), 3)
  out <- rep(NA_character_, length(x))
  positive <- !is.na(value) & is.finite(value) & value > 0
  out[positive] <- sub("\\.?0+$", "", sprintf("%.3f", value[positive]))
  return(out)
}

#Decimal degrees no further than limit from 0 (90 for latitudes, 180 for
#longitudes), or NA
coordinate <- function(x, limit) {
  value <- suppressWarnings(as.numeric(x))
  return(ifelse(!is.na(value) & abs(value) <= limit, x, NA_character_))
}

#Lower case MIME types, with the other names of the WAV and MP3 types replaced
#by audio/x-wav and audio/mpeg; NA for none
mimeType <- function(x) {
  aliases <- c("audio/wav"="audio/x-wav", "audio/wave"="audio/x-wav",
               "audio/x-wave"="audio/x-wav", "audio/vnd.wave"="audio/x-wav",
               "audio/mp3"="audio/mpeg", "audio/x-mp3"="audio/mpeg",
               "audio/mpeg3"="audio/mpeg", "audio/x-mpeg-3"="audio/mpeg")
  x <- tolower(trimws(as.character(x)))
  alias <- x %in% names(aliases)
  x[alias] <- unname(aliases[x[alias]])
  x[!is.na(x) & x == ""] <- NA
  return(x)
}

#http(s) URLs, with ones that start // (as xeno-canto gives them) made https;
#NA for anything else
httpURL <- function(x) {
  x <- sub("^//", "https://", trimws(as.character(x)))
  return(ifelse(grepl("^https?://[^[:space:]/?#]+[^[:space:]]*$", x), x, NA_character_))
}

#Warns of values of a column that couldn't be read, so are left out
#' @importFrom utils head
warnUnread <- function(column, values, read) {
  unread <- !is.na(values) & values != "" & is.na(read)
  if (any(unread)) {
    examples <- paste0("\"", head(unique(values[unread]), 3), "\"", collapse=", ")
    warning(sum(unread), " recordings have a ", column, " that could not be read, so it is left out, e.g. ",
            examples, call.=FALSE)
  }
}

#Recordings with the given values, and every other column empty
recordingsTable <- function(...) {
  values <- data.frame(..., stringsAsFactors=FALSE, check.names=FALSE)
  columns <- names(getHeaders("recordings"))
  table <- as.data.frame(lapply(setNames(nm=columns), function(column) rep_len("", nrow(values))),
                         stringsAsFactors=FALSE, check.names=FALSE)
  table[names(values)] <- values
  return(table)
}

test_that("recording dates are read as ISO 8601 dates", {
  expect_identical(
    isoDate(c("2021-12-23", "2021-05-24 20:48", "2021-10-11T12:19:42.058Z", "1998-06", "2004",
              "Sunday, January 19, 2003", "Thursday, June 23, 2011", "January 9, 2003", "Jan 9, 2003")),
    c("2021-12-23", "2021-05-24", "2021-10-11", "1998-06", "2004",
      "2003-01-19", "2011-06-23", "2003-01-09", "2003-01-09"))
  expect_identical(
    isoDate(c("2021-02-30", "2021-13", "0000", "0000-01-01", "19/01/2003", "Someday, Smarch 19, 2003", "", NA)),
    rep(NA_character_, 8))
})

test_that("recording times are read as 24-hour clock times", {
  expect_identical(
    clockTime(c("09:30", "9:30", "7.05", "0:41", "23:59:30", "1640", "0833", "0000")),
    c("09:30", "09:30", "07:05", "00:41", "23:59:30", "16:40", "08:33", "00:00"))
  #Spreadsheets store times as fractions of a day
  expect_identical(
    clockTime(c("0.75", "0.875", "0.916666666666667", "0.8590277777777778")),
    c("18:00", "21:00", "22:00", "20:37"))
  expect_identical(
    clockTime(c("24:00", "2400", "12:60", "0", "morning", "1300-1400", "", NA)),
    rep(NA_character_, 8))
})

test_that("recordings are normalised", {
  table <- recordingsTable(
    source=c("bio.acousti.ca", "bio.acousti.ca", "unp"),
    id=c("58428", "58354", "nhm-unp-1-1588606809"),
    Title=c(" Jassopsaltria rufifacies male calling song ", "Promops centralis social calls",
            "nhm-unp-1 2020-05-04 1640"),
    file=c("https://bio.acousti.ca/sites/default/files/song.mp3",
           "https://bio.acousti.ca/sites/default/files/m5300132_00.zc",
           "aao/nhm-unp-1/1588606809.wav"),
    post_date=c("2021-05-24 20:48", "2020-05-28 16:01", "2020-05-04"),
    size_raw=c("1069487", "1088", NA),
    type=c("audio/mpeg", "application/octet-stream", "audio/x-wave"),
    NonSpecimen=c("", "", "Soundscape"),
    Date=c("Sunday, January 19, 2003", "Wednesday, May 30, 2012", "2020-05-04"),
    Time=c("", "0.8590277777777778", "1640"),
    Duration=c("29", "0", "60"),
    deployment=c("", "", "nhm-unp-1"))

  normalised <- normaliseRecordings(table)

  expect_identical(names(normalised), names(getHeaders("recordings")))
  expect_identical(normalised$Title[1], "Jassopsaltria rufifacies male calling song")
  #Files that aren't at a URL are kept as they are
  expect_identical(normalised$file, table$file)
  expect_identical(normalised$post_date, c("2021-05-24", "2020-05-28", "2020-05-04"))
  expect_identical(normalised$size_raw, c("1069487", "1088", NA))
  expect_identical(normalised$type, c("audio/mpeg", "application/octet-stream", "audio/x-wav"))
  expect_identical(normalised$NonSpecimen, c("", "", "Soundscape"))
  expect_identical(normalised$Date, c("2003-01-19", "2012-05-30", "2020-05-04"))
  expect_identical(normalised$Time, c(NA, "20:37", "16:40"))
  expect_identical(normalised$time_of_day, rep(NA_character_, 3))
  expect_identical(normalised$Duration, c("29", NA, "60"))
  expect_identical(normalised$deployment, c("", "", "nhm-unp-1"))
  expect_identical(normalised$lat, rep(NA_character_, 3))
  expect_identical(normalised$license, rep(NA_character_, 3))
})

test_that("times that aren't clock times are kept as the time of day", {
  table <- recordingsTable(
    id=as.character(1:6),
    Time=c("morning", "1300 - 1400", "?", "0", "", "Evening"),
    time_of_day=c("", "", "", "", "dawn", "dusk"))

  normalised <- normaliseRecordings(table)

  expect_identical(normalised$Time, rep(NA_character_, 6))
  #"?" and 0 say nothing about the time of day
  expect_identical(normalised$time_of_day, c("morning", "1300 - 1400", NA, NA, "dawn", "dusk; Evening"))
})

test_that("normalising recordings again changes nothing", {
  table <- recordingsTable(
    id=c("1", "2", "3"),
    Date=c("Sunday, January 19, 2003", "1998-06", ""),
    Time=c("0.75", "morning", "9:30"),
    Duration=c("29.50", "0", ""),
    type=c("audio/x-wave", "", "AUDIO/MPEG"),
    license=c("//creativecommons.org/licenses/by/4.0/", "", "not a licence"),
    lat=c("51.5", "", "91"),
    lon=c("-0.1", "", "0"))

  normalised <- normaliseRecordings(table)

  expect_identical(normaliseRecordings(normalised), normalised)
  expect_identical(normalised$Date, c("2003-01-19", "1998-06", NA))
  expect_identical(normalised$Time, c("18:00", NA, "09:30"))
  expect_identical(normalised$time_of_day, c(NA, "morning", NA))
  expect_identical(normalised$Duration, c("29.5", NA, NA))
  expect_identical(normalised$type, c("audio/x-wav", NA, "audio/mpeg"))
  expect_identical(normalised$license, c("https://creativecommons.org/licenses/by/4.0/", NA, NA))
  expect_identical(normalised$lat, c("51.5", NA, NA))
  expect_identical(normalised$lon, c("-0.1", NA, "0"))
})

test_that("the sound, rights and place of a recording are normalised", {
  table <- recordingsTable(
    id=as.character(1:4),
    rights_holder=c("Natural History Museum, London", "", " Klaus-Gerhard Heller ", ""),
    country=c("GB", "gy", "Guyana", ""),
    locality=c("Mill Site", "", " Kabocalli ", ""),
    sample_rate=c("44100", "44100.0", "0", ""),
    channels=c("stereo", "Joint Stereo", "2", "5.1"))

  expect_warning(
    normalised <- normaliseRecordings(table),
    '1 recordings have a country that could not be read, so it is left out, e.g. "Guyana"',
    fixed=TRUE)

  expect_identical(normaliseRecordings(normalised), normalised)
  expect_identical(normalised$rights_holder,
                   c("Natural History Museum, London", NA, "Klaus-Gerhard Heller", NA))
  expect_identical(normalised$country, c("GB", "GY", NA, NA))
  expect_identical(normalised$locality, c("Mill Site", NA, "Kabocalli", NA))
  expect_identical(normalised$sample_rate, c("44100", "44100", NA, NA))
  #A recording given as stereo has 2 channels
  expect_identical(normalised$channels, c("2", "2", "2", NA))
})

test_that("recordings from before columns were added are given them", {
  table <- recordingsTable(id="1", Time="morning")[1:17]

  normalised <- normaliseRecordings(table)

  expect_identical(names(normalised), names(getHeaders("recordings")))
  expect_identical(normalised$time_of_day, "morning")
  expect_identical(normalised$license, NA_character_)
})

test_that("dates that can't be read are warned of and left out", {
  table <- recordingsTable(id=c("1", "2", "3"), Date=c("19/01/2003", "19/01/2003", "2003-01-19"))

  expect_warning(
    normalised <- normaliseRecordings(table),
    '2 recordings have a Date that could not be read, so it is left out, e.g. "19/01/2003"',
    fixed=TRUE)
  expect_identical(normalised$Date, c(NA, NA, "2003-01-19"))
})

test_that("durations, MIME types and URLs are normalised", {
  expect_identical(
    positiveNumber(c("29", "29.50", "3723.0001", "1e3", "0.0004", "0", "-5", "", "abc", NA)),
    c("29", "29.5", "3723", "1000", NA, NA, NA, NA, NA, NA))
  expect_identical(
    mimeType(c("audio/x-wave", "Audio/WAV", " audio/mp3 ", "audio/flac", "", NA)),
    c("audio/x-wav", "audio/x-wav", "audio/mpeg", "audio/flac", NA, NA))
  expect_identical(
    httpURL(c("https://xeno-canto.org/1", "//creativecommons.org/licenses/by/4.0/",
              "http://example.org/a b", "ftp://example.org", "creativecommons.org", "https://", "", NA)),
    c("https://xeno-canto.org/1", "https://creativecommons.org/licenses/by/4.0/", rep(NA, 6)))
})

#' ConvertStringToDate
#'
#' @param str str
#'
#' @return Date
#'
#' @examples
#' ConvertStringToDate(str = c(
#'   NA_character_, 'UNK', '', '2020', '2020-Q1', '2020-Q02', '2020/Q2', 'Q1-2020', 'Q03-2020',
#'   '2020/q2', 'q1-2020', 'Q04/2020', '2020-W01', '2020-W45', '2020-w40', 'W02-2020', 'w10-2020',
#'   '2020-05-20', '2020/1/10', '20/5/2020', '2020-1', '2020-02', '2020/06', '05-2020', '6/2020'
#' ))
#'
#' @export
ConvertStringToDate <- function(
  str
) {
  dates <- rep(as.Date(NA), length(str))
  str <- gsub('/', '-', str)

  isYear <- grepl('^[0-9]{4}$', str)
  isQuarter1 <- grepl('^[0-9]{4}-(Q|q)[0-9]{1,2}$', str)
  isQuarter2 <- grepl('^(Q|q)[0-9]{1,2}-[0-9]{4}$', str)
  isMonth1 <- grepl('^[0-9]{4}-[0-9]{1,2}$', str)
  isMonth2 <- grepl('^[0-9]{1,2}-[0-9]{4}$', str)
  isWeek1 <- grepl('^[0-9]{4}-(W|w)[0-9]{1,2}$', str)
  isWeek2 <- grepl('^(W|w)[0-9]{1,2}-[0-9]{4}$', str)
  isDate1 <- grepl('^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$', str)
  isDate2 <- grepl('^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$', str)

  # Expand year to date (mid year)
  dates[isYear] <- as.Date(sprintf('%s-07-02', str[isYear]), tz = 'UTC')

  # Expand quarter to date (mid quarter)
  strs <- strsplit(str[isQuarter1], '-')
  years <- as.integer(sapply(strs, '[[', 1))
  months <- as.integer(sub('^(Q|q)', '', sapply(strs, '[[', 2))) * 3 - 1
  dates[isQuarter1] <- as.Date(sprintf('%s-%s-15', years, months), tz = 'UTC')

  strs <- strsplit(str[isQuarter2], '-')
  years <- as.integer(sapply(strs, '[[', 2))
  months <- as.integer(sub('^(Q|q)', '', sapply(strs, '[[', 1))) * 3 - 1
  dates[isQuarter2] <- as.Date(sprintf('%s-%s-15', years, months), tz = 'UTC')

  # Expand month to date (mid month)
  strs <- strsplit(str[isMonth1], '-')
  years <- as.integer(sapply(strs, '[[', 1))
  months <- as.integer(sapply(strs, '[[', 2))
  dates[isMonth1] <- as.Date(sprintf('%s-%s-15', years, months), tz = 'UTC')

  strs <- strsplit(str[isMonth2], '-')
  years <- as.integer(sapply(strs, '[[', 2))
  months <- as.integer(sapply(strs, '[[', 1))
  dates[isMonth2] <- as.Date(sprintf('%s-%s-15', years, months), tz = 'UTC')

  # Expand week to date (mid week)
  strs <- strsplit(str[isWeek1], '-')
  years <- as.integer(sapply(strs, '[[', 1))
  days <- as.integer(sub('^(W|w)', '', sapply(strs, '[[', 2))) * 7 - 4
  dates[isWeek1] <-
    as.Date(as.POSIXct(sprintf('%s-01-01', years), tz = 'UTC') + days * 86400L, tz = 'UTC')

  strs <- strsplit(str[isWeek2], '-')
  years <- as.integer(sapply(strs, '[[', 2))
  days <- as.integer(sub('^(W|w)', '', sapply(strs, '[[', 1))) * 7 - 4
  dates[isWeek2] <-
    as.Date(as.POSIXct(sprintf('%s-01-01', years), tz = 'UTC') + days * 86400L, tz = 'UTC')

  # Process full dates
  dates[isDate1] <- as.Date(str[isDate1], tz = 'UTC', format = '%Y-%m-%d')
  dates[isDate2] <- as.Date(str[isDate2], tz = 'UTC', format = '%d-%m-%Y')

  return(dates)
}

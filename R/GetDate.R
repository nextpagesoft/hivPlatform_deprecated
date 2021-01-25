#' GetDate
#'
#' @param y y
#' @param q q
#' @param m m
#' @param w w
#' @param d d
#'
#' @return Date
#'
#' @examples
#' GetDate(1977, 2, 5, NA, 20)
#'
#' @export
GetDate <- function(
  y = NA,
  q = NA,
  m = NA,
  w = NA,
  d = NA
) {
  tempD <- d
  tempW <- w
  tempM <- m
  tempQ <- q
  tempY <- y
  tempDate <- as.Date(tempW, '%Y-%W')

  # Day and month missing but week observed
  tempD <- ifelse(
    is.na(tempD) & is.na(tempM) & !(is.na(tempW) | tempW == ''),
    as.numeric(strftime(tempDate, '%d')),
    tempD
  )
  tempM <- ifelse(
    is.na(tempM) & !(is.na(tempW) | tempW == ''),
    as.numeric(strftime(tempDate, '%m')),
    tempM
  )

  # Day, week, month missing but quarter observed
  # Q1
  tempD <- ifelse(is.na(tempM) & tempQ == 1, 14, tempD)
  tempM <- ifelse(is.na(tempM) & tempQ == 1, 2, tempM)
  # Q2
  tempD <- ifelse(is.na(tempM) & tempQ == 2, 16, tempD)
  tempM <- ifelse(is.na(tempM) & tempQ == 2, 5, tempM)
  # Q3
  tempD <- ifelse(is.na(tempM) & tempQ == 3, 15, tempD)
  tempM <- ifelse(is.na(tempM) & tempQ == 3, 8, tempM)
  # Q4
  tempD <- ifelse(is.na(tempM) & tempQ == 4, 15, tempD)
  tempM <- ifelse(is.na(tempM) & tempQ == 4, 11, tempM)

  # Day missing, not missing month
  tempD <- ifelse(is.na(tempD) & !is.na(tempM), 15, tempD)

  # Day and month missing
  tempM <- ifelse(is.na(tempD) & is.na(tempM), 7, tempM)
  tempD <- ifelse(is.na(tempD), 1, tempD)

  return(as.Date(ISOdate(tempY, tempM, tempD)))
}

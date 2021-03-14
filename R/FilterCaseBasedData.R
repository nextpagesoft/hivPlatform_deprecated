FilterCaseBasedData <- function(
  data,
  filters
) {
  if (is.null(data)) {
    return(NULL)
  }

  diagYearRange <- c(
    filters$DiagYear$MinYear,
    filters$DiagYear$MaxYear
  )

  notifQuarterRange <- c(
    filters$NotifQuarter$MinYear,
    filters$NotifQuarter$MaxYear
  )

  if (
    any(is.null(diagYearRange)) || any(is.null(notifQuarterRange))
  ) {
    return(data)
  }

  filteredData <- data[
    is.na(YearOfHIVDiagnosis) | is.na(NotificationTime) |
    (YearOfHIVDiagnosis %between% diagYearRange & NotificationTime %between% notifQuarterRange)
  ]

  return(filteredData)
}

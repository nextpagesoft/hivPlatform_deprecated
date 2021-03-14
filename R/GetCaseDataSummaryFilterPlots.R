GetCaseDataSummaryFilterPlots <- function(
  data
) {
  genders <- levels(data$Gender)
  genderLabels <- list(
    M = 'Male',
    F = 'Female',
    O = 'Other'
  )

  # Diagnosis year plot
  diagYearCounts <- data[, .(Count = .N), keyby = .(Gender, YearOfHIVDiagnosis)]
  diagYearCounts <- dcast(diagYearCounts, 'YearOfHIVDiagnosis ~ Gender', value.var = 'Count')
  missingCols <- genders[!genders %in% colnames(diagYearCounts)]
  if (length(missingCols) > 0) {
    diagYearCounts[, (missingCols) := 0]
  }
  diagYearCounts[, (genders) := lapply(.SD, na.zero), .SDcols = genders]
  diagYearCategories <- sort(unique(diagYearCounts$YearOfHIVDiagnosis))
  diagYearPlotData <- list(
    filter = list(
      scaleMinYear = min(diagYearCategories),
      scaleMaxYear = max(diagYearCategories),
      valueMinYear = min(diagYearCategories),
      valueMaxYear = max(diagYearCategories),
      applyInAdjustments = FALSE
    ),
    chartCategories = diagYearCategories,
    chartData = lapply(genders, function(gender) {
      list(
        name = genderLabels[[gender]],
        data = diagYearCounts[[gender]]
      )
    })
  )

  # Notification quarter plot
  notifQuarterCounts <- data[, .(Count = .N), keyby = .(Gender, NotificationTime)]
  notifQuarterCounts <- dcast(notifQuarterCounts, 'NotificationTime ~ Gender', value.var = 'Count')
  missingCols <- genders[!genders %in% colnames(notifQuarterCounts)]
  if (length(missingCols) > 0) {
    notifQuarterCounts[, (missingCols) := 0]
  }
  notifQuarterCounts[, (genders) := lapply(.SD, na.zero), .SDcols = genders]
  notifQuarterCategories <- sort(unique(notifQuarterCounts$NotificationTime))
  notifQuarterPlotData <- list(
    filter = list(
      scaleMinYear = min(notifQuarterCategories),
      scaleMaxYear = max(notifQuarterCategories),
      valueMinYear = min(notifQuarterCategories),
      valueMaxYear = max(notifQuarterCategories),
      applyInAdjustments = FALSE
    ),
    chartCategories = notifQuarterCategories,
    chartData = lapply(genders, function(gender) {
      list(
        name = genderLabels[[gender]],
        data = notifQuarterCounts[[gender]]
      )
    })
  )

  summary <- list(
    DiagYearPlotData = diagYearPlotData,
    NotifQuarterPlotData = notifQuarterPlotData
  )

  return(summary)
}

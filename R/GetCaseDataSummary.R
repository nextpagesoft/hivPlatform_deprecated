GetCaseDataSummary <- function(
  data
) {
  # Diagnosis year plot
  diagYearCounts <- data[, .(Count = .N), keyby = .(Gender, YearOfHIVDiagnosis)]
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
    chartData = list(
      list(
        name = 'Female',
        data = diagYearCounts[Gender == 'F', Count]
      ),
      list(
        name = 'Male',
        data = diagYearCounts[Gender == 'M', Count]
      )
    )
  )

  # Notification quarter plot
  notifQuarterCounts <- data[, .(Count = .N), keyby = .(Gender, NotificationTime)]
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
    chartData = list(
      list(
        name = 'Female',
        data = notifQuarterCounts[Gender == 'F', Count]
      ),
      list(
        name = 'Male',
        data = notifQuarterCounts[Gender == 'M', Count]
      )
    )
  )

  summary <- list(
    DiagYearPlotData = diagYearPlotData,
    NotifQuarterPlotData = notifQuarterPlotData
  )

  return(summary)
}

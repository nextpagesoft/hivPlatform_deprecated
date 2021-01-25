ConvertHIVModelParams <- function(
  x,
  session,
  inputname
) {
  intervals <- ConvertListToDt(x$timeIntervals)
  intervals[, endYear := NULL]
  setnames(intervals, new = Capitalize(colnames(intervals)))

  params <- list(
    ModelMinYear = x$minYear,
    ModelMaxYear = x$maxYear,
    FitPosMinYear = x$minFitPos,
    FitPosMaxYear = x$maxFitPos,
    FitPosCD4MinYear = x$minFitCD4,
    FitPosCD4MaxYear = x$maxFitCD4,
    FitAIDSMinYear = x$minFitAIDS,
    FitAIDSMaxYear = x$maxFitAIDS,
    FitAIDSPosMinYear = x$minFitHIVAIDS,
    FitAIDSPosMaxYear = x$maxFitHIVAIDS,
    FullData = x$fullData,
    ModelNoKnots = x$knotsCount,
    StartIncZero = x$startIncZero,
    MaxIncCorr = x$maxIncCorr,
    FitDistribution = x$distributionFit,
    Delta4Fac = x$delta4Fac,
    Country = x$country,
    Intervals = intervals,
    PopCombination = x$popCombination
  )

  return(params)
}

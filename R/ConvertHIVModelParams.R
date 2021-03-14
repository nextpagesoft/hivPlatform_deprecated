ConvertHIVModelParams <- function(
  x,
  session,
  inputname
) {
  intervals <- ConvertListToDt(x$timeIntervals)
  intervals[, endYear := NULL]
  setnames(intervals, new = Capitalize(colnames(intervals)))

  variableCodeMapping <- list(
    G = 'Gender',
    T = 'Transmission',
    O = 'GroupedRegionOfOrigin',
    R = 'PlaceOfResidence'
  )

  casePopulationsAbbr <- sapply(x$popCombination$casePopulations, '[[', 1)
  casePopulations <- lapply(casePopulationsAbbr, function(casePopulation) {
    casePopulation  <- strsplit(casePopulation, ', ')[[1]]
    l <- lapply(casePopulation, function(el) {
      vals <- strsplit(el, ' ')[[1]]
      value <- vals[1]
      variableCode <- gsub('(\\[|\\])', '', vals[2])
      variable <- variableCodeMapping[variableCode][[1]]
      list(Value = value, Variable = variable)
    })
    list(
      Values = sapply(l, '[[', 'Value'),
      Variables = sapply(l, '[[', 'Variable')
    )
  })
  aggrPopulations <- sapply(x$popCombination$aggrPopulations, '[[', 1)
  popCombination <- list(
    Case = casePopulations,
    CaseAbbr = casePopulationsAbbr,
    Aggr = aggrPopulations
  )

  params <- list(
    Params = list(
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
      Intervals = intervals
    ),
    PopCombination = popCombination
  )

  return(params)
}

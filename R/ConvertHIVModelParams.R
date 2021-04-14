ConvertHIVModelParams <- function(
  x,
  session,
  inputname
) {
  intervals <- ConvertListToDt(x$timeIntervals)
  setnames(intervals, new = Capitalize(colnames(intervals)))
  setcolorder(intervals, c('StartYear', 'EndYear', 'Jump', 'DiffByCD4', 'ChangeInInterval'))

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
      ModelMinYear = as.numeric(x$minYear),
      ModelMaxYear = as.numeric(x$maxYear),
      FitPosMinYear = as.numeric(x$minFitPos),
      FitPosMaxYear = as.numeric(x$maxFitPos),
      FitPosCD4MinYear = as.numeric(x$minFitCD4),
      FitPosCD4MaxYear = as.numeric(x$maxFitCD4),
      FitAIDSMinYear = as.numeric(x$minFitAIDS),
      FitAIDSMaxYear = as.numeric(x$maxFitAIDS),
      FitAIDSPosMinYear = as.numeric(x$minFitHIVAIDS),
      FitAIDSPosMaxYear = as.numeric(x$maxFitHIVAIDS),
      FullData = x$fullData,
      ModelNoKnots = as.integer(x$knotsCount),
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

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

  aggrDataSelection <- x$aggrDataSelection
  aggrDataSelection <- lapply(aggrDataSelection, function(dataEl) {
    dataNames <- strsplit(dataEl$name, ', ')[[1]]
    els <- lapply(dataNames, function(dataName) {
      list(
        Name = dataName,
        Use = dataEl$use,
        MinYear = dataEl$years[[1]],
        MaxYear = dataEl$years[[2]]
      )
    })
  })
  aggrDataSelection <- unlist(aggrDataSelection, recursive = FALSE)
  aggrDataSelection <- data.table(
    Name = sapply(aggrDataSelection, '[[', 'Name'),
    Use = sapply(aggrDataSelection, '[[', 'Use'),
    MinYear = sapply(aggrDataSelection, '[[', 'MinYear'),
    MaxYear = sapply(aggrDataSelection, '[[', 'MaxYear')
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
    PopCombination = popCombination,
    AggrDataSelection = aggrDataSelection
  )

  return(params)
}

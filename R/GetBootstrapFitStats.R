GetBootstrapFitStats <- function(
  fits
) {
  flatList <- Reduce(c, fits)
  resultsList <- lapply(flatList, '[[', 'Results')
  runTime <- sapply(resultsList, '[[', 'RunTime')
  converged <- sapply(resultsList, '[[', 'Converged')
  succFlatList <- Filter(function(item) item$Results$Converged, flatList)
  succResultsList <- lapply(succFlatList, '[[', 'Results')

  info <- lapply(succResultsList, '[[', 'Info')[[1]]
  years <- info$ModelMinYear:(info$ModelMaxYear - 1)

  mainOutputList <- lapply(succResultsList, '[[', 'MainOutputs')
  colNames <- colnames(mainOutputList[[1]])
  mainOutputStats <- setNames(lapply(colNames, function(colName) {
    resultSample <- sapply(mainOutputList, '[[', colName)
    result <- cbind(
      t(apply(resultSample, 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)),
      Mean = apply(resultSample, 1, mean, na.rm = TRUE),
      Std = apply(resultSample, 1, sd, na.rm = TRUE)
    )
    rownames(result) <- years
    return(result)
  }), colNames)

  succParamList <- lapply(succResultsList, '[[', 'Param')
  betas <- as.data.table(t(sapply(succParamList, '[[', 'Beta')))
  setnames(betas, sprintf('Beta%d', seq_len(ncol(betas))))
  bootBetasStats <- lapply(betas, function(col) {
    c(
      quantile(col, probs = c(0.025, 0.5, 0.975), na.rm = TRUE),
      Mean = mean(col),
      Std = sd(col)
    )
  })

  thetasList <- lapply(succParamList, '[[', 'Theta')
  maxThetasLength <- max(sapply(thetasList, length))
  thetasList <- lapply(thetasList, function(thetas) {
    t(c(rep(0, maxThetasLength - length(thetas)), thetas))
  })
  thetas <- rbindlist(lapply(thetasList, as.data.table))
  setnames(thetas, sprintf('Theta%d', seq_len(ncol(thetas))))
  bootThetasStats <- lapply(thetas, function(col) {
    c(
      quantile(col, probs = c(0.025, 0.5, 0.975), na.rm = TRUE),
      Mean = mean(col),
      Std = sd(col)
    )
  })

  stats <- list(
    RunTime = runTime,
    Converged = converged,
    Beta = betas,
    Theta = thetas,
    MainOutputsStats = mainOutputStats,
    BetaStats = bootBetasStats,
    ThetaStats = bootThetasStats
  )

  return(stats)
}
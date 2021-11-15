PredictInf <- function( # nolint
  input,
  params
) {
  outputAIDS <- copy(input$AIDS)
  outputCD4VL <- copy(input$CD4VL)

  if (is.null(outputAIDS)) {
    outputAIDS <- data.table(
      Imputation = integer(),
      RecordId = character(),
      UniqueId = integer(),
      Ord = integer(),
      ProbPre = numeric()
    )
  }

  if (is.null(outputCD4VL)) {
    outputCD4VL <- data.table(
      Imputation = integer(),
      RecordId = character(),
      UniqueId = integer(),
      Ord = integer(),
      ProbPre = numeric()
    )
  }

  # AIDS -------------------------------------------------------------------------------------------
  xAIDS <- cbind(
    1,
    as.integer(outputAIDS$Gender == 'M'),
    outputAIDS$Age
  )

  countAIDS <- outputAIDS[, .N]
  countAIDSNChar <- nchar(as.character(countAIDS))
  if (countAIDS > 0) {
    outputAIDS[, ProbPre := NA_real_]
  }
  startTime <- Sys.time()
  lastTime <- startTime
  PrintH1('Processing AIDS data')
  PrintAlert('Start time: {format(startTime)}')
  for (i in seq_len(countAIDS)) {
    if (i %% 500 == 0) {
      currentTime <- Sys.time()
      percComplete <- stringi::stri_pad_left(
        sprintf('%0.2f%%', i / countAIDS * 100),
        width = 8
      )
      iterComplete <- stringi::stri_pad_left(
        sprintf('%d/%d', i, countAIDS),
        width = countAIDSNChar * 2 + 1
      )
      PrintAlert(
        '{percComplete} ({iterComplete}) - {.timestamp {prettyunits::pretty_dt(currentTime - startTime)}}', # nolint
        type = 'success'
      )
      lastTime <- currentTime
    }

    fit1 <- try(integrate(
      VPostWAIDS,
      lower = outputAIDS$Mig[i], upper = outputAIDS$U[i],
      x = xAIDS[i, ], dTime = outputAIDS$DTime[i],
      betaAIDS = params$betaAIDS, kappa = params$kappa
    ), silent = TRUE)

    fit2 <- try(integrate(
      VPostWAIDS,
      lower = 0, upper = outputAIDS$U[i],
      x = xAIDS[i, ], dTime = outputAIDS$DTime[i],
      betaAIDS = params$betaAIDS, kappa = params$kappa
    ), silent = TRUE)

    if (IsError(fit1) || IsError(fit2) || fit1$message != 'OK' || fit2$message != 'OK') {
      next
    } else {
      outputAIDS[i, ProbPre := fit1$value / fit2$value]
    }
  }
  endTime <- Sys.time()
  if (countAIDS > 0) {
    percComplete <- stringi::stri_pad_left(
      sprintf('%0.2f%%', i / countAIDS * 100),
      width = 8
    )
    iterComplete <- stringi::stri_pad_left(
      sprintf('%d/%d', i, countAIDS),
      width = countAIDSNChar * 2 + 1
    )
    PrintAlert(
      '{percComplete} ({iterComplete}) - {.timestamp {prettyunits::pretty_dt(endTime - startTime)}}', # nolint
      type = 'success'
    )
  } else {
    PrintAlert('No AIDS data to be processed')
  }
  PrintAlert('End time: {format(endTime)}')

  # CD4VL ------------------------------------------------------------------------------------------
  countCD4VL <- outputCD4VL[, uniqueN(UniqueId)]
  countCD4VLNChar <- nchar(as.character(countCD4VL))
  if (countCD4VL > 0) {
    outputCD4VL[, ProbPre := NA_real_]
  }
  i <- 0
  startTime <- Sys.time()
  lastTime <- startTime
  PrintH1('Processing CD4VL data')
  PrintAlert('Start time: {format(startTime)}')
  for (uniqueId in outputCD4VL[, unique(UniqueId)]) {
    i <- i + 1
    if (i %% 500 == 0) {
      currentTime <- Sys.time()
      percComplete <- stringi::stri_pad_left(
        sprintf('%0.2f%%', i / countCD4VL * 100),
        width = 8
      )
      iterComplete <- stringi::stri_pad_left(
        sprintf('%d/%d', i, countCD4VL),
        width = countCD4VLNChar * 2 + 1
      )
      PrintAlert(
        '{percComplete} ({iterComplete}) - {.timestamp {prettyunits::pretty_dt(currentTime - startTime)}}', # nolint
        type = 'success'
      )
      lastTime <- currentTime
    }

    dt <- outputCD4VL[UniqueId == uniqueId]

    x <- dt[, .(Gender, GroupedRegionOfOrigin, Mode, Age, DTime, Calendar, Consc, Consr)]
    y <- dt[, .(YVar)]
    z <- dt[, .(Consc, CobsTime, Consr, RobsTime, RLogObsTime2, DTime)]
    migTime <- dt[Ord == 1, Mig]
    upTime <- dt[Ord == 1, U]
    xAIDS <- as.matrix(dt[Ord == 1, .(1, as.integer(Gender == 'M'), Age)])
    maxDTime <- dt[, max(DTime)]

    if (dt[Ord == 1, KnownPrePost] != 'Unknown') {
      next
    }

    switch(dt[Ord == 1, Only],
      'Both' = {
        bFE <- params$bFE
        sigma2 <- params$sigma2
        varCovRE <- params$varCovRE
        func <- VPostW
      },
      'CD4 only' = {
        bFE <- params$bFECD4
        sigma2 <- params$sigma2CD4
        varCovRE <- params$varCovRECD4
        func <- VPostWCD4
      },
      'VL only' = {
        bFE <- params$bFEVL
        sigma2 <- params$sigma2VL
        varCovRE <- params$varCovREVL
        func <- VPostWVL
      }
    )

    fit1 <- try(integrate(
      func,
      lower = migTime, upper = upTime,
      x = x, y = y, z = z,
      xAIDS = xAIDS, maxDTime = maxDTime,
      betaAIDS = params$betaAIDS, kappa = params$kappa,
      bFE = bFE, sigma2 = sigma2, varCovRE = varCovRE
    ), silent = TRUE)
    fit2 <- try(integrate(
      func,
      lower = 0, upper = upTime,
      x = x, y = y, z = z,
      xAIDS = xAIDS, maxDTime = maxDTime,
      betaAIDS = params$betaAIDS, kappa = params$kappa,
      bFE = bFE, sigma2 = sigma2, varCovRE = varCovRE
    ), silent = TRUE)

    if (IsError(fit1) || IsError(fit2) || fit1$message != 'OK' || fit2$message != 'OK') {
      next
    } else {
      outputCD4VL[UniqueId == uniqueId, ProbPre := fit1$value / fit2$value]
    }
  }
  endTime <- Sys.time()
  if (countCD4VL > 0) {
      percComplete <- stringi::stri_pad_left(
        sprintf('%0.2f%%', i / countCD4VL * 100),
        width = 8
      )
      iterComplete <- stringi::stri_pad_left(
        sprintf('%d/%d', i, countCD4VL),
        width = countCD4VLNChar * 2 + 1
      )
      PrintAlert(
        '{percComplete} ({iterComplete}) - {.timestamp {prettyunits::pretty_dt(endTime - startTime)}}', # nolint
        type = 'success'
      )
  } else {
    PrintAlert('No CD4VL data to be processed')
  }
  PrintAlert('End time: {format(endTime)}')

  output <- list()
  if (nrow(outputAIDS) > 0) {
    output[['AIDS']] <- outputAIDS[Ord == 1, .(Imputation, RecordId, ProbPre)]
  }
  if (nrow(outputCD4VL) > 0) {
    output[['CD4VL']] <- outputCD4VL[Ord == 1, .(Imputation, RecordId, ProbPre)]
  }
  output <- rbindlist(output)

  if (nrow(output) == 0) {
    output <- data.table(
      Imputation = integer(),
      RecordId = character(),
      ProbPre = numeric()
    )
  }

  return(output)
}

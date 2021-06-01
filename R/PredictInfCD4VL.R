# data <- PrepareMigrantData()
PredictInfCD4VL <- function(
  baseCD4VL,
  bFE,
  sigma2,
  varCovRE
) {
  set.seed(10)

  baseCD4VL[, Ord := seq_len(.N), by = .(Id)]

  # Get the design matrices
  x <- split(
    baseCD4VL[, .(Id, Gender, GroupedRegion, Mode, AgeDiag, DTime, Calendar, Consc, Consr)],
    by = c('Id'),
    keep.by = FALSE
  )

  z <- split(
    baseCD4VL[, .(Id, Consc, CobsTime, Consr, RobsTime, RLogObsTime2, DTime)],
    by = c('Id'),
    keep.by = FALSE
  )

  y <- split(baseCD4VL[, .(Id, YVar)], by = c('Id'), keep.by = FALSE)

  u <- split(baseCD4VL[, .(Id, U)], by = c('Id'), keep.by = FALSE)

  ids <- unique(baseCD4VL$Id)
  only <- split(baseCD4VL[, .(Id, Only)], by = c('Id'), keep.by = FALSE)
  mig <- split(baseCD4VL[, .(Id, Mig)], by = c('Id'), keep.by = FALSE)
  known <- split(baseCD4VL[, .(Id, KnownPrePost)], by = c('Id'), keep.by = FALSE)

  # One row per subject
  baseCD4VLId <- baseCD4VL[Ord == 1]

  xAIDS <- cbind(
    1,
    as.integer(baseCD4VLId$Gender == 'Male'),
    baseCD4VLId$AgeDiag
  )
  maxDTime <- baseCD4VL[, .(DTime = max(DTime)), by = .(Id)]
  ind <- list()
  for (i in seq_along(unique(ids))) {
    ind[[i]] <- which(baseCD4VL$Id == ids[i])
  }

  baseCD4VL[, ProbPre := NA]

  for (i in seq_along(unique(ids))) {
    upTime <- u[[i]][1, U]
    migTime <- mig[[i]][1, Mig]

    if (known[[i]][1, KnownPrePost] != 'Unknown') {
      next
    }

    if (only[[i]][1, Only] == 'Both') {
      fit1 <- try(integrate(
        VPostW,
        lower = migTime,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFE,
        sigma2,
        varCovRE
      ), silent = TRUE)
      fit2 <- try(integrate(
        VPostW,
        lower = 0,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFE,
        sigma2,
        varCovRE
      ), silent = TRUE)

      if (IsError(fit1) || IsError(fit2)) {
        next
      } else {
        res <- fit1$value / fit2$value
      }

      if (fit1$message == 'OK' && fit2$message == 'OK') {
        baseCD4VL[ind[[i]], ProbPre := res]
      }
    }

    if (only[[i]][1, Only] == 'CD4 only') {
      fit1 <- try(integrate(
        VPostWCD4,
        lower = migTime,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFECD4,
        sigma2CD4,
        varCovRECD4
      ), silent = TRUE)
      fit2 <- try(integrate(
        VPostWCD4,
        lower = 0,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFECD4,
        sigma2CD4,
        varCovRECD4
      ), silent = TRUE)

      if (IsError(fit1) || IsError(fit2)) {
        next
      } else {
        res <- fit1$value / fit2$value
      }

      if (fit1$message == 'OK' && fit2$message == 'OK') {
        baseCD4VL[ind[[i]], ProbPre := res]
      }
    }

    if (only[[i]][1] == 'VL only') {
      fit1 <- try(integrate(
        VPostWVL,
        lower = migTime,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFEVL,
        sigma2VL,
        varCovREVL
      ), silent = TRUE)
      fit2 <- try(integrate(
        VPostWVL,
        lower = 0,
        upper = upTime,
        x = x[[i]],
        y = y[[i]],
        z = z[[i]],
        xAIDS = xAIDS[i, ],
        maxDTime = maxDTime[Id == i, DTime],
        betaAIDS = betaAIDS,
        bFEVL,
        sigma2VL,
        varCovREVL
      ), silent = TRUE)

      if (IsError(fit1) || IsError(fit2)) {
        next
      } else {
        res <- fit1$value / fit2$value
      }

      if (fit1$message == 'OK' && fit2$message == 'OK') {
        baseCD4VL[ind[[i]], ProbPre := res]
      }
    }
  }

}

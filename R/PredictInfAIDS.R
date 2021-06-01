PredictInfAIDS <- function(
  baseAIDS
) {
  set.seed(10)
  xAIDS <- cbind(
    1,
    as.integer(baseAIDS$Gender == 'Male'),
    baseAIDS$AgeDiag
  )

  # Posterior mean
  baseAIDS[, ProbPre := NA]
  for (i in seq_len(nrow(baseAIDS))) {
    fit1 <- try(integrate(
      VPostWAIDS,
      lower = baseAIDS$Mig[i],
      upper = baseAIDS$U[i],
      x = xAIDS[i, ],
      dTime = baseAIDS$DTime[i],
      betaAIDS,
      kappa
    ), silent = TRUE)

    fit2 <- try(integrate(
      VPostWAIDS,
      lower = 0,
      upper = baseAIDS$U[i],
      x = xAIDS[i, ],
      dTime = baseAIDS$DTime[i],
      betaAIDS,
      kappa
    ), silent = TRUE)

    if (IsError(fit1) || IsError(fit2)) {
      next
    } else {
      res <- fit1$value / fit2$value
    }

    if (fit1$message == 'OK' && fit2$message == 'OK') {
      baseAIDS[i, ProbPre := res]
    }
  }
}

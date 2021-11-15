LogPostW <- function(
  w,
  x,
  y,
  z,
  xAIDS,
  maxDTime,
  betaAIDS,
  kappa,
  bFE,
  sigma2,
  varCovRE
) {
  consc <- x[, Consc]
  consr <- x[, Consr]

  xAIDS[3] <- xAIDS[3] - w
  lambda <- exp(xAIDS %*% betaAIDS)

  # Formulae used for constructing the appropriate design matrices

  # Get the design matrix for CD4
  fxCD4 <- formula(
    YVar ~
    I(DTime + w) * Gender +
      I(DTime + w) * GroupedRegionOfOrigin +
      I(DTime + w) * Mode +
      I(DTime + w) * lspline::lspline(I(Age - w), knots = c(25, 35, 45)) +
      lspline::lspline(I(Calendar - w), knots = c(16, 22))
  )
  xCD4 <- model.matrix(fxCD4, data = cbind(y, x)[Consc == 1])
  dimCD4 <- dim(xCD4)

  # Get the design matrix for VL
  fxVR <- formula(
    YVar ~
    I(DTime + w) * Gender +
      I(DTime + w) * GroupedRegionOfOrigin +
      I(DTime + w) * Mode +
      I(DTime + w) * lspline::lspline(I(Age - w), knots = c(25, 35, 45)) +
      I(log(DTime + w + 0.013)) * Gender +
      I(log(DTime + w + 0.013)) * GroupedRegionOfOrigin +
      I(log(DTime + w + 0.013)) * Mode +
      I(log(DTime + w + 0.013)) * lspline::lspline(I(Age - w), knots = c(25, 35, 45)) +
      lspline::lspline(I(Calendar - w), knots = c(16, 22))
  )
  xVR <- model.matrix(fxVR, data = cbind(y, x)[Consr == 1])
  dimVR <- dim(xVR)

  # Combine into one design matrix
  x <- rbind(
    cbind(matrix(0, nrow = dimVR[1], ncol = dimCD4[2]), xVR),
    cbind(xCD4, matrix(0, nrow = dimCD4[1], ncol = dimVR[2]))
  )

  # Formula for the design matrices of the random effects
  fz <- formula(
    YVar ~
      -1 + Consc + I((DTime + w) * Consc) + Consr + I((DTime + w) * Consr) +
      I(log(DTime + w + 0.013) * Consr)
  )
  z <- model.matrix(fz, data = cbind(y, z))

  # Mean and variance of the normal distribution
  mu <- c(x %*% bFE)
  var <- z %*% tcrossprod(varCovRE, z) + diag(consr * sigma2[2] + consc * sigma2[1])

  p <- mvnfast::dmvn(y$YVar, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxDTime)^kappa

  # Return -p since the "optim" function works for minimization problems
  return(-p)
}

LogPostWCD4 <- function(
  w,
  x,
  y,
  z,
  xAIDS,
  maxDTime,
  betaAIDS,
  kappa,
  bFE,
  sigma2,
  varCovRE
) {
  xAIDS[3] <- xAIDS[3] - w
  lambda <- exp(xAIDS %*% betaAIDS)[1, 1]

  # Formula for design matrix of fixed effects
  fxCD4 <- formula(
    YVar ~
      I(DTime + w) * Gender +
      I(DTime + w) * GroupedRegionOfOrigin +
      I(DTime + w) * Mode +
      I(DTime + w) * lspline::lspline(I(Age - w), knots = c(25, 35, 45)) +
      lspline::lspline(I(Calendar - w), knots = c(16, 22))
  )
  x <- model.matrix(fxCD4, data = cbind(y, x))

  # Formula for design matrix of random effects
  fz <- formula(YVar ~ -1 + Consc + I(DTime + w))
  z <- model.matrix(fz, data = cbind(y, z))

  # Mean and variance of the normal kernel
  mu <- c(x %*% bFE)
  var <- z %*% tcrossprod(varCovRE, z) + sigma2 * diag(length(x[, 1]))

  p <- mvnfast::dmvn(y$YVar, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxDTime)^kappa

  return(-p)
}

LogPostWVL <- function(
  w,
  x,
  y,
  z,
  xAIDS,
  maxDTime,
  betaAIDS,
  kappa,
  bFE,
  sigma2,
  varCovRE
) {
  # Design matrix of the time-to-AIDS model
  xAIDS[3] <- xAIDS[3] - w
  lambda <- exp(xAIDS %*% betaAIDS)

  # Formula for design matrices
  fxVR <- formula(
    YVar ~
    I(DTime + w) * Gender +
      I(DTime + w) * GroupedRegionOfOrigin +
      I(DTime + w) * Mode +
      I(DTime + w) * lspline::lspline(I(Age - w), knots = c(25, 35, 45)) +
      I(log(DTime + w + 0.013)) * Gender +
      I(log(DTime + w + 0.013)) * GroupedRegionOfOrigin +
      I(log(DTime + w + 0.013)) * Mode +
      I(log(DTime + w + 0.013)) * lspline::lspline(I(Age - w), knots = c(25, 35, 45)) +
      lspline::lspline(I(Calendar - w), knots = c(16, 22))
  )

  x <- model.matrix(fxVR, data = cbind(y, x))

  # Formula for design matrix of random effects
  fz <- formula(YVar ~ -1 + Consr + I((DTime + w) * Consr) + I(log(DTime + w + 0.013) * Consr))
  z <- model.matrix(fz, data = cbind(y, z))

  # Mean and variance of the normal kernel
  mu <- c(x %*% bFE)
  var <- z %*% tcrossprod(varCovRE, z) + sigma2 * diag(length(x[, 1]))

  p <- mvnfast::dmvn(y$YVar, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxDTime)^kappa

  return(-p)
}

LogPostWAIDS <- function(
  w,
  x,
  dTime,
  betaAIDS,
  kappa
) {
  x[3] <- x[3] - w
  lambda <- exp(x %*% betaAIDS)[1, 1]

  val <- log(kappa) + log(lambda) + (kappa - 1) * log(w + dTime) - lambda * (w + dTime)^kappa
  return(-val)
}

# Posterior up to a proportionality constant
PostW <- function(w, ...) {
  return(exp(-LogPostW(w, ...)))
}

PostWCD4 <- function(w, ...) {
  return(exp(-LogPostWCD4(w, ...)))
}

PostWVL <- function(w, ...) {
  return(exp(-LogPostWVL(w, ...)))
}

PostWAIDS <- function(w, ...) {
  return(suppressWarnings(exp(-LogPostWAIDS(w, ...))))
}

# Vectorize the functions as the "integrate" function works with vectorized functions
VPostW <- Vectorize(PostW, vectorize.args = c('w'))
VPostWCD4 <- Vectorize(PostWCD4, vectorize.args = c('w'))
VPostWVL <- Vectorize(PostWVL, vectorize.args = c('w'))
VPostWAIDS <- Vectorize(PostWAIDS, vectorize.args = c('w'))

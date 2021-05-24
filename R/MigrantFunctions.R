# The function log_postW() gives the posterior distribution of w,
# i.e. the time gap between seroconversion and diagnosis
# We dropped the additive constants, i.e. the posterior distribution is unnormalized
LogPostW <- function(
  w,
  AIDSdateKnown = FALSE
) {
  # Design matrices of the i-th patient
  x <- x[[i]]
  z <- z[[i]]
  y <- y[[i]]

  consc <- x[, 'consc']
  consr <- x[, 'consr']

  # Design matrix of the time-to-AIDS model
  xaids <- xaids[i, ]
  xaids[3] <- xaids[3] - w
  maxdtime <- maxdtime[i]
  lambda <- exp(xaids %*% beta.aids)

  # Formulae used for constructing the appropriate design matrices

  # Get the design matrix for CD4
  fxcd4 <- formula(
    yvar ~
    I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )
  x.cd4 <- model.matrix(fxcd4, data = data.frame(yvar = y, x)[consc == 1, ])
  dim.cd4 <- dim(x.cd4)

  # Get the design matrix for VL
  fxvr <- formula(
    yvar ~
    I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      I(log(dtime + w + 0.013)) * gender +
      I(log(dtime + w + 0.013)) * region_group +
      I(log(dtime + w + 0.013)) * mode +
      I(log(dtime + w + 0.013)) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )

  x.vr <- model.matrix(fxvr, data = data.frame(yvar = y, x)[consr == 1, ])
  dim.vr <- dim(x.vr)

  # Combine into one design matrix
  x <- rbind(
    cbind(matrix(0, nr = dim.vr[1], nc = dim.cd4[2]), x.vr),
    cbind(x.cd4, matrix(0, nr = dim.cd4[1], nc = dim.vr[2]))
  )

  # Formula for the design matrices of the random effects
  fz <- formula(
    yvar ~
      -1 + consc + I((dtime + w) * consc) + consr + I((dtime + w) * consr) +
      I(log(dtime + w + 0.013) * consr)
  )
  z <- model.matrix(fz, data = data.frame(yvar = y, z))

  # Mean and variance of the normal distribution
  mu <- c(x %*% bFE)
  var <- z %*% tcrossprod(VarCovRE, z) + diag(consr * sigma2[2] + consc * sigma2[1])

  p <- dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  # Return -p since the "optim" function works for minimization problems
  return(-p)
}

LogPostWcd4 <- function(
  w
) {
  # Design matrices of i-patient
  x <- x[[i]]
  z <- z[[i]]
  y <- y[[i]]

  # Design matrix of the time-to-AIDS model
  xaids <- xaids[i, ]
  xaids[3] <- xaids[3] - w
  maxdtime <- maxdtime[i]
  lambda <- exp(xaids %*% beta.aids)

  # Formula for design matrix of fixed effects
  fxcd4 <- formula(
    yvar ~
      I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )
  x <- model.matrix(fxcd4, data = data.frame(yvar = y, x))

  # Formula for design matrix of random effects
  fz <- formula(yvar ~ -1 + consc + I(dtime + w))
  z <- model.matrix(fz, data = data.frame(yvar = y, z))

  # Mean and variance of the normal kernel
  mu <- c(x %*% bFE.CD4)
  var <- z %*% tcrossprod(VarCovRE.CD4, z) + sigma2.CD4 * diag(length(x[, 1]))

  p <- dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  return(-p)
}

LogPostWvl <- function(
  w
) {
  # Design matrices of i-patient
  x <- x[[i]]
  z <- z[[i]]
  y <- y[[i]]

  # Design matrix of the time-to-AIDS model
  xaids <- xaids[i, ]
  xaids[3] <- xaids[3] - w
  maxdtime <- maxdtime[i]
  lambda <- exp(xaids %*% beta.aids)

  # Formula for design matrices
  fxvr <- formula(
    yvar ~
      I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      I(log(dtime + w + 0.013)) * gender +
      I(log(dtime + w + 0.013)) * region_group +
      I(log(dtime + w + 0.013)) * mode +
      I(log(dtime + w + 0.013)) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )

  x <- model.matrix(fxvr, data = data.frame(yvar = y, x))

  # Formula for design matrix of random effects
  fz <- formula(yvar ~ -1 + consr + I((dtime + w) * consr) + I(log(dtime + w + 0.013) * consr))
  z <- model.matrix(fz, data = data.frame(yvar = y, z))

  # Mean and variance of the normal kernel
  mu <- c(x %*% bFE.VL)
  var <- z %*% tcrossprod(VarCovRE.VL, z) + sigma2.VL * diag(length(x[, 1]))

  p <- dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  return(-p)
}

# Posterior up to a proportionality constant
PostW <- function(w, ...) {
  return(exp(-LogPostW(w, ...)))
}

PostWcd4 <- function(w) {
  return(exp(-LogPostWcd4(w)))
}

PostWvl <- function(w) {
  return(exp(-LogPostWvl(w)))
}

MeanPostW <- function(w, ...) {
  return(w * PostW(w, ...))
}

MeanPostWcd4 <- function(w, ...) {
  return(w * PostWcd4(w, ...))
}

MeanPostWvl <- function(w, ...) {
  return(w * PostWvl(w, ...))
}

# Vectorize the functions as the "integrate" function works with vectorized functions
VlogPostW <- Vectorize(LogPostW)
VlogPostWcd4 <- Vectorize(LogPostWcd4)
VlogPostWvl <- Vectorize(LogPostWvl)
VpostW <- Vectorize(PostW)
VpostWcd4 <- Vectorize(PostWcd4)
VpostWvl <- Vectorize(PostWvl)
VmeanPostW <- Vectorize(MeanPostW)
VmeanPostWcd4 <- Vectorize(MeanPostWcd4)
VmeanPostWvl <- Vectorize(MeanPostWvl)

# AIDS only cases
LogPostWaids <- function(w) {
  x <- x.aids[i, ]
  x[3] <- x[3] - w
  dtime <- dtime[i]

  lambda <- exp(x %*% beta.aids)

  val <- log(kappa) + log(lambda) + (kappa - 1) * log(w + dtime) - lambda * (w + dtime)^kappa
  return(-val)
}

PostWaids <- function(w) {
  return(exp(-LogPostWaids(w)))
}

MeanPostWaids <- function(w) {
  return(w * PostWaids(w))
}

VlogPostWaids <- Vectorize(LogPostWaids)
VpostWaids <- Vectorize(PostWaids)
VmeanPostWaids <- Vectorize(MeanPostWaids)

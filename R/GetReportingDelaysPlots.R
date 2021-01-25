#' GetReportingDelaysPlots
#'
#' Create plot with overview of reporting delays data.
#'
#' @param inputData Pre-processed input data. Required.
#'
#' @return list object
#'
#' @examples
#' \dontrun{
#' GetReportingDelaysPlots(inputData)
#' }
#'
#' @export
GetReportingDelaysPlots <- function(
  inputData
) {
  stopifnot(!missing(inputData))

  densData <- inputData[!is.na(VarX)][VarX >= 0]
  if (densData[, .N > 0]) {
    allQuant95 <- unname(quantile(densData$VarX, probs = 0.95, na.rm = TRUE))
    allQuant99 <- unname(quantile(densData$VarX, probs = 0.99, na.rm = TRUE))

    allDensDelay <- density(densData$VarX, adjust = 4, n = 400)
    x <- allDensDelay$x
    y <- allDensDelay$y
    sel <- x >= 0 & x <= allQuant99
    x <- x[sel]
    y <- y[sel]
    allDensDelay <- lapply(seq_along(x), function(i) {
      list(x[i], y[i])
    })

  } else {
    allDensDelay <- list()
    allQuant95 <- 0
    allQuant99 <- 0
  }

  if (densData[Gender == 'F', .N > 0]) {
    fQuant95 <- unname(quantile(densData[Gender == 'F', VarX], probs = 0.95, na.rm = TRUE))
    fQuant99 <- unname(quantile(densData[Gender == 'F', VarX], probs = 0.99, na.rm = TRUE))

    fDensDelay <- density(densData[Gender == 'F', VarX], adjust = 4, n = 400)
    x <- fDensDelay$x
    y <- fDensDelay$y
    sel <- x >= 0 & x <= fQuant99
    x <- x[sel]
    y <- y[sel]
    fDensDelay <- lapply(seq_along(x), function(i) {
      list(x[i], y[i])
    })
  } else {
    fDensDelay <- list()
    fQuant95 <- 0
    fQuant99 <- 0
  }

  if (densData[Gender == 'M', .N > 0]) {
    mQuant95 <- unname(quantile(densData[Gender == 'M', VarX], probs = 0.95, na.rm = TRUE))
    mQuant99 <- unname(quantile(densData[Gender == 'M', VarX], probs = 0.99, na.rm = TRUE))

    mDensDelay <- density(densData[Gender == 'M', VarX], adjust = 4, n = 400)
    x <- mDensDelay$x
    y <- mDensDelay$y
    sel <- x >= 0 & x <= mQuant99
    x <- x[sel]
    y <- y[sel]
    mDensDelay <- lapply(seq_along(x), function(i) {
      list(x[i], y[i])
    })
  } else {
    mDensDelay <- list()
    mQuant95 <- 0
    mQuant99 <- 0
  }

  # ------------------------------------------------------------------------------------------------
  plot1 <- list(
    chartData = list(
      all = list(
        q95 = allQuant95,
        series = allDensDelay
      ),
      female = list(
        q95 = fQuant95,
        series = fDensDelay
      ),
      male = list(
        q95 = mQuant95,
        series = mDensDelay
      )
    )
  )

  return(plot1)
}

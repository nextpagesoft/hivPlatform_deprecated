PredictInfCD4VL <- function(baseCD4VL) {
  # In the biomarkers dataset VL measurements should be first
  set.seed(10)

  baseCD4VL[, Ord := seq_len(.N), by = .(Id)]

  # Get the design matrices
  x <- split(
    baseCD4VL[, .(Id, Gender, GroupedRegion, Mode, AgeDiag, DateOfExam, Calendar, Consc, Consr)],
    by = c('Id'),
    keep.by = FALSE
  )

  z <- split(
    baseCD4VL[, .(Id, Consc, CobsTime, Consr, RobsTime, RLogObsTime2, DateOfExam)],
    by = c('Id'),
    keep.by = FALSE
  )


  z <- lapply(split(
    baseCD4VL[, c("consc", "Cobstime", "consr", "Robstime", "RlogObstime2", "dtime")],
    baseCD4VL$id
  ), function(x) as.matrix(x))
  y <- split(baseCD4VL$yvar, baseCD4VL$id)
  u <- split(baseCD4VL$u, baseCD4VL$id)
  ids <- unique(baseCD4VL$id)
  only <- split(baseCD4VL$only, baseCD4VL$id)
  mig <- split(baseCD4VL$mig, baseCD4VL$id)
  known <- split(baseCD4VL$KnownPrePost, baseCD4VL$id)


  # One row per subject
  baseCD4VL.id <- baseCD4VL[baseCD4VL$ord == 1, ]


  xaids <- cbind(1, baseCD4VL.id$gender == "Male", baseCD4VL.id$ageDiag)
  maxdtime <- tapply(baseCD4VL$dtime, baseCD4VL$id, max)
  G <- length(unique(ids))
  ind = list()
  for (i in 1:G) {
    ind[[i]] = which(baseCD4VL$id == ids[i])
  }

  baseCD4VL$ProbPre = NA

}

#' GetAdjustmentSpecsWithParams
#'
#' @param params Params
#'
#' @return list
#'
#' @examples
#' GetAdjustmentSpecs(c('Multiple Imputation using Chained Equations - MICE'))
#'
#' @export
GetAdjustmentSpecsWithParams <- function(
  params
) {
  miAdjustment <- NULL
  if (params$MIAdjustType == 'jomo') {
    miAdjustment <- GetAdjustmentSpecs('Joint Modelling Multiple Imputation')
  } else if (params$MIAdjustType == 'mice') {
    miAdjustment <- GetAdjustmentSpecs('Multiple Imputation using Chained Equations - MICE')
  }

  rdAdjustment <- NULL
  if (params$RDAdjustType == 'withoutTrend') {
    rdAdjustment <- GetAdjustmentSpecs('Reporting Delays')
  } else if (params$RDAdjustType == 'withTrend') {
    rdAdjustment <- GetAdjustmentSpecs('Reporting Delays with trend')
  }

  if (!is.null(miAdjustment)) {
    if (miAdjustment[[1]]$SubType == 'JOMO') {
      miAdjustment[[1]]$Parameters$nimp$value <- params$MIParams$nimp
      miAdjustment[[1]]$Parameters$nburn$value <- params$MIParams$nburn
      miAdjustment[[1]]$Parameters$nbetween$value <- params$MIParams$nbetween
      miAdjustment[[1]]$Parameters$nsdf$value <- params$MIParams$nsdf
      miAdjustment[[1]]$Parameters$imputeRD$value <- params$MIParams$imputeRD
    } else if (miAdjustment[[1]]$SubType == 'MICE') {
      miAdjustment[[1]]$Parameters$nimp$value <- params$MIParams$nimp
      miAdjustment[[1]]$Parameters$nit$value <- params$MIParams$nit
      miAdjustment[[1]]$Parameters$nsdf$value <- params$MIParams$nsdf
      miAdjustment[[1]]$Parameters$imputeRD$value <- params$MIParams$imputeRD
    }
  }

  if (!is.null(rdAdjustment)) {
    rdAdjustment[[1]]$Parameters$startYear$value <- params$RDParams$startYear
    rdAdjustment[[1]]$Parameters$endYear$value <- params$RDParams$endYear
    rdAdjustment[[1]]$Parameters$endQrt$value <- params$RDParams$endQrt
    rdAdjustment[[1]]$Parameters$stratGender$value <- params$RDParams$stratGender
    rdAdjustment[[1]]$Parameters$stratTrans$value <- params$RDParams$stratTrans
    rdAdjustment[[1]]$Parameters$stratMigr$value <- params$RDParams$stratMigr
  }

  adjustmentSpecs <- union(miAdjustment, rdAdjustment)
  return(adjustmentSpecs)
}

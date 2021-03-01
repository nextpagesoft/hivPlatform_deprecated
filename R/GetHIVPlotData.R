#' GetHIVPlotData
#'
#' Get HIV plot data
#'
#' @param mainFitResult mainFitResult
#' @param bootstrapFitStats bootstrapFitStats
#'
#' @return data.table
#'
#' @examples
#' \dontrun{
#' GetHIVPlotData <- function(
#'   mainFitResult,
#'   bootstrapFitStats
#' )
#' }
#'
#' @export
GetHIVPlotData <- function(
  mainFitResult,
  bootstrapFitStats
) {
  dt <- mainFitResult$`0`$Results$MainOutputs[, .(
    Year,
    N_HIV_D, N_HIV_Obs_M,
    N_CD4_1_D, N_CD4_1_Obs_M,
    N_CD4_2_D, N_CD4_2_Obs_M,
    N_CD4_3_D, N_CD4_3_Obs_M,
    N_CD4_4_D, N_CD4_4_Obs_M,
    N_HIVAIDS_D, N_HIVAIDS_Obs_M,
    N_AIDS_D, N_AIDS_M,
    N_Inf_M,
    t_diag,
    N_Alive, N_Alive_Diag_M, N_Und,
    N_Und_Alive_p
  )]
  if (!is.null(bootstrapFitStats)) {
    modelColNames <- c(
      'N_HIV_Obs_M', 'N_CD4_1_Obs_M', 'N_CD4_2_Obs_M', 'N_CD4_3_Obs_M', 'N_CD4_4_Obs_M',
      'N_HIVAIDS_Obs_M', 'N_AIDS_M', 'N_Inf_M', 't_diag', 'N_Alive', 'N_Alive_Diag_M', 'N_Und',
      'N_Und_Alive_p'
    )
    for (modelColName in modelColNames) {
      confBounds <- bootstrapFitStats$MainOutputsStats[[modelColName]]
      colNames <- paste(modelColName, c('LB', 'UB', 'Range'), sep = '_')
      dt[confBounds, (colNames) := .(i.LB, i.UB, i.UB - i.LB), on = .(Year)]
    }
  }

  jsonDt <- jsonlite::toJSON(dt, dataframe = 'columns')

  return(jsonDt)
}

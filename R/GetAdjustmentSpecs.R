#' GetAdjustmentSpecs
#'
#' @param adjustmentNames adjustmentNames
#'
#' @return list
#'
#' @examples
#' GetAdjustmentSpecs(c('Multiple Imputation using Chained Equations - MICE'))
#'
#' @export
GetAdjustmentSpecs <- function(
  adjustmentNames
) {
  adjustmentFilePaths <- GetAdjustmentSpecFileNames()
  adjustmentSpecs <- setNames(
    lapply(
      adjustmentNames,
      function(adjName) GetListObject(adjustmentFilePaths[adjName])
    ),
    adjustmentNames
  )

  return(adjustmentSpecs)
}

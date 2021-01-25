#' GetAvailableStrata
#'
#' Get available strata
#'
#' @param dt dt
#' @param colNames colNames
#'
#' @return list
#'
#' @examples
#' dt <- data.table::data.table(
#'   Imputation = c(0, 1, 2),
#'   Gender = c('F', 'F', 'M'),
#'   Transmission = c('IDU', 'IDU', 'MSM')
#' )
#' GetAvailableStrata(dt, colNames = c('Gender', 'Transmission'))
#'
#' @export
GetAvailableStrata <- function(
  dt,
  colNames = c('Gender', 'Transmission', 'GroupedRegionOfOrigin', 'PlaceOfResidence')
) {
  result <- setNames(lapply(colNames, function(attr) {
    dt[Imputation != 0, as.character(unique(get(attr)))]
  }), colNames)

  return(result)
}

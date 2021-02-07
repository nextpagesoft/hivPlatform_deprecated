#' GetAvailableStrata
#'
#' Get available strata
#'
#' @param dt dt
#' @param variables variables
#'
#' @return list
#'
#' @examples
#' dt <- data.table::data.table(
#'   Imputation = c(0, 1, 2),
#'   Gender = c('F', 'F', 'M'),
#'   Transmission = c('IDU', 'IDU', 'MSM')
#' )
#' GetAvailableStrata(dt, variables = c('Gender' = 'G', 'Transmission' = 'T'))
#'
#' @export
GetAvailableStrata <- function(
  dt,
  variables = c(
    'Gender' = 'G',
    'Transmission' = 'T',
    'GroupedRegionOfOrigin' = 'O',
    'PlaceOfResidence' = 'R'
  )
) {
  colNames <- intersect(names(variables), colnames(dt))

  if (dt[, all(Imputation == 0)]) {
    data <- dt[Imputation == 0, ..colNames]
  } else {
    data <- dt[Imputation != 0, ..colNames]
  }
  setorderv(data, colNames)

  ConvertDataTableColumns(data, setNames(rep('string', length(colNames)), colNames))
  data[is.na(data)] <- 'NA'

  totalCount <- nrow(data)

  varCombinations <- unlist(lapply(
    seq_along(colNames), function(i) {
      combn(colNames, i, simplify = FALSE)
    }
  ), recursive = FALSE)

  strata <- lapply(varCombinations, function(varCombination) {
    strata <- data[, .(Count = .N), keyby = varCombination]
    strata[, Perc := Count / totalCount]
    strata[, (varCombination) := lapply(varCombination, function(colName) {
      sprintf('%s [%s]', get(colName), variables[colName])
    })]
    strata[,
      Combination := paste(.SD, collapse = ', '),
      by = seq_len(nrow(strata)),
      .SDcols = varCombination
    ]
    return(strata[, .(Combination, Count, Perc)])
  })
  names(strata) <- sapply(varCombinations, paste, collapse = ', ')

  return(list(
    Variables = variables[colNames],
    Strata = strata
  ))
}

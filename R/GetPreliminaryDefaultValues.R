#' GetPreliminaryDefaultValues
#'
#' Get preliminary default values for input data columns.
#'
#' @return list of default values
#'
#' @examples
#' GetPreliminaryDefaultValues()
#'
#' @export
GetPreliminaryDefaultValues <- function()
{
  columnSpecs <- GetListObject(
    GetSystemFile('referenceData/requiredColumns.R'),
    includeFileName = FALSE
  )
  defaultValues <- lapply(columnSpecs, function(columnSpec) {
    defaultValue <- columnSpec$defaultValue
    if (is.null(defaultValue)) {
      return('')
    } else {
      return(defaultValue)
    }
  })

  return(defaultValues)
}

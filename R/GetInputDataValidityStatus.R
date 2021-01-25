#' GetInputDataValidityStatus
#'
#' Validate input data.
#'
#' @param inputData Input data. Required.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' GetInputDataValidityStatus(inputData)
#' }
#'
#' @export
GetInputDataValidityStatus <- function(inputData)
{
  stopifnot(!missing(inputData))

  inputData <- copy(inputData)

  if (is.null(inputData)) {
    return(NULL)
  }

  columnSpecs <- GetListObject(
    GetSystemFile('referenceData/requiredColumns.R'),
    includeFileName = FALSE
  )

  checkStatus <- list()
  for (columnName in names(columnSpecs)) {
    columnSpec <- columnSpecs[[columnName]]
    allowedValues <- columnSpec$values
    restrictedValues <- columnSpec$restrictedValues

    wrongValues <- c()

    if (!is.null(restrictedValues)) {
      wrongValues <- c(
        wrongValues,
        restrictedValues[inputData[, restrictedValues %in% unique(get(columnName))]]
      )
    }

    if (!is.null(allowedValues)) {
      wrongValues <- c(
        wrongValues,
        inputData[!get(columnName) %in% c(allowedValues, NA), unique(get(columnName))]
      )
    }

    valid <- length(wrongValues) == 0

    checkStatus[[columnName]] <- list(Valid = valid, WrongValues = wrongValues)
  }

  return(list(
    Valid = all(sapply(checkStatus, '[[', 'Valid')),
    CheckStatus = checkStatus)
  )
}

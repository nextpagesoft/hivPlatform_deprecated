#' ApplyAttributesMapping
#'
#' Applies attributes mapping to original data
#'
#' @param originalData Original data. Required.
#' @param attrMapping List of attributes mapping between original data and internal representation.
#'   Required.
#'
#' @return data.table
#'
#' @examples
#' \dontrun{
#' ApplyAttributesMapping(originalData, attrMapping)
#' }
#'
#' @export
ApplyAttributesMapping <- function(
  originalData,
  attrMapping
) {
  stopifnot(!missing(originalData))
  stopifnot(!missing(attrMapping))

  # Check attributes mapping
  attrMappingStatus <- GetAttrMappingStatus(attrMapping)
  if (!attrMappingStatus$Valid) {
    return(NULL)
  }

  # Deal with mapped attributes
  nonMappedAttributes <- Filter(function(x) IsEmptyString(x$origColName), attrMapping)
  mappedAttributes <- Filter(function(x) !IsEmptyString(x$origColName), attrMapping)

  oldColNames <- unname(sapply(mappedAttributes, '[[', 'origColName'))
  newColNames <- unname(sapply(mappedAttributes, '[[', 'attribute'))

  outputData <- originalData[, ..oldColNames]
  setnames(outputData, oldColNames, newColNames)

  # Deal with non-mapped attributes
  for (nonMappedAttributeName in names(nonMappedAttributes)) {
    defValue <- attrMapping[[nonMappedAttributeName]]$defaultValue
    if (defValue %in% c('NA', '')) {
      defValue <- NA
    }
    outputData[, (nonMappedAttributeName) := defValue]
  }

  # Ensure columns types
  columnSpecs <- GetListObject(
    GetSystemFile('referenceData/requiredColumns.R'),
    includeFileName = FALSE
  )

  ConvertDataTableColumns(outputData, sapply(columnSpecs, '[[', 'type'))

  # Add Imputation column
  outputData[, Imputation := 0L]

  setcolorder(outputData, union('Imputation', names(attrMapping)))

  return(outputData)
}

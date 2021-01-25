#' GetPreliminaryAttributesMapping
#'
#' Gets attributes mappings.
#'
#' @param origData Original data. Required.
#' @param maxDistance Maximum allowed generalized Levenshtein distance between internal and origina
#'   column name. Optional. Default = 4
#'
#' @return list with attributes mapping
#'
#' @examples
#' \dontrun{
#' GetPreliminaryAttributesMapping(origData)
#' }
#'
#' @export
GetPreliminaryAttributesMapping <- function(
  origData,
  maxDistance = 4
) {
  stopifnot(!missing(origData))

  # Get required columns
  columnSpecs <- GetListObject(
    GetSystemFile('referenceData/requiredColumns.R'),
    includeFileName = FALSE
  )
  requiredColumnNames <- names(columnSpecs)
  origColNames <- names(origData)

  # Initialize column mapping
  attrMapping <- lapply(
    columnSpecs,
    modifyList,
    val = list(origColName = NULL),
    keep.null = TRUE
  )

  # 1. Fixed matching first
  for (requiredColumnName in requiredColumnNames) {
    fixedColNames <- columnSpecs[[requiredColumnName]]$candidateOrigColNames
    for (fixedColName in fixedColNames) {
      if (fixedColName %in% origColNames) {
        attrMapping[[requiredColumnName]]$origColName <- fixedColName
        requiredColumnNames <- setdiff(requiredColumnNames, requiredColumnName)
        origColNames <- setdiff(origColNames, fixedColName)
        break
      }
    }
  }

  # 2. Exact matching second
  exactMatchIndices <- match(tolower(requiredColumnNames), tolower(origColNames))
  for (i in seq_along(requiredColumnNames)) {
    requiredColumnName <- requiredColumnNames[i]
    exactMatchIndex <- exactMatchIndices[i]
    if (!is.na(exactMatchIndex)) {
      attrMapping[[requiredColumnName]]$origColName <- origColNames[exactMatchIndex]
    }
  }
  requiredColumnNames <- setdiff(
    requiredColumnNames,
    requiredColumnNames[!is.na(exactMatchIndices)]
  )
  origColNames <- setdiff(
    origColNames,
    origColNames[exactMatchIndices[!is.na(exactMatchIndices)]]
  )

  # 3. Fuzzy matching third
  for (requiredColumnName in requiredColumnNames) {
    # Fuzzy string matching
    distance <- as.vector(adist(requiredColumnName, origColNames, ignore.case = TRUE))
    if (min(distance) <= maxDistance) {
      bestMatchColumn <-
        origColNames[which.min(adist(requiredColumnName, origColNames, ignore.case = TRUE))]
      # Remove matched column from searching in the next step.
      if (!is.na(bestMatchColumn)) {
        attrMapping[[requiredColumnName]]$origColName <- bestMatchColumn
        origColNames <- setdiff(origColNames, bestMatchColumn)
      }
    }
  }

  return(attrMapping)
}

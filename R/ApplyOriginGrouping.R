#' ApplyOriginGrouping
#'
#' Applies RegionOfOrigin grouping map to the input data
#'
#' @param inputData Input data. Required.
#' @param originGrouping Data.table object with mapping from RegionOfOrigin to GroupOfOrigin.
#'   Required.
#'
#' @return inputData
#'
#' @examples
#' \dontrun{
#' ApplyOriginGrouping(inputData, originGrouping)
#' }
#'
#' @export
ApplyOriginGrouping <- function(
  inputData,
  originGrouping
) {
  if (length(originGrouping) > 0) {
    dtMap <- ConvertListToDt(originGrouping)
  } else {
    origin <- inputData[, sort(unique(FullRegionOfOrigin))]
    dtMap <- data.table(
      name = origin,
      origin = origin
    )
  }
  inputData[, GroupedRegionOfOrigin := 'UNK']
  inputData[
    dtMap,
    GroupedRegionOfOrigin := name,
    on = .(FullRegionOfOrigin = origin)
  ]

  inputData[, GroupedRegionOfOrigin := factor(GroupedRegionOfOrigin)]

  return(inputData)
}

#' GetOriginDistribution
#'
#' Get distribution of RegionOfOrigin
#'
#' @param inputData Input data. Required.
#'
#' @return data.table
#'
#' @examples
#' inputData <-
#'   data.table::data.table(FullRegionOfOrigin = c('REPCOUNTRY', 'SUBAFR', 'SUBAFR', 'UNK'))
#' GetOriginDistribution(inputData)
#'
#' @export
GetOriginDistribution <- function(
  inputData
) {
  distr <- inputData[, .(count = .N), by = .(origin = FullRegionOfOrigin)]
  distr[is.na(origin), origin := 'UNK']
  distr <- distr[order(-count)]
  distr <- rbind(
    distr[origin == 'REPCOUNTRY'],
    distr[!origin %chin% c('REPCOUNTRY', 'UNK', 'OTHER')],
    distr[origin == 'UNK'],
    distr[origin == 'OTHER']
  )
  return(distr)
}

#' GetOriginGroupingPreset
#'
#' Get mapping from RegionOfOrigin to GroupOfOrigin
#'
#' @param type Grouping type. Default = 'REPCOUNTRY + UNK + OTHER'
#' @param distr Distribution of RegionOfOrigin
#'
#' @return NULL
#'
#' @examples
#' distr <- data.table::data.table(
#'   origin = c('REPCOUNTRY', 'SUBAFR'),
#'   count = c(1536, 2237)
#' )
#' GetOriginGroupingPreset(
#'   type = 'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER',
#'   distr = distr
#' )
#'
#' @export
GetOriginGroupingPreset <- function(
  type = 'REPCOUNTRY + UNK + OTHER',
  distr
) {
  # Initialize mapping
  map <- c(
    'UNK', 'ABROAD', 'AUSTNZ', 'CAR', 'CENTEUR', 'EASTASIAPAC', 'EASTEUR', 'EUROPE', 'LATAM',
    'NORTHAFRMIDEAST', 'NORTHAM', 'REPCOUNTRY', 'SOUTHASIA', 'SUBAFR', 'WESTEUR'
  )
  names(map) <- map

  # Adjust according to type
  switch(
    type,
    'REPCOUNTRY + UNK + OTHER' = ,
    'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER' = {
      map[
        map %chin% c(
          'ABROAD', 'SUBAFR', 'WESTEUR', 'CENTEUR', 'EASTEUR', 'EASTASIAPAC', 'EUROPE', 'AUSTNZ',
          'SOUTHASIA', 'NORTHAFRMIDEAST', 'NORTHAM', 'CAR', 'LATAM'
        )
      ] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + SUBAFR + OTHER' = {
      map[
        map %chin% c(
          'ABROAD', 'WESTEUR', 'CENTEUR', 'EASTEUR', 'EASTASIAPAC', 'EUROPE', 'AUSTNZ', 'SOUTHASIA',
          'NORTHAFRMIDEAST', 'NORTHAM', 'CAR', 'LATAM'
        )
      ] <- 'OTHER'
    }
  )

  map <- as.data.table(map, keep.rownames = TRUE)
  setnames(map, c('origin', 'name'))

  if (type == 'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER') {
    sepRegions <- head(
      distr[!origin %chin% c('REPCOUNTRY', 'UNK'), origin],
      3
    )
    map[origin %chin% sepRegions, name := origin]
  }

  map <- ConvertOriginGroupingDtToList(map)

  return(map)
}

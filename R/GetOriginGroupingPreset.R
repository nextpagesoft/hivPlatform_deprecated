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
    'REPCOUNTRY', 'UNK', 'EASTEUR', 'CENTEUR', 'WESTEUR', 'NORTHAM', 'EUROPE', 'SUBAFR',
    'NORTHAFRMIDEAST', 'SOUTHASIA', 'EASTASIAPAC', 'CAR', 'LATAM', 'AUSTNZ', 'ABROAD'
  )
  names(map) <- map

  # Adjust according to type
  switch(
    type,
    'REPCOUNTRY + UNK + EUROPE + AFRICA + ASIA + OTHER' = ,
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER + AFRICA + ASIA + OTHER' = ,
    'REPCOUNTRY + UNK + EUROPE + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = ,
    'REPCOUNTRY + UNK + EUROPE + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = ,
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = , # nolint
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = , # nolint
    'REPCOUNTRY + UNK + EUROPE + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = , # nolint
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = { # nolint
      map[map %chin% c('EASTEUR')] <- 'EASTERN EUROPE'
      map[map %chin% c('CENTEUR', 'WESTEUR', 'EUROPE', 'NORTHAM')] <- 'EUROPE-OTHER'
      map[map %chin% c('SUBAFR')] <- 'SUB-SAHARAN AFRICA'
      map[map %chin% c('NORTHAFRMIDEAST')] <- 'AFRICA-OTHER'
      map[map %chin% c('CAR', 'LATAM')] <- 'CARIBBEAN-LATIN AMERICA'
      map[map %chin% c('SOUTHASIA', 'EASTASIAPAC')] <- 'ASIA'
      map[
        !(map %chin% c(
          'EASTERN EUROPE', 'EUROPE-OTHER', 'SUB-SAHARAN AFRICA', 'AFRICA-OTHER', 'ASIA',
          'CARIBBEAN-LATIN AMERICA', 'UNK', 'REPCOUNTRY'
        ))
      ] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + OTHER' = ,
    'REPCOUNTRY + UNK + 3 most prevalent regions + OTHER' = {
      map[
        map %chin% c(
          'ABROAD', 'SUBAFR', 'WESTEUR', 'CENTEUR', 'EASTEUR', 'EASTASIAPAC', 'EUROPE', 'AUSTNZ',
          'SOUTHASIA', 'NORTHAFRMIDEAST', 'NORTHAM', 'CAR', 'LATAM'
        )
      ] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + SUB-SAHARAN AFRICA + OTHER' = {
      map[map %chin% c('SUBAFR')] <- 'SUB-SAHARAN AFRICA'
      map[
        map %chin% c(
          'ABROAD', 'WESTEUR', 'CENTEUR', 'EASTEUR', 'EASTASIAPAC', 'EUROPE', 'AUSTNZ', 'SOUTHASIA',
          'NORTHAFRMIDEAST', 'NORTHAM', 'CAR', 'LATAM'
        )
      ] <- 'OTHER'
    }
  )

  # Second pass for migrant-compatible presets
  switch(type,
    'REPCOUNTRY + UNK + EUROPE + AFRICA + ASIA + OTHER' = {
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER')] <- 'EUROPE'
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER + AFRICA + ASIA + OTHER' = {
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EUROPE + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = {
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER')] <- 'EUROPE'
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EUROPE + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = {
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER')] <- 'EUROPE'
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
    },
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + OTHER' = { # nolint
      map[map %chin% c('CARIBBEAN-LATIN AMERICA')] <- 'OTHER'
    },
    'REPCOUNTRY + UNK + EASTERN EUROPE + EUROPE-OTHER + AFRICA + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = { # nolint
      map[map %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER')] <- 'AFRICA'
    },
    'REPCOUNTRY + UNK + EUROPE + SUB-SAHARAN AFRICA + AFRICA-OTHER + ASIA + CARIBBEAN-LATIN AMERICA + OTHER' = { # nolint
      map[map %chin% c('EASTERN EUROPE', 'EUROPE-OTHER')] <- 'EUROPE'
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

  # Add migrant mapping
  map[, migrant := 'OTHER']
  map[name %chin% c('EASTERN EUROPE', 'EUROPE-OTHER', 'EUROPE'), migrant := 'EUROPE']
  map[name %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER', 'AFRICA'), migrant := 'AFRICA']
  map[name %chin% c('ASIA'), migrant := 'ASIA']
  map[name %chin% c('REPCOUNTRY'), migrant := 'REPCOUNTRY']
  map[name %chin% c('UNK'), migrant := 'UNK']

  mapList <- ConvertOriginGroupingDtToList(map)

  return(mapList)
}

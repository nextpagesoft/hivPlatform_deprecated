CheckOriginGroupingForMigrant <- function(
  grouping,
  distr
) {
  if (length(grouping) > 0) {
    distrDt <- copy(distr)
    groupingDt <- ConvertListToDt(grouping)

    distrDt[
      groupingDt,
      GroupedRegionOfOrigin := i.name,
      on = .(origin)
    ]
    distrDt[is.na(GroupedRegionOfOrigin), GroupedRegionOfOrigin := 'UNK']

    distrDt[
      groupingDt,
      MigrantOrigin := i.migrant,
      on = .(GroupedRegionOfOrigin = name)
    ]
    distrDt[is.na(MigrantOrigin), MigrantOrigin := 'UNK']

    allowedNames <- c(
      'REPCOUNTRY',
      'EUROPE', 'EASTERN EUROPE', 'EUROPE-OTHER',
      'AFRICA', 'SUB-SAHARAN AFRICA', 'AFRICA-OTHER',
      'ASIA',
      'CARIBBEAN-LATIN AMERICA',
      'OTHER',
      'UNK'
    )

    result <- all(toupper(distrDt$MigrantOrigin) %chin% toupper(allowedNames))
  } else {
    result <- FALSE
  }

  return(result)
}

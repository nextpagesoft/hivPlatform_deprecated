ConvertOriginGroupingForMigrant <- function( # nolint
  grouping
) {
  stopifnot(CheckOriginGroupingForMigrant(grouping))

  groupingDt <- ConvertListToDt(grouping)
  groupingDt[name %chin% c('EASTERN EUROPE', 'EUROPE-OTHER'), name := 'EUROPE']
  groupingDt[name %chin% c('SUB-SAHARAN AFRICA', 'AFRICA-OTHER'), name := 'AFRICA']
  groupingDt[name %chin% c('CARIBBEAN-LATIN AMERICA'), name := 'OTHER']
  grouping <- ConvertOriginGroupingDtToList(groupingDt)

  return(grouping)
}

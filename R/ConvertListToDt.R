ConvertListToDt <- function(dtList) {
  dtMap <- rbindlist(lapply(dtList, as.data.table))
  return(dtMap)
}

ConvertListToDt <- function(dtList) {
  dtMap <- rbindlist(lapply(dtList, as.data.table), use.names = TRUE)
  return(dtMap)
}

GetPrelimAggrDataSelection <- function(
  aggrData
) {
  result <- lapply(
    names(aggrData),
    function(dataName) {
      dt <- aggrData[[dataName]]
      list(
        Name = dataName,
        Use = TRUE,
        MinYear = min(dt$Year),
        MaxYear = max(dt$Year)
      )
    }
  )
  return(result)
}

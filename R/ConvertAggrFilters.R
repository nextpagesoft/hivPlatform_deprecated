ConvertAggrFilters <- function(
  x,
  session,
  inputname
) {
  if (length(x) == 0) {
    return(NULL)
  }
  aggrDataSelection <- lapply(x, function(dataEl) {
    dataNames <- strsplit(dataEl$name, ', ')[[1]]
    els <- lapply(dataNames, function(dataName) {
      list(
        Name = dataName,
        Use = dataEl$use,
        MinYear = dataEl$years[[1]],
        MaxYear = dataEl$years[[2]]
      )
    })
  })
  aggrDataSelection <- unlist(aggrDataSelection, recursive = FALSE)
  aggrDataSelection <- data.table(
    Name = sapply(aggrDataSelection, '[[', 'Name'),
    Use = sapply(aggrDataSelection, '[[', 'Use'),
    MinYear = sapply(aggrDataSelection, '[[', 'MinYear'),
    MaxYear = sapply(aggrDataSelection, '[[', 'MaxYear')
  )

  return(aggrDataSelection)
}

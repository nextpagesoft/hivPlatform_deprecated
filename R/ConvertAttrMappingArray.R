ConvertAttrMappingArray <- function(
  x,
  session,
  inputname
) {
  # Restore names
  names(x) <- sapply(x, '[[', 'attribute')

  # Convert null to NA
  x <- lapply(x, jsonlite:::null_to_na)

  # Convert values from list of single values to a vector of values
  x <- lapply(x, function(el) {
    if ('values' %in% names(el)) {
      el$values <- sapply(el$values, '[[', 1)
    }
    return(el)
  })

  return(x)
}

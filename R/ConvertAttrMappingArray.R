ConvertAttrMappingArray <- function(
  x,
  session,
  inputname
) {
  # Restore names
  names(x) <- sapply(x, '[[', 'attribute')

  # Convert null to NA
  ConvertNULLToNA <- get('null_to_na', envir = asNamespace('jsonlite'), inherits = FALSE)
  x <- lapply(x, ConvertNULLToNA)

  # Convert values from list of single values to a vector of values
  x <- lapply(x, function(el) {
    if ('values' %in% names(el)) {
      el$values <- sapply(el$values, '[[', 1)
    }
    return(el)
  })

  return(x)
}

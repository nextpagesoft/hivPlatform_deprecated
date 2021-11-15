ConvertOriginGroupingArray <- function(
  x,
  session,
  inputname
) {
  originGrouping <- lapply(x, function(el) {
    origin <- ifelse1(length(el$origin) == 0, NA_character_, simplify2array(el$origin))
    list(
      name = el$name,
      origin = origin,
      migrant = el$migrant
    )
  })
  return(originGrouping)
}

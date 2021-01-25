ConvertOriginGroupingArray <- function(
  x,
  session,
  inputname
) {
  originGrouping <- lapply(x, function(el) {
    list(
      name = el$name,
      origin = simplify2array(el$origin)
    )
  })

  return(originGrouping)
}

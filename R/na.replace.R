#' na.replace
#'
#' Sets NAs to new value.
#'
#' @param x A numeric vector.
#' @param replace Value for replacing NAs
#'
#' @return Vector of length of x, but with NA's replaced by new value.
#'
#' @examples
#' na.replace(c(1, NA), 2)
#'
#' @export
na.replace <- function(x, replace) {
  x[is.na(x)] <- replace
  x
}

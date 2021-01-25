#' IsError
#'
#' Test if is an error
#'
#' @param x Object. Required.
#'
#' @return logical
#'
#' @examples
#' IsError(try({(function() stop())()}, silent = TRUE))
#' IsError(try({(function() 1 + 1)()}, silent = TRUE))
#'
#' @export
IsError <- function(
  x
) {
  return(inherits(x, 'try-error'))
}

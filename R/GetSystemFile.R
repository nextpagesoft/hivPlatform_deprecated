#' GetSystemFile
#'
#' Get the system path - 'inst' folder.
#'
#' @param ... Arguments for \code{\link{system.file}}
#' @param package Package name. Default = \code{'hivEstimatesAccuracy2'}.
#'
#' @return Path to a system file
#'
#' @examples
#' GetSystemFile('adjustments')
#'
#' @export
GetSystemFile <- function(
  ...,
  package = 'hivEstimatesAccuracy2'
) {
  return(system.file(..., package = package))
}

#' GetSystemFile
#'
#' Get the system path - 'inst' folder.
#'
#' @param ... Arguments for \code{\link{system.file}}
#' @param package Package name. Default = \code{'hivPlatform'}.
#'
#' @return Path to a system file
#'
#' @examples
#' GetSystemFile('adjustments')
#'
#' @export
GetSystemFile <- function(
  ...,
  package = 'hivPlatform'
) {
  return(system.file(..., package = package))
}

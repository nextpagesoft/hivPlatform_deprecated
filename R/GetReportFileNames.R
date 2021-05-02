#' GetReportFileNames
#'
#' Get the list of report file names in a specific folder. By default the search folder is
#' \code{reports} folder installed with package \code{\link{hivPlatform}}.
#'
#' @param path Path to the folder with adjustment specifications. Optional.
#'   Default = \code{\link{GetSystemFile}('reports')}.
#'
#' @return Character vector of adjustment specification file paths.
#'
#' @examples
#' \dontrun{
#' GetReportFileNames()
#' }
#'
#' @export
GetReportFileNames <- function(
  path = GetSystemFile('reports')
) {
  reportFileNames <- list.files(path, pattern = '.Rmd$', full.names = TRUE)
  reportNames <- sapply(reportFileNames, ReadRmdFrontMatter, section = 'name')
  names(reportFileNames) <- reportNames

  return(reportFileNames)
}

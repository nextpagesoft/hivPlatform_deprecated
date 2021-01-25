#' AppUI
#'
#' Application client logic
#'
#' @return html
#'
#' @export
AppUI <- function() {
  wwwPath <- GetSystemFile('app/www')
  shiny::addResourcePath('www', wwwPath)
  shiny::htmlTemplate(file.path(wwwPath, 'index.html'))
}

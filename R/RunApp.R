#' RunApp
#'
#' Run the application.
#'
#' @param launchBrowser Logical indicating to open the app in a newly open web browser
#' @param trace Logical indicating to output detailed debug info in R session
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' RunApp()
#' }
#'
#' @export
RunApp <- function(
  launchBrowser = getOption('shiny.launch.browser', interactive()),
  trace = FALSE
) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  options(shiny.trace = trace)
  app <- shiny::shinyApp(AppUI, AppServer)
  shiny::runApp(
    app,
    port = 3306,
    display.mode = 'normal',
    test.mode = FALSE,
    launch.browser = launchBrowser
  )
  return(invisible(NULL))
}

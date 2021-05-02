#' AppServer
#'
#' Application server logic
#'
#' @param input Input
#' @param output Output
#' @param session Session
#'
#' @return NULL (invisibly)
#'
#' @export
AppServer <- function(
  input,
  output,
  session
) {
  appMgr <- AppManager$new(session)

  # Respond to events
  Events(input, output, session, appMgr)

  apiURL <- session$registerDataObj(
    name = 'api',
    data = list(),
    filter = function(data, req) {
      print(ls(req))
      print(req$rook.input)
      print(req$REQUEST_METHOD)

      params <- parseQueryString(req$QUERY_STRING)
      print(params)

      json <- 'Test text'
      fileName <- 'Test.txt'
      ext <- paste0('.', tools::file_ext(fileName))
      tmpFile <- tempfile(fileext = ext)
      writeLines(json, tmpFile)
      shiny::httpResponse(
        200,
        'application/octet-stream',
        list(file = tmpFile, owned = TRUE),
        c(
          'Content-Disposition' = 'attachment; filename="Test2.txt"',
          'Cache-Control' = 'no-cache'
        )
      )
    }
  )
  session$sendCustomMessage('apiURL', list(url = apiURL))

  session$onSessionEnded(stopApp)

  return(invisible(NULL))
}

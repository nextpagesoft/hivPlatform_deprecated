#' PrintH1
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintH1('Test')
#'
#' @export
PrintH1 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  formattedText <- capt0(cli::cli_h1(CollapseTexts(..., collapse = collapse), .envir = .envir))
  cat(formattedText)

  invisible(NULL)
}

#' PrintH2
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintH2('Test')
#'
#' @export
PrintH2 <- function(
  ...,
  collapse = ' ',
  .envir = parent.frame()
) {
  formattedText <- capt0(cli::cli_h2(CollapseTexts(..., collapse = collapse), .envir = .envir))
  cat(formattedText)

  invisible(NULL)
}

#' PrintAlert
#'
#' @param ... Text to be printed
#' @param collapse String to be used for concatenating texts
#' @param type Type of alert
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintAlert('Test')
#' PrintAlert('Test', type = 'success')
#' PrintAlert('Test', type = 'danger')
#' PrintAlert('Test', type = 'warning')
#' PrintAlert('Test', type = 'info')
#'
#' @export
PrintAlert <- function(
  ...,
  collapse = ' ',
  type = 'info',
  .envir = parent.frame()
) {

  alertFunc <- switch(
    tolower(type),
    'danger'  = cli::cli_alert_danger,
    'warning' = cli::cli_alert_warning,
    'info'    = cli::cli_alert_info,
    'success' = cli::cli_alert_success,
    'text'    = cli::cli_text,
    cli::cli_alert
  )

  formattedText <- capt0(alertFunc(CollapseTexts(..., collapse = collapse), .envir = .envir))
  cat(formattedText)

  invisible(NULL)
}

#' PrintBullets
#'
#' @param items Vector of text items
#' @param .envir Environment for lookup of variables referenced in the text
#'
#' @return NULL
#'
#' @examples
#' PrintBullets(c('Item 1', 'Item 2'))
#'
#' @export
PrintBullets <- function(
  items = c(),
  .envir = parent.frame()
) {
  formattedText <- capt0({
    cli::cli_ul()
    sapply(items, cli::cli_li, .envir = .envir)
    cli::cli_end()
  })
  cat(formattedText)

  invisible(NULL)
}

#' CollapseTexts
#'
#' @param ... Text to be collapsed
#' @param collapse String to be used for concatenating texts
#'
#' @return NULL
#'
#' @examples
#' CollapseTexts('Item 1', 'Item 2')
#'
#' @export
CollapseTexts <- function(
  ...,
  collapse = ' '
) {
  return(paste(list(...), collapse = collapse))
}

capture_messages <- function(expr) {
  msgs <- character()
  i <- 0
  suppressMessages(withCallingHandlers(
    expr,
    message = function(e) msgs[[i <<- i + 1]] <<- conditionMessage(e)
  ))
  paste0(msgs, collapse = "")
}

capt0 <- function(expr, strip_style = FALSE) {
  out <- capture_messages(expr)
  if (strip_style) cli::ansi_strip(out) else out
}

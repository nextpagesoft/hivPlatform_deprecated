#' WriteExcelFile
#'
#' Write an object to an Excel file.
#'
#' @param data Object to save. Required.
#' @param fileName Name of the saved file. Required.
#' @param template Full path to input template Excel file. Optional. Default = \code{NULL}.
#' @param sheetName Name of the sheet were exported data should be saved. Default = 'DATA'.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' WriteExcelFile(
#'   data = appMgr$HIVModelMgr$PlotData,
#'   fileName = 'D:/Charts.xlsm',
#'   template = GetSystemFile('templates', 'Charts.xlsm'),
#'   sheetName = 'DATA'
#' )
#' WriteExcelFile(
#'   data = appMgr$HIVModelMgr$PlotData,
#'   fileName = 'D:/Charts_withoutMacro.xlsx',
#'   template = GetSystemFile('templates', 'Charts_withoutMacro.xlsx'),
#'   sheetName = 'DATA'
#' )
#' }
#'
#' @export
WriteExcelFile <- function(
  data,
  fileName,
  sheetName = 'DATA',
  template = NULL
) {
  stopifnot(!missing(data))
  stopifnot(!missing(fileName))

  if (!is.null(template) && file.exists(template)) {
    wb <- openxlsx::loadWorkbook(template)
  } else {
    wb <- openxlsx::createWorkbook()
  }

  if (!is.null(sheetName) && nchar(sheetName) > 0 && !(sheetName %in% names(wb))) {
    openxlsx::addWorksheet(wb = wb, sheetName = sheetName)
  }

  openxlsx::writeData(
    wb = wb,
    x = data,
    sheet = sheetName
  )

  openxlsx::saveWorkbook(wb, fileName, overwrite = TRUE)

  return(NULL)
}

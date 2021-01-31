#' ReadDataFile
#'
#' Read content of a data file, either Excel or text.\cr
#' It is a wrapper around \code{\link{ReadExcelFile}} and \code{\link{ReadTextFile}}. Appropriate
#' loading function is resolved by the extension of \code{fileName}.
#'
#' @param fileName Name of file to read. Required.
#' @param fileType Type of data to read. Required if \code{fileName} has no file extension,
#'   otherwise optional.
#' @param ... Additional parameters passed to \code{\link{ReadExcelFile}} or
#'   \code{\link{ReadTextFile}}. Optional.
#'
#' @return data.table object.
#'
#' @examples
#' \dontrun{
#' ReadDataFile(fileName)
#' ReadDataFile(fileName, "xlsx")
#' ReadDataFile(fileName, "csv")
#' }
#'
#' @export
ReadDataFile <- function(
  fileName,
  fileType,
  ...
) {
  stopifnot(!missing(fileName))

  localFileName <- fileName

  if (tools::file_ext(fileName) == 'zip') {
    zippedFilesList <- as.character(unzip(fileName, list = TRUE)$Name)
    if (length(zippedFilesList) == 1) {
      zippedFile <- utils::unzip(zipfile = fileName, files = zippedFilesList[1], exdir = tempdir())
      localFileName <- zippedFile
      on.exit({
        unlink(zippedFile)
      })
    }
  }

  # Get file type
  if (missing(fileType)) {
    fileType <- tolower(tools::file_ext(localFileName))
  }

  if (IsEmptyString(fileType)) {
    stop('Undetected file type')
  }

  # Run appropriate reading function
  data <- switch(
    fileType,
    'xls'  = {
      dt <- ReadExcelFile(
        localFileName,
        fileType,
        col_types = 'list',
        guess_max = 0,
        progress = FALSE,
        ...
      )
      colNames <- colnames(dt)
      dt[, (colNames) := lapply(.SD, function(col) sapply(col, as.character)), .SDcols = colNames]
    },
    'xlsx' = {
      dt <- ReadExcelFile(
        localFileName,
        fileType,
        col_types = 'list',
        guess_max = 0,
        progress = FALSE,
        ...
      )
      colNames <- colnames(dt)
      dt[, (colNames) := lapply(.SD, function(col) sapply(col, as.character)), .SDcols = colNames]
    },
    'txt'  = ReadTextFile(localFileName, colClasses = 'character', ...),
    'csv'  = ReadTextFile(localFileName, colClasses = 'character', ...),
    'rds'  = ReadRdsFile(localFileName, ...),
    'Unsupported file extension'
  )

  # Convert all strings to upper case
  colClasses <- sapply(sapply(data, class), '[[', 1)
  charColNames <- names(colClasses[colClasses == 'character'])
  data[, (charColNames) := lapply(.SD, toupper), .SDcols = charColNames]

  # Replace UNKs and BLANKS with NAs
  for (colName in charColNames) {
    data[get(colName) %chin% c('UNK', 'NA', ''), (colName) := NA_character_]
  }

  return(data)
}

#' GetMissingnessPlots
#'
#' Create plot with overview of missing data.
#'
#' @param inputData Pre-processed input data. Required.
#' @param columnNames Names of columns in \code{inputData} selected for missingness plot. Optional.
#'   Default = \code{c('Transmission', 'GroupedRegionOfOrigin', 'Gender', 'Age', 'FirstCD4Count')}
#' @param labels Labels to be displayed instead of strings in \code{columnNames}. Optional.
#'   Default = \code{c('Transm.', 'Migrant', 'Gender', 'Age', 'CD4')}
#'
#' @return list object
#'
#' @examples
#' \dontrun{
#' GetMissingnessPlots(inputData)
#' }
#'
#' @export
GetMissingnessPlots <- function(
  inputData,
  columnNames = c('Transmission', 'GroupedRegionOfOrigin', 'Age', 'FirstCD4Count'),
  labels = c('Transmission', 'Migrant', 'Age', 'CD4')
)
{
  stopifnot(!missing(inputData))
  stopifnot(length(columnNames) == length(labels))
  stopifnot(length(columnNames) == length(unique(columnNames)))
  stopifnot(length(labels) == length(unique(labels)))

  GetRelFreq <- function(x) sum(x) / length(x)
  IsMissing <- function(x) as.integer(is.na(x))
  AllPresent <- function(x) ifelse(all(x == 1), 'Present', 'Missing')
  Negate <- function(x) as.integer(!(x))

  missData <- inputData[, union(c('Gender', 'YearOfHIVDiagnosis'), columnNames), with = FALSE]
  missData[, c(columnNames) := lapply(.SD, IsMissing), .SDcols = columnNames]

  allStat <- missData[, lapply(.SD, GetRelFreq), .SDcols = columnNames]
  colOrder <- order(allStat, decreasing = TRUE)
  chartCategories <- labels[colOrder]
  labels <- labels[colOrder]
  columnNames <- columnNames[colOrder]
  setcolorder(missData, union(c('Gender', 'YearOfHIVDiagnosis'), columnNames))

  # ------------------------------------------------------------------------------------------------
  plot1Data <- rbind(
    missData[, lapply(.SD, GetRelFreq), .SDcols = columnNames, by = .(Gender)],
    allStat[, Gender := 'A']
  )

  plot1 <- list(
    chartCategories = chartCategories,
    chartData = list(
      all = unlist(unname(plot1Data[Gender == 'A', -c('Gender')])),
      female = unlist(unname(plot1Data[Gender == 'F', -c('Gender')])),
      male = unlist(unname(plot1Data[Gender == 'M', -c('Gender')]))
    )
  )

  # ------------------------------------------------------------------------------------------------
  missData[, GenderGroupSize := .N, by = .(Gender)]
  plot2Data <- rbind(
    missData[, .(Percentage = .N / GenderGroupSize[1]), by = c('Gender', columnNames)],
    missData[, .(Percentage = .N / nrow(missData)), by = c(columnNames)][, Gender := 'A']
  )
  setorder(plot2Data, Gender, -Percentage)
  plot2Data[, (columnNames) := lapply(.SD, Negate), .SDcols = columnNames]

  plot2 <- list(
    chartCategories = chartCategories,
    chartData = list(
      all = unname(as.matrix(plot2Data[Gender == 'A', -c('Gender', 'Percentage')])),
      female = unname(as.matrix(plot2Data[Gender == 'F', -c('Gender', 'Percentage')])),
      male = unname(as.matrix(plot2Data[Gender == 'M', -c('Gender', 'Percentage')]))
    )
  )

  # ------------------------------------------------------------------------------------------------
  plot2Data[, name := apply(.SD, 1, AllPresent), .SDcols = columnNames]
  setorder(plot2Data, Gender, Percentage)
  setnames(plot2Data, c('Percentage'), c('y'))
  plot3 <- list(
    chartData = list(
      all = apply(plot2Data[Gender == 'A'], 1, function(x) list(name = x[['name']], y = x[['y']])),
      female = apply(plot2Data[Gender == 'F'], 1, function(x) list(name = x[['name']], y = x[['y']])),
      male = apply(plot2Data[Gender == 'M'], 1, function(x) list(name = x[['name']], y = x[['y']]))
    )
  )

  # ------------------------------------------------------------------------------------------------
  chartCategories <-
    min(missData$YearOfHIVDiagnosis, na.rm = TRUE):max(missData$YearOfHIVDiagnosis, na.rm = TRUE)
  combinations <- CJ(
    Gender = c('A', 'F', 'M'),
    YearOfHIVDiagnosis = chartCategories
  )
  plot4Data <- rbind(
    missData[, lapply(.SD, GetRelFreq), by = .(Gender, YearOfHIVDiagnosis), .SDcols = columnNames],
    missData[, lapply(.SD, GetRelFreq), by = .(YearOfHIVDiagnosis), .SDcols = columnNames][, Gender := 'A']
  )
  plot4Data <- merge(
    combinations,
    plot4Data,
    by = c('Gender', 'YearOfHIVDiagnosis'),
    all.x = TRUE
  )
  setnames(plot4Data, columnNames, labels)

  plot4 <- list(
    chartCategories = chartCategories,
    chartData = list(
      all = lapply(labels, function(label) list(
        name = label,
        data = plot4Data[Gender == 'A', get(label)]
      )),
      female = lapply(labels, function(label) list(
        name = label,
        data = plot4Data[Gender == 'F', get(label)]
      )),
      male = lapply(labels, function(label) list(
        name = label,
        data = plot4Data[Gender == 'M', get(label)]
      ))
    )
  )

  return(
    list(
      plot1 = plot1,
      plot2 = plot2,
      plot3 = plot3,
      plot4 = plot4
    )
  )
}

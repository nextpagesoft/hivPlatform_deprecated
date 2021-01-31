#' RunAdjustments
#'
#' Execute adjustments specified in \code{adjustmentFileNames}.
#'
#' @param data data table object on which adjustments should be applied. Required.
#' @param adjustmentSpecs List of adjustment specifications to execute. Optional.
#'   Default = \code{list()}.
#' @param diagYearRange Numeric vector of length two with lower and upper bound for diagnosis year.
#'   Optional. Default = \code{NULL}.
#' @param notifQuarterRange Numeric vector of length two with lower and upper bound for
#'   notification quarter. Optional. Default = \code{NULL}.
#' @param seed Random seed. Optional. Default = NULL
#'
#' @return data table object after adjustments applied.
#'
#' @examples
#' \dontrun{
#' RunAdjustments(data)
#' }
#'
#' @export
RunAdjustments <- function(
  data,
  adjustmentSpecs = list(),
  diagYearRange = NULL,
  notifQuarterRange = NULL,
  seed = NULL
) {
  stopifnot(!missing(data))

  if (is.null(adjustmentSpecs) || length(adjustmentSpecs) == 0) {
    PrintAlert('No adjustments to process')
  }

  # Data table performs many operations by reference.
  # We make a copy of the data to make sure the input object is not changed by the adjustment
  # procedures.
  data <- copy(data)
  if (!is.null(diagYearRange)) {
    data <- data[HIVDiagnosisYear %between% diagYearRange | is.na(DateOfDiagnosis)]
  }
  if (!is.null(notifQuarterRange)) {
    data <- data[NotificationTime %between% notifQuarterRange | is.na(NotificationTime)]
  }

  PreProcessInputDataBeforeAdjustments(data)

  # Process adjustments
  set.seed(seed)
  results <- list()
  for (i in seq_along(adjustmentSpecs)) {
    adjustmentSpec <- adjustmentSpecs[[i]]

    caption <- sprintf('%d. %s', i, adjustmentSpec$Name)
    if (!('Key' %in% names(adjustmentSpec))) {
      adjustmentSpec$Key <- caption
    }

    PrintH1('Adjustment {caption}')
    cat('\n')

    startTime <- Sys.time()
    PrintAlert('Start time: {startTime}')

    # Extract parameters for better visibility.
    parameters <- GetParamInfoFromAdjustSpec(adjustmentSpec$Parameters, infoType = 'value')

    PrintH2('Parameters')
    print(setNames(as.character(parameters), names(parameters)))
    cat('\n')

    PrintH2('Executing')

    # Run adjustment function
    output <- adjustmentSpec$AdjustmentFunction(inputData = data, parameters = parameters)

    if ('Imputation' %in% colnames(output$Data)) {
      setorderv(output$Data, 'Imputation')
    }

    adjustResults <- list(
      Data = output$Data,
      Artifacts = output$Artifacts,
      Parameters = parameters,
      RunIdx = i,
      Name = adjustmentSpec$Name,
      Type = adjustmentSpec$Type,
      SubType = adjustmentSpec$SubType,
      TimeStamp = GetTimeStamp()
    )

    cat('\n')
    PrintH2('Rendering diagnostics report')
    report <- RenderReportForAdjSpec(
      adjustmentSpec,
      'intermediate',
      adjustResults
    )

    adjustResults <- modifyList(
      adjustResults,
      list(Report = report)
    )

    # Store intermediate results for later reference
    results[[adjustmentSpec$Key]] <- adjustResults

    endTime <- Sys.time()
    cat('\n')
    PrintH2('Done')
    PrintAlert('End time: {endTime}')
    PrintAlert('Elapsed time: {.timestamp {prettyunits::pretty_dt(endTime - startTime)}}')
  }

  return(results)
}

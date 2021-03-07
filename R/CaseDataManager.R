#' CaseDataManager
#'
#' R6 class for representing the case-based data manaager
#'
#' @name CaseDataManager
#' @examples
#' caseMgr <- CaseDataManager$new()
NULL

#' @export
CaseDataManager <- R6::R6Class(
  classname = 'CaseDataManager',
  class = FALSE,
  cloneable = FALSE,
  public = list(

    # GENERIC METHOD ===============================================================================
    initialize = function(
      session = NULL,
      appMgr = NULL
    ) {
      private$AppMgr <- appMgr
      private$Session <- session
      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        FileName = NULL,
        OriginalData = NULL,
        AttrMapping = NULL,
        AttrMappingStatus = NULL,
        OriginDistribution = NULL,
        OriginGrouping = list(),
        PreProcessArtifacts = NULL,
        Summary = NULL,
        Filters = NULL,
        PreProcessedData = NULL,
        PreProcessedDataStatus = NULL,
        AdjustedData = NULL,
        AdjustmentTask = NULL,
        AdjustmentResult = NULL
      )
    },

    print = function() {
      print('CaseDataManager')
    },

    # USER ACTIONS =================================================================================

    # 1. Read case-based data ----------------------------------------------------------------------
    ReadData = function(
      fileName
    ) {
      if (!is.element(private$AppMgr$Steps['SESSION_INITIALIZED'], private$AppMgr$CompletedSteps)) {
        PrintAlert(
          'AppManager is not initialized properly before reading data',
          type = 'danger'
        )
        return(invisible(self))
      }

      status <- 'SUCCESS'
      msg <- 'Data read correctly'
      tryCatch({
        originalData <- ReadDataFile(fileName)
        attrMapping <- GetPreliminaryAttributesMapping(originalData)
        attrMappingStatus <- GetAttrMappingStatus(attrMapping)
      },
      error = function(e) {
        status <<- 'FAIL'
        msg <<-
          'There was a difficulty encountered when reading the data file. It has not been loaded.'
      })

      if (status == 'SUCCESS') {
        private$Catalogs$FileName <- fileName
        private$Catalogs$OriginalData <- originalData
        private$Catalogs$AttrMapping <- attrMapping
        private$Catalogs$AttrMappingStatus <- attrMappingStatus
        private$InvalidateAfterStep('CASE_BASED_READ')
        PrintAlert('Data file {.file {fileName}} loaded')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          ColumnNames = colnames(originalData),
          RecordCount = nrow(originalData),
          AttrMapping = unname(attrMapping),
          AttrMappingStatus = attrMappingStatus
        )
      } else {
        PrintAlert('Loading data file {.file {fileName}} failed', type = 'danger')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      private$SendMessage('CASE_BASED_DATA_READ', payload)

      return(invisible(self))
    },

    # 2. Apply attributes mapping ------------------------------------------------------------------
    ApplyAttributesMapping = function(
      attrMapping
    ) {
      private$SendMessage(
        'CASE_BASED_ATTRIBUTE_MAPPING_APPLY_START',
        list(ActionStatus = 'SUCCESS')
      )

      if (!is.element(private$AppMgr$Steps['CASE_BASED_READ'], private$AppMgr$CompletedSteps)) {
        PrintAlert('Data must be read before applying atrributes mapping', type = 'danger')
        return(invisible(self))
      }

      status <- 'SUCCESS'
      msg <- 'Attributes applied correctly'
      tryCatch({
        originalData <- private$Catalogs$OriginalData
        if (missing(attrMapping)) {
          attrMapping <- GetPreliminaryAttributesMapping(originalData)
        }
        attrMappingStatus <- GetAttrMappingStatus(attrMapping)

        if (attrMappingStatus$Valid) {
          data <- ApplyAttributesMapping(originalData, attrMapping)
          preProcessArtifacts <- PreProcessInputDataBeforeSummary(data)
          PreProcessInputDataBeforeAdjustments(data)
          dataStatus <- GetInputDataValidityStatus(data)
          if (dataStatus$Valid) {
            originDistribution <- GetOriginDistribution(data)
            originGroupingType <- 'REPCOUNTRY + UNK + OTHER'
            origingGrouping <- GetOriginGroupingPreset(originGroupingType, originDistribution)
          } else {
            msg <- 'Data pre-processing did not succeed'
            status <- 'FAIL'
          }
        } else {
          msg <- 'Attributes mapping has invalid status'
          status <- 'FAIL'
        }
      },
      error = function(e) {
        msg <<- sprintf('Applying attributes mapping failed: %s', e$message)
        status <<- 'FAIL'
      })

      if (status == 'SUCCESS') {
        private$Catalogs$AttrMapping <- attrMapping
        private$Catalogs$AttrMappingStatus <- attrMappingStatus
        private$Catalogs$OriginDistribution <- originDistribution
        private$Catalogs$PreProcessArtifacts <- preProcessArtifacts
        private$Catalogs$PreProcessedData <- data
        private$Catalogs$PreProcessedDataStatus <- dataStatus
        private$InvalidateAfterStep('CASE_BASED_ATTR_MAPPING')
        PrintAlert('Attribute mapping has been applied')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          OriginDistribution = originDistribution,
          OriginGroupingType = originGroupingType,
          OriginGrouping = origingGrouping
        )
      } else {
        PrintAlert(msg, type = 'danger')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      private$SendMessage('CASE_BASED_ATTRIBUTE_MAPPING_APPLY_END', payload)
      return(invisible(self))
    },

    # 3. Apply origin grouping ---------------------------------------------------------------------
    ApplyOriginGrouping = function(
      originGrouping,
      originGroupingType = 'CUSTOM'
    ) {
      if (!is.element(
        private$AppMgr$Steps['CASE_BASED_ATTR_MAPPING'],
        private$AppMgr$CompletedSteps
      )) {
        PrintAlert(
          'Atrributes mapping must be applied before applying origin grouping',
          type = 'danger'
        )
        return(invisible(self))
      }

      status <- 'SUCCESS'
      msg <- 'Grouping applied correctly'
      tryCatch({
        if (missing(originGrouping)) {
          originDistribution <- private$Catalogs$OriginDistribution
          originGrouping <- GetOriginGroupingPreset(originGroupingType, originDistribution)
        }
        preProcessedData <- copy(private$Catalogs$PreProcessedData)
        ApplyOriginGrouping(preProcessedData, originGrouping)
        summary <- GetCaseDataSummary(preProcessedData)
      },
      error = function(e) {
        status <<- 'FAIL'
        msg <<- 'Applying grouping failed'
      })

      if (status == 'SUCCESS') {
        private$Catalogs$OriginGrouping <- originGrouping
        private$Catalogs$PreProcessedData <- preProcessedData
        private$Catalogs$Summary <- summary
        private$InvalidateAfterStep('CASE_BASED_ORIGIN_GROUPING')
        PrintAlert('Origin grouping {.val {originGroupingType}} has been applied')
      } else {
        PrintAlert('Origin grouping cannot be applied', type = 'danger')
      }

      private$SendMessage(
        'CASE_BASED_DATA_ORIGIN_GROUPING_APPLIED',
        payload = list(
          ActionStatus = status,
          ActionMessage = msg,
          Summary = summary
        )
      )

      return(invisible(self))
    },

    # 4. Adjust data -------------------------------------------------------------------------------
    RunAdjustments = function(
      adjustmentSpecs,
      filters = NULL
    ) {
      if (!is.element(
        private$AppMgr$Steps['CASE_BASED_ORIGIN_GROUPING'],
        private$AppMgr$CompletedSteps
      )) {
        PrintAlert(
          'Origing grouping must be applied before running adjustments',
          type = 'danger'
        )
        return(invisible(self))
      }

      tryCatch({
        PrintAlert('Starting adjustment task')

        private$Catalogs$Filters <- filters
        preProcessedData <- private$Catalogs$PreProcessedData

        if (isTRUE(filters$DiagYear$ApplyInAdjustments)) {
          diagYearRange <- c(
            filters$DiagYear$MinYear,
            filters$DiagYear$MaxYear
          )

          preProcessedData <- preProcessedData[
            is.na(YearOfHIVDiagnosis) |
            YearOfHIVDiagnosis %between% diagYearRange
          ]
        }

        if (isTRUE(filters$NotifQuarter$ApplyInAdjustments)) {
          notifQuarterRange <- c(
            filters$NotifQuarter$MinYear,
            filters$NotifQuarter$MaxYear
          )

          preProcessedData <- preProcessedData[
            is.na(NotificationTime) |
              NotificationTime %between% notifQuarterRange
          ]
        }

        if (nrow(preProcessedData)) {
          private$Catalogs$AdjustmentTask <- Task$new(
            function(data, adjustmentSpecs) {
              suppressMessages(pkgload::load_all())
              options(width = 120)

              result <- hivEstimatesAccuracy2::RunAdjustments(
                data = data,
                adjustmentSpecs = adjustmentSpecs,
                diagYearRange = NULL,
                notifQuarterRange = NULL,
                seed = NULL
              )
              return(result)
            },
            args = list(data = preProcessedData, adjustmentSpecs = adjustmentSpecs),
            session = private$Session,
            progressCallback = function(runLog) {
              private$SendMessage(
                'ADJUSTMENTS_RUN_LOG_SET',
                payload = list(
                  ActionStatus = 'SUCCESS',
                  RunLog = runLog
                )
              )
            },
            successCallback = function(result) {
              private$Catalogs$AdjustmentResult <- result
              private$Catalogs$AdjustedData <- copy(self$LastAdjustmentResult$Data)
              private$InvalidateAfterStep('CASE_BASED_ADJUSTMENTS')
              PrintAlert('Running adjustment task finished')
              private$SendMessage(
                'ADJUSTMENTS_RUN_FINISHED',
                payload = list(
                  ActionStatus = 'SUCCESS',
                  ActionMessage = 'Running adjustment task finished',
                  AdjustmentsReport = self$AdjustmentsReport
                )
              )
            },
            failCallback = function() {
              PrintAlert('Running adjustment task failed', type = 'danger')
              private$SendMessage(
                'ADJUSTMENTS_RUN_FINISHED',
                payload = list(
                  ActionStatus = 'FAIL',
                  ActionMessage = 'Running adjustment task failed'
                )
              )
            }
          )
          private$SendMessage(
            'ADJUSTMENTS_RUN_STARTED',
            payload = list(
              ActionStatus = 'SUCCESS',
              ActionMessage = 'Running adjustment task started'
            )
          )
        }
      },
      error = function(e) {
        private$SendMessage(
          'ADJUSTMENTS_RUN_STARTED',
          payload = list(
            ActionStatus = 'FAIL',
            ActionMessage = 'Running adjustment task failed'
          )
        )
        print(e)
      })

      return(invisible(self))
    },

    CancelAdjustments = function() {
      if (!is.null(private$Catalogs$AdjustmentTask)) {
        private$Catalogs$AdjustmentTask$Stop()

        private$SendMessage(
          'ADJUSTMENTS_RUN_CANCELLED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running adjustment task cancelled'
          )
        )
      }

      return(invisible(self))
    }
  ),

  private = list(
    # Shiny session
    Session = NULL,

    # Parent application manager
    AppMgr = NULL,

    # Storage
    Catalogs = NULL,

    SendMessage = function(...) {
      if (is.function(private$AppMgr$SendMessage)) {
        private$AppMgr$SendMessage(...)
      }
    },

    InvalidateAfterStep = function(step) {
      if (
        step %in% c('CASE_BASED_READ')
      ) {
        private$Catalogs$OriginDistribution <- NULL
        private$Catalogs$OriginGrouping <- list()
        private$Catalogs$PreProcessArtifacts <- NULL
        private$Catalogs$Summary <- NULL
        private$Catalogs$PreProcessedData <- NULL
        private$Catalogs$PreProcessedDataStatus <- NULL
        private$Catalogs$AdjustedData <- NULL
        private$Catalogs$AdjustmentTask <- NULL
        private$Catalogs$AdjustmentResult <- NULL
        if ('GroupedRegionOfOrigin' %in% colnames(private$Catalogs$PreProcessedData$Table)) {
          private$Catalogs$PreProcessedData$Table[, GroupedRegionOfOrigin := NULL]
        }
      }

      if (
        step %in% c('CASE_BASED_ATTR_MAPPING')
      ) {
        private$Catalogs$OriginGrouping <- list()
        private$Catalogs$Summary <- NULL
        private$Catalogs$AdjustedData <- NULL
        private$Catalogs$AdjustmentTask <- NULL
        private$Catalogs$AdjustmentResult <- NULL
        if ('GroupedRegionOfOrigin' %in% colnames(private$Catalogs$PreProcessedData$Table)) {
          private$Catalogs$PreProcessedData$Table[, GroupedRegionOfOrigin := NULL]
        }
      }

      if (
        step %in% c('CASE_BASED_ORIGIN_GROUPING')
      ) {
        private$Catalogs$AdjustedData <- NULL
        private$Catalogs$AdjustmentTask <- NULL
        private$Catalogs$AdjustmentResult <- NULL
      }

      private$AppMgr$SetCompletedStep(step)

      return(invisible(self))
    }
  ),

  active = list(
    FileName = function() {
      return(private$Catalogs$FileName)
    },

    OriginalData = function() {
      return(private$Catalogs$OriginalData)
    },

    AttrMapping = function() {
      return(private$Catalogs$AttrMapping)
    },

    AttrMappingStatus = function() {
      return(private$Catalogs$AttrMappingStatus)
    },

    OriginDistribution = function() {
      return(private$Catalogs$OriginDistribution)
    },

    OriginGrouping = function() {
      return(private$Catalogs$OriginGrouping)
    },

    PreProcessArtifacts = function() {
      return(private$Catalogs$PreProcessArtifacts)
    },

    Summary = function() {
      return(private$Catalogs$Summary)
    },

    SummaryJSON = function() {
      return(jsonlite::toJSON(private$Catalogs$Summary, keep_vec_names = TRUE))
    },

    Filters = function() {
      return(private$Catalogs$Filters)
    },

    PreProcessedData = function() {
      return(private$Catalogs$PreProcessedData)
    },

    PreProcessedDataStatus = function() {
      return(private$Catalogs$PreProcessedDataStatus)
    },

    AdjustedData = function() {
      return(private$Catalogs$AdjustedData)
    },

    AdjustmentTask = function() {
      return(private$Catalogs$AdjustmentTask)
    },

    AdjustmentResult = function() {
      return(private$Catalogs$AdjustmentResult)
    },

    LastAdjustmentResult = function() {
      if (
        is.list(private$Catalogs$AdjustmentResult) &&
        length(private$Catalogs$AdjustmentResult) > 0
      ) {
        result <- private$Catalogs$AdjustmentResult[[length(private$Catalogs$AdjustmentResult)]]
      } else {
        result <- NULL
      }

      return(result)
    },

    AdjustmentsReport = function() {
      report <- ''
      for (i in seq_along(private$Catalogs$AdjustmentResult)) {
        report <- paste(report, private$Catalogs$AdjustmentResult[[i]]$Report)
      }
      return(report)
    },

    Data = function() {
      if (!is.null(private$Catalogs$AdjustedData)) {
        data <- private$Catalogs$AdjustedData
      } else {
        data <- private$Catalogs$PreProcessedData
      }
      return(data)
    }
  )
)

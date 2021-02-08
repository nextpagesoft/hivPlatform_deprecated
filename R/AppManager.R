#' AppManager
#'
#' R6 class for representing the app manaager
#'
#' @name AppManager
#' @examples
#' pppManager <- AppManager$new()
NULL

#' @export
AppManager <- R6::R6Class(
  classname = 'AppManager',
  class = FALSE,
  cloneable = FALSE,
  public = list(

    # GENERIC METHOD ===============================================================================
    initialize = function(
      session = NULL
    ) {
      PrintAlert('Temporary directory: {.val {tempdir()}}')

      private$Session <- session

      private$CaseMgrPriv <- CaseDataManager$new(session, self)
      private$AggrMgrPriv <- AggrDataManager$new(session, self)
      private$HIVModelMgrPriv <- HIVModelManager$new(session, self)

      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        CompletedSteps = NULL,
        ReportTask = NULL,
        ReportArtifacts = NULL,
        Report = NULL
      )

      self$SetCompletedStep('SESSION_INITIALIZED')
    },

    print = function() {
      print(self$Session)
    },

    SendMessage = function(type, payload = list()) {
      if (missing(type)) {
        PrintAlert('Arguments {.arg type} must be provided', type = 'danger')
        return(invisible(NULL))
      }

      if (!is.null(private$Session)) {
        private$Session$sendCustomMessage('shinyHandler', list(
          type = type,
          payload = payload
        ))
      }
    },

    Steps = c(
      'SESSION_INITIALIZED' = 1L,
      'CASE_BASED_READ' = 2L,
      'CASE_BASED_ATTR_MAPPING' = 3L,
      'CASE_BASED_ORIGIN_GROUPING' = 4L,
      'CASE_BASED_SUMMARY' = 5L,
      'CASE_BASED_ADJUSTMENTS' = 6L,
      'BOOTSTRAP' = 7L,
      'REPORTS' = 8L,
      'OUTPUTS' = 9L,
      'AGGR_READ' = 10L,
      'MODELLING' = 11L
    ),

    SetCompletedStep = function(step) {
      completedSteps <- isolate(private$Catalogs$CompletedSteps)
      step <- self$Steps[step]
      keptSteps <- completedSteps[completedSteps < step]
      newCompletedSteps <- sort(union(keptSteps, step))
      private$Catalogs$CompletedSteps <- self$Steps[newCompletedSteps]

      if (!identical(completedSteps, newCompletedSteps)) {
        self$SendMessage(
          'COMPLETED_STEPS_SET',
          payload = list(
            ActionStatus = 'SUCCESS',
            CompletedSteps = isolate(names(private$Catalogs$CompletedSteps))
          )
        )
      }
    },

    SaveState = function() {
      PrintAlert('Saving state to file')
    },

    # USER ACTIONS =================================================================================

    CreateReport = function(
      reportSpec
    ) {
      if (!is.element(
        self$Steps['CASE_BASED_ADJUSTMENTS'],
        private$Catalogs$CompletedSteps
      )) {
        PrintAlert(
          'Adjustments must be run before creating a report',
          type = 'danger'
        )
        return(invisible(self))
      }

      tryCatch({
        PrintAlert('Starting report task')

        private$Catalogs$ReportTask <- Task$new(
          function(reportSpec, fileName, filters, adjustedData) {
            suppressMessages(pkgload::load_all())
            reportFilePath <- hivEstimatesAccuracy2::GetReportFileNames()[reportSpec$name]
            params <- modifyList(
              reportSpec,
              list(
                AdjustedData = adjustedData
              )
            )
            params <- hivEstimatesAccuracy2::GetMainReportArtifacts(params)
            params <- modifyList(
              params,
              list(
                Artifacts =
                  list(
                    FileName = fileName,
                    Filters = filters
                  )
              )
            )
            report <- hivEstimatesAccuracy2::RenderReportToHTML(reportFilePath, params)

            result <- list(
              Artifacts = params,
              Report = report
            )

            return(result)
          },
          args = list(
            reportSpec = reportSpec,
            fileName = private$CaseMgrPriv$FileName,
            filters = private$CaseMgrPriv$Filters,
            adjustedData = private$CaseMgrPriv$AdjustmentResult
          ),
          session = private$Session,
          successCallback = function(result) {
            private$Catalogs$Report <- result$Report
            private$Catalogs$ReportArtifacts <- result$Artifacts
            PrintAlert('Running report task finished')
            self$SendMessage(
              'CREATING_REPORT_FINISHED',
              payload = list(
                ActionStatus = 'SUCCESS',
                ActionMessage = 'Running report task finished',
                Report = result$Report
              )
            )
            self$SetCompletedStep('REPORTS')
          },
          failCallback = function() {
            PrintAlert('Running report task failed', type = 'danger')
            self$SendMessage(
              'CREATING_REPORT_FINISHED',
              payload = list(
                ActionStatus = 'FAIL',
                ActionMessage = 'Running report task failed'
              )
            )
          }
        )
        self$SendMessage(
          'CREATING_REPORT_STARTED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running report task started'
          )
        )
      },
      error = function(e) {
        self$SendMessage(
          'CREATING_REPORT_STARTED',
          payload = list(
            ActionStatus = 'FAIL',
            ActionMessage = 'Starting report task failed'
          )
        )
        print(e)
      })

      return(invisible(self))
    },

    CancelReport = function() {
      if (!is.null(private$Catalogs$ReportTask)) {
        private$Catalogs$ReportTask$Stop()

        self$SendMessage(
          'CREATING_REPORT_CANCELLED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running report task cancelled'
          )
        )
      }

      return(invisible(self))
    }
  ),

  private = list(
    # Shiny session
    Session = NULL,

    # Case-based data manager
    CaseMgrPriv = NULL,

    # Aggregated data manager
    AggrMgrPriv = NULL,

    # HIV Model manager
    HIVModelMgrPriv = NULL,

    # Storage
    Catalogs = NULL
  ),

  active = list(
    CaseMgr = function() {
      return(private$CaseMgrPriv)
    },

    AggrMgr = function() {
      return(private$AggrMgrPriv)
    },

    HIVModelMgr = function() {
      return(private$HIVModelMgrPriv)
    },

    CompletedSteps = function() {
      return(private$Catalogs$CompletedSteps)
    },

    ReportTask = function() {
      return(private$Catalogs$ReportTask)
    },

    ReportArtifacts = function() {
      return(private$Catalogs$ReportArtifacts)
    },

    Report = function() {
      return(private$Catalogs$Report)
    }

    # HIVModelParameters = function(xmlModel) {
    #   if (missing(xmlModel)) {
    #     return(private$Catalogs$HIVModelParameters)
    #   } else {
    #     private$Catalogs$HIVModelParameters <- ParseXMLModel(xmlModel)
    #   }
    # },
  )
)

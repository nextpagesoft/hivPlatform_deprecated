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
      'MODELLING' = 7L,
      'BOOTSTRAP' = 8L,
      'REPORTS' = 9L,
      'OUTPUTS' = 10L
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

    # USER ACTIONS =================================================================================

    CreateReport = function(
      reportName
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
          function(reportName, fileName, filters, adjustedData) {
            suppressMessages(pkgload::load_all())
            reportFilePath <- hivEstimatesAccuracy2::GetReportFileNames()[reportName]
            params <- list(
              AdjustedData = adjustedData,
              ReportingDelay = TRUE,
              Smoothing = TRUE,
              CD4ConfInt = FALSE
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

            return(report)
          },
          args = list(
            reportName = reportName,
            fileName = private$CaseMgrPriv$FileName,
            filters = private$CaseMgrPriv$Filters,
            adjustedData = private$CaseMgrPriv$AdjustmentResult
          ),
          session = private$Session,
          successCallback = function(report) {
            private$Catalogs$Report <- report
            PrintAlert('Running report task finished')
            self$SendMessage(
              'CREATING_REPORT_FINISHED',
              payload = list(
                ActionStatus = 'SUCCESS',
                ActionMessage = 'Running report task finished',
                Report = report
              )
            )
            self$SetCompletedStep('REPORTS')
          },
          failCallback = function() {
            PrintAlert('Running adjustment task failed', type = 'danger')
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
            ActionMessage = 'Running report task failed'
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

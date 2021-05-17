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
      private$UIStatePriv <- NULL

      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        Seed = NULL,
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
      'REPORTS' = 7L,
      'OUTPUTS' = 8L,
      'AGGR_READ' = 9L,
      'MODELLING' = 10L,
      'BOOTSTRAP' = 11L
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

    SetSeed = function(seed) {
      private$Catalogs$Seed <- seed
      set.seed(seed)
      if (is.null(seed)) {
        msg <- 'Random seed set to time-based (random) for all subsequent computations'
      } else {
        msg <- sprintf('Random seed set to %s for all subsequent computations', seed)
      }
      self$SendMessage(
        'SEED_SET',
        payload = list(
          ActionStatus = 'SUCCESS',
          ActionMessage = msg,
          Seed = seed
        )
      )
    },

    SetUIState = function(uiState) {
      private$UIStatePriv <- uiState
    },

    SaveState = function() {
      rc <- rawConnection(raw(0), 'r+')
      saveRDS(self, rc)
      rcData <- rawConnectionValue(rc)
      close(rc)
      self$SendMessage(
        'SAVE_STATE',
        payload = list(
          ActionStatus = 'SUCCESS',
          ActionMessage = 'Application state available for saving',
          Data = rcData,
          FileName = sprintf('HIVPlatformState_%s.rds', GetTimeStamp())
        )
      )
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
          function(reportSpec, fileName, filters, adjustedData, randomSeed) {
            suppressMessages(pkgload::load_all())
            .Random.seed <- randomSeed #nolint

            reportFilePath <- hivPlatform::GetReportFileNames()[reportSpec$name]
            params <- modifyList(
              reportSpec,
              list(
                AdjustedData = adjustedData
              )
            )
            params <- hivPlatform::GetMainReportArtifacts(params)
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
            report <- hivPlatform::RenderReportToHTML(reportFilePath, params)

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
            adjustedData = private$CaseMgrPriv$AdjustmentResult,
            randomSeed = .Random.seed
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
          failCallback = function(msg = NULL) {
            if (!is.null(msg)) {
              PrintAlert(msg, type = 'danger')
            }
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

    # UI state (JSON)
    UIStatePriv = NULL,

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
    UIState = function() {
      return(private$UIStatePriv)
    },

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

    Seed = function() {
      return(private$Catalogs$Seed)
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
  )
)

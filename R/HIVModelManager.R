#' HIVModelManager
#'
#' R6 class for representing the HIV Model manaager
#'
#' @name HIVModelManager
#' @examples
#' hivModelMgr <- HIVModelManager$new()
NULL

#' @export
HIVModelManager <- R6::R6Class(
  classname = 'HIVModelManager',
  class = FALSE,
  cloneable = FALSE,
  public = list(

    # GENERIC METHOD ===============================================================================
    initialize = function(
      session = NULL,
      appMgr = NULL
    ) {
      private$Session <- session
      private$AppMgr <- appMgr
      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        PopCombination = NULL,
        AggrDataSelection = NULL,
        MainFitTask = NULL,
        MainFitResult = NULL,
        BootstrapFitTask = NULL,
        BootstrapFitResult = NULL,
        BootstrapFitStats = NULL,
        LastStep = 0L
      )
    },

    print = function() {
      print(self$Session)
    },

    # USER ACTIONS =================================================================================

    RunMainFit = function(
      settings = list(),
      parameters = list(),
      popCombination = NULL,
      aggrDataSelection = NULL
    ) {
      if (private$Catalogs$LastStep < 0) {
        PrintAlert(
          'HIVModelManager is not initialized properly before combining data',
          type = 'danger'
        )
        return(invisible(self))
      }

      tryCatch({
        PrintAlert('Starting HIV Model main fit task')

        caseData <- private$AppMgr$CaseMgr$Data
        aggrData <- private$AppMgr$AggrMgr$Data
        dataSets <- CombineData(caseData, aggrData, popCombination, aggrDataSelection)

        private$Catalogs$MainFitTask <- Task$new(
          function(dataSets, settings, parameters) {
            options(width = 100)

            result <- list()
            for (imp in names(dataSets)) {
              context <- hivModelling::GetRunContext(
                data = dataSets[[imp]],
                settings = settings,
                parameters = parameters
              )
              popData <- hivModelling::GetPopulationData(context)

              startTime <- Sys.time()
              fitResults <- hivModelling::PerformMainFit(context, popData, attemptSimplify = TRUE)
              runTime <- Sys.time() - startTime

              result[[imp]] <- list(
                Context = context,
                PopData = popData,
                Results = fitResults,
                RunTime = runTime,
                Imputation = imp
              )
            }
            return(result)
          },
          args = list(
            dataSets = dataSets,
            settings = settings,
            parameters = parameters
          ),
          session = private$Session,
          successCallback = function(result) {
            private$Catalogs$PopCombination <- popCombination
            private$Catalogs$AggrDataSelection <- aggrDataSelection
            private$Catalogs$MainFitResult <- result
            private$Catalogs$LastStep <- 1L
            private$Reinitialize('RunMainFit')
            PrintAlert('Running HIV Model main fit task finished')
            private$SendMessage('MODELS_RUN_FINISHED', 'SUCCESS')
          },
          failCallback = function() {
            PrintAlert('Running HIV Model main fit task failed', type = 'danger')
            private$SendMessage('MODELS_RUN_FINISHED', 'FAIL')
          }
        )
        private$SendMessage('MODELS_RUN_STARTED', 'SUCCESS')
      },
      error = function(e) {
        private$SendMessage('MODELS_RUN_STARTED', 'FAIL')
        print(e)
      })

      return(invisible(self))
    },

    CancelMainFit = function() {
      private$Catalogs$MainFitTask$Stop()
      return(invisible(self))
    },

    RunBootstrapFit = function(
      bsCount = 0,
      bsType = 'PARAMETRIC',
      maxRunTimeFactor = 3
    ) {
      if (private$Catalogs$LastStep < 1) {
        PrintAlert('Main fit must be performed before running bootstrap', type = 'danger')
        return(invisible(self))
      }

      tryCatch({
        avgRunTime <- mean(sapply(private$Catalogs$MainFitResult, '[[', 'RunTime'))
        maxRunTime <- as.difftime(avgRunTime * maxRunTimeFactor, units = 'secs')

        PrintAlert('Starting HIV Model bootstrap fit task')
        PrintAlert('Maximum allowed run time: {.timestamp {prettyunits::pretty_dt(maxRunTime)}}')

        private$Catalogs$BootstrapFitTask <- Task$new(
          function(
            bsCount,
            bsType,
            maxRunTime,
            mainFitResult,
            caseData,
            aggrData,
            popCombination,
            aggrDataSelection
          ) {
            suppressMessages(pkgload::load_all())
            options(width = 100)
            mainCount <- length(mainFitResult)
            fits <- list()
            i <- 0
            for (imp in names(mainFitResult)) {
              i <- i + 1
              mainFit <- mainFitResult[[imp]]
              context <- mainFit$Context
              param <- mainFit$Results$Param
              info <- mainFit$Results$Info

              context$Settings <- modifyList(
                context$Settings,
                list(
                  ModelFilePath = NULL,
                  InputDataPath = NULL,
                  Verbose = FALSE
                ),
                keep.null = TRUE
              )

              PrintH2('Main data set {.val {imp}}')

              if (bsType == 'NON-PARAMETRIC' & !is.null(caseData)) {
                caseDataImp <- caseData[Imputation == as.integer(imp)]
              } else {
                caseDataImp <- NULL
              }

              jSucc <- 1
              j <- 0
              bootResults <- list()
              while (jSucc <= bsCount) {
                j <- j + 1

                # Bootstrap data set
                if (bsType == 'NON-PARAMETRIC' & !is.null(caseData)) {
                  bootCaseDataImp <- caseDataImp[sample.int(nrow(caseDataImp), replace = TRUE)]
                } else {
                  bootCaseDataImp <- NULL
                }
                bootData <-
                  CombineData(bootCaseDataImp, aggrData, popCombination, aggrDataSelection)[[1]]

                bootContext <- hivModelling::GetRunContext(
                  data = bootData,
                  parameters = context$Parameters,
                  settings = context$Settings
                )

                bootPopData <- hivModelling::GetPopulationData(bootContext)

                startTime <- Sys.time()
                switch(
                  bsType,
                  'PARAMETRIC' = {
                    bootResult <- hivModelling::PerformBootstrapFit(
                      j, bootContext, bootPopData, mainFit$Results
                    )
                  },
                  'NON-PARAMETRIC' = {
                    capture.output({
                      bootResult <- hivModelling::PerformMainFit(
                        bootContext, bootPopData,
                        param = param, info = info, attemptSimplify = FALSE,
                        maxRunTime = maxRunTime, verbose = FALSE
                      )
                    })
                  }
                )
                runTime <- Sys.time() - startTime

                msgType <- ifelse(bootResult$Converged, 'success', 'danger')

                PrintAlert(
                  'Iteration {.val {jSucc}} done |',
                  'Run time: {.timestamp {prettyunits::pretty_dt(runTime)}}',
                  type = msgType
                )

                if (bootResult$Converged) {
                  jSucc <- jSucc + 1
                  progress <- (jSucc - 1 + (i - 1) * bsCount) / (mainCount * bsCount) * 100
                }

                bootResults[[j]] <- list(
                  Context = bootContext,
                  Data = bootData,
                  Results = bootResult,
                  RunTime = runTime
                )
              }

              fits[[imp]] <- bootResults
            }

            stats <- GetBootstrapFitStats(fits)

            result <- list(
              Fits = fits,
              Stats = stats
            )

            return(result)
          },
          args = list(
            bsCount = bsCount,
            bsType = bsType,
            maxRunTime = maxRunTime,
            mainFitResult = private$Catalogs$MainFitResult,
            caseData = private$AppMgr$CaseMgr$Data,
            aggrData = private$AppMgr$AggrMgr$Data,
            popCombination = private$Catalogs$PopCombination,
            aggrDataSelection = private$Catalogs$AggrDataSelection
          ),
          session = private$Session,
          successCallback = function(result) {
            private$Catalogs$BootstrapFitResult <- result$Fits
            private$Catalogs$BootstrapFitStats <- result$Stats
            private$Catalogs$LastStep <- 2L
            private$Reinitialize('RunBootstrapFit')
            PrintAlert('Running HIV Model bootstrap fit task finished')
            private$SendMessage('BOOTSTRAP_RUN_FINISHED', 'SUCCESS')
          },
          failCallback = function() {
            PrintAlert('Running HIV Model bootstrap fit task failed', type = 'danger')
            private$SendMessage('BOOTSTRAP_RUN_FINISHED', 'FAIL')
          }
        )
        private$SendMessage('BOOTSTRAP_RUN_STARTED', 'SUCCESS')
      },
      error = function(e) {
        private$SendMessage('BOOTSTRAP_RUN_STARTED', 'FAIL')
        print(e)
      })

      return(invisible(self))
    },

    CancelBootstrapFit = function() {
      private$Catalogs$BootstrapFitTask$Stop()
      return(invisible(self))
    }
  ),

  private = list(
    # Shiny session
    Session = NULL,

    # Private application manager
    AppMgr = NULL,

    # Storage
    Catalogs = NULL,

    SendMessage = function(...) {
      if (is.function(private$AppMgr$SendMessage)) {
        private$AppMgr$SendMessage(...)
      }
    },

    Reinitialize = function(step) {
      if (step %in% c('RunMainFit')) {
        private$Catalogs$BootstrapFitTask <- NULL
        private$Catalogs$BootstrapFitResult <- NULL
        private$Catalogs$BootstrapFitStats <- NULL
      }

      return(invisible(self))
    }
  ),

  active = list(
    Data = function() {
      return(private$Catalogs$Data)
    },

    LastStep = function() {
      return(private$Catalogs$LastStep)
    },

    PopCombination = function() {
      return(private$Catalogs$PopCombination)
    },

    AggrDataSelection = function() {
      return(private$Catalogs$AggrDataSelection)
    },

    MainFitTask = function() {
      return(private$Catalogs$MainFitTask)
    },

    MainFitResult = function() {
      return(private$Catalogs$MainFitResult)
    },

    BootstrapFitTask = function() {
      return(private$Catalogs$BootstrapFitTask)
    },

    BootstrapFitResult = function() {
      return(private$Catalogs$BootstrapFitResult)
    },

    BootstrapFitStats = function() {
      return(private$Catalogs$BootstrapFitStats)
    }
  )
)

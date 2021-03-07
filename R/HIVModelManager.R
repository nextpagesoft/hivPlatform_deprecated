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
      private$AppMgr <- appMgr
      private$Session <- session
      catalogStorage <- ifelse(!is.null(session), shiny::reactiveValues, list)
      private$Catalogs <- catalogStorage(
        Parameters = NULL,
        PopCombination = NULL,
        AggrDataSelection = NULL,
        MainFitTask = NULL,
        MainFitResult = NULL,
        BootstrapFitTask = NULL,
        BootstrapFitResult = NULL,
        BootstrapFitStats = NULL,
        PlotData = NULL
      )
    },

    print = function() {
      print(self$Session)
    },

    # USER ACTIONS =================================================================================

    SetParameters = function(
      xmlModel
    ) {
      status <- 'SUCCESS'
      msg <- 'Parameters read correctly'
      tryCatch({
        params <- ParseXMLModel(xmlModel)
      }, error = function(e) {
        status <<- 'FAIL'
        msg <<-
          paste(
            'There was a difficulty encountered when reading the parameters file.',
            'They have not been loaded.'
          )
      })

      if (status == 'SUCCESS') {
        private$Catalogs$Parameters <- params
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          Params = params
        )
      } else {
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      private$SendMessage('MODELS_PARAMS_SET', payload)
    },

    RunMainFit = function(
      settings = list(),
      parameters = list(),
      popCombination = NULL,
      aggrDataSelection = NULL
    ) {
      if (!any(is.element(
        private$AppMgr$Steps[c('CASE_BASED_READ', 'AGGR_READ')],
        private$AppMgr$CompletedSteps
      ))) {
        PrintAlert(
          'Data must be read before running main fit of the HIV model',
          type = 'danger'
        )
        return(invisible(self))
      }

      tryCatch({
        PrintAlert('Starting HIV Model main fit task')

        private$Catalogs$MainFitTask <- Task$new(
          function(
            caseData,
            aggrData,
            settings,
            parameters,
            popCombination,
            aggrDataSelection
          ) {
            suppressMessages(pkgload::load_all())
            options(width = 120)

            PrintH1('Parameters')

            if (length(popCombination$Aggr) > 0) {
              PrintH2('Aggregated data selection')
              cli::cli_text(
                '{.val {cat(capture.output(aggrDataSelection), sep = "\n")}}'
              )
            }

            PrintH2('Population combination')
            if (!is.null(caseData)) {
              if (length(popCombination$CaseAbbr) > 0) {
                PrintAlert('Case-based populations: {.val {popCombination$CaseAbbr}}')
              } else {
                PrintAlert('Case-based populations: {.val \'All data available\'}')
              }
            } else {
              PrintAlert('Case-based populations: {.val \'None\'}')
            }

            if (!is.null(aggrData) && length(popCombination$Aggr) > 0) {
              PrintAlert('Aggregated populations: {.val {popCombination$Aggr}}')
            } else {
              PrintAlert('Aggregated populations: {.val \'None\'}')
            }

            PrintH2('Time intervals and diagnosis rates modelling')
            cli::cli_text(
              '{.val {cat(capture.output(parameters$Intervals), sep = "\n")}}'
            )

            PrintH2('Advanced paramaters')
            PrintAlert(' 1. Range of calculations                                : {.val {parameters$ModelMinYear} - {parameters$ModelMaxYear}}')
            PrintAlert(' 2. HIV diagnoses, total                                 : {.val {parameters$FitPosMinYear} - {parameters$FitPosMaxYear}}')
            PrintAlert(' 3. HIV diagnoses, by CD4 count                          : {.val {parameters$FitPosCD4MinYear} - {parameters$FitPosCD4MaxYear}}')
            PrintAlert(' 4. AIDS diagnoses, total                                : {.val {parameters$FitAIDSMinYear} - {parameters$FitAIDSMaxYear}}')
            PrintAlert(' 5. HIV/AIDS diagnoses, total                            : {.val {parameters$FitAIDSPosMinYear} - {parameters$FitAIDSPosMaxYear}}')
            PrintAlert(' 6. Do you have data from the start of the epidemic      : {.val {ifelse(parameters$FullData, "Yes", "No")}}')
            PrintAlert(' 7. Knots count                                          : {.val {parameters$ModelNoKnots}}')
            PrintAlert(' 8. Start at zero                                        : {.val {ifelse(parameters$StartIncZero, "Yes", "No")}}')
            PrintAlert(' 9. Prevent sudden changes at end of observation interval: {.val {ifelse(parameters$MaxIncCorr, "Yes", "No")}}')
            PrintAlert('10. Maximum likelihood distribution                      : {.val {parameters$FitDistribution}}')
            PrintAlert('11. Extra diagnosis rate due to non-AIDS symptoms        : {.val {parameters$Delta4Fac}}')
            PrintAlert('12. Country-specific settings                            : {.val {parameters$Country}}')

            dataSets <- CombineData(caseData, aggrData, popCombination, aggrDataSelection)

            PrintH1('Performing main fit')

            impResult <- list()
            for (imp in names(dataSets)) {
              PrintH2('Iteration {.val {imp}}')
              context <- hivModelling::GetRunContext(
                data = dataSets[[imp]],
                settings = settings,
                parameters = list(
                  INCIDENCE = parameters
                )
              )
              popData <- hivModelling::GetPopulationData(context)

              startTime <- Sys.time()
              fitResults <- hivModelling::PerformMainFit(
                context,
                popData,
                attemptSimplify = TRUE,
                verbose = TRUE
              )
              runTime <- Sys.time() - startTime

              impResult[[imp]] <- list(
                Context = context,
                PopData = popData,
                Results = fitResults,
                RunTime = runTime,
                Imputation = imp
              )
            }

            plotData <- GetHIVPlotData(impResult, NULL)

            result <- list(
              MainFitResult = impResult,
              PlotData = plotData
            )

            return(result)
          },
          args = list(
            caseData = private$AppMgr$CaseMgr$Data,
            aggrData = private$AppMgr$AggrMgr$Data,
            settings = settings,
            parameters = parameters,
            popCombination = popCombination,
            aggrDataSelection = aggrDataSelection
          ),
          session = private$Session,
          successCallback = function(result) {
            private$Catalogs$PopCombination <- popCombination
            private$Catalogs$AggrDataSelection <- aggrDataSelection
            private$Catalogs$MainFitResult <- result$MainFitResult
            private$Catalogs$PlotData <- result$PlotData
            private$InvalidateAfterStep('MODELLING')
            PrintAlert('Running HIV Model main fit task finished')
            private$SendMessage(
              'MODELS_RUN_FINISHED',
              payload = list(
                ActionStatus = 'SUCCESS',
                ActionMessage = 'Running HIV Model main fit task finished',
                PlotData = result$PlotData
              )
            )
          },
          failCallback = function(msg = NULL) {
            if (!is.null(msg)) {
              PrintAlert(msg, type = 'danger')
            }
            PrintAlert('Running HIV Model main fit task failed', type = 'danger')
            private$SendMessage(
              'MODELS_RUN_FINISHED',
              payload = list(
                ActionStatus = 'FAIL',
                ActionMessage = 'Running HIV Model main fit task failed'
              )
            )
          }
        )

        private$SendMessage(
          'MODELS_RUN_STARTED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model main fit task started'
          )
        )
      },
      error = function(e) {
        private$SendMessage(
          'MODELS_RUN_STARTED',
          payload = list(
            ActionStatus = 'FAIL',
            ActionMessage = 'Running HIV Model main fit task failed'
          )
        )
        print(e)
      })

      return(invisible(self))
    },

    CancelMainFit = function() {
      if (!is.null(private$Catalogs$MainFitTask)) {
        private$Catalogs$MainFitTask$Stop()
        private$SendMessage(
          'MODELS_RUN_CANCELLED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model main fit task cancelled'
          )
        )
      }

      return(invisible(self))
    },

    RunBootstrapFit = function(
      bsCount = 0,
      bsType = 'PARAMETRIC',
      maxRunTimeFactor = 3
    ) {
      if (!is.element(
        private$AppMgr$Steps['MODELLING'],
        private$AppMgr$CompletedSteps
      )) {
        PrintAlert(
          'Main fit must be performed before running bootstrap',
          type = 'danger'
        )
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
            options(width = 120)

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

            plotData <- GetHIVPlotData(mainFitResult, stats)

            result <- list(
              Fits = fits,
              Stats = stats,
              PlotData = plotData
            )

            return(result)
          },
          args = list(
            bsCount = bsCount,
            bsType = bsType,
            maxRunTime = maxRunTime,
            mainFitResult = isolate(private$Catalogs$MainFitResult),
            caseData = isolate(private$AppMgr$CaseMgr$Data),
            aggrData = isolate(private$AppMgr$AggrMgr$Data),
            popCombination = isolate(private$Catalogs$PopCombination),
            aggrDataSelection = isolate(private$Catalogs$AggrDataSelection)
          ),
          session = private$Session,
          successCallback = function(result) {
            PrintAlert('Running HIV Model bootstrap fit task finished')

            private$Catalogs$BootstrapFitResult <- result$Fits
            private$Catalogs$BootstrapFitStats <- result$Stats
            private$Catalogs$PlotData <- result$PlotData
            private$InvalidateAfterStep('BOOTSTRAP')

            private$SendMessage(
              'BOOTSTRAP_RUN_FINISHED',
              payload = list(
                ActionStatus = 'SUCCESS',
                ActionMessage = 'Running HIV Model bootstrap fit task finished',
                PlotData = result$PlotData
              )
            )
          },
          failCallback = function(msg = NULL) {
            PrintAlert('Running HIV Model bootstrap fit task failed', type = 'danger')
            if (!is.null(msg)) {
              PrintAlert(msg, type = 'danger')
            }
            private$Catalogs$BootstrapFitTask$Stop(force = TRUE)
            private$SendMessage(
              'BOOTSTRAP_RUN_FINISHED',
              payload = list(
                ActionStatus = 'FAIL',
                ActionMessage = 'Running HIV Model bootstrap fit task failed'
              )
            )
          }
        )

        private$SendMessage(
          'BOOTSTRAP_RUN_STARTED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model bootstrap fit task started'
          )
        )
      },
      error = function(e) {
        private$SendMessage(
          'BOOTSTRAP_RUN_STARTED',
          payload = list(
            ActionStatus = 'FAIL',
            ActionMessage = 'Running HIV Model bootstrap fit task failed'
          )
        )
        print(e)
      })

      return(invisible(self))
    },

    CancelBootstrapFit = function() {
      if (!is.null(private$Catalogs$BootstrapFitTask)) {
        private$Catalogs$BootstrapFitTask$Stop()
        private$SendMessage(
          'BOOTSTRAP_RUN_CANCELLED',
          payload = list(
            ActionStatus = 'SUCCESS',
            ActionMessage = 'Running HIV Model bootstrap fit task cancelled'
          )
        )
      }
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

    InvalidateAfterStep = function(step) {
      if (
        step %in% c('MODELLING')
      ) {
        private$Catalogs$BootstrapFitTask <- NULL
        private$Catalogs$BootstrapFitResult <- NULL
        private$Catalogs$BootstrapFitStats <- NULL
      }

      private$AppMgr$SetCompletedStep(step)

      return(invisible(self))
    }
  ),

  active = list(
    Parameters = function(xmlModel) {
      return(private$Catalogs$Parameters)
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
    },

    PlotData = function() {
      return(private$Catalogs$PlotData)
    }
  )
)

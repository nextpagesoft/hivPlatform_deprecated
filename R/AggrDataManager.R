#' AggrDataManager
#'
#' R6 class for representing the aggregated data manaager
#'
#' @name AggrDataManager
#' @examples
#' caseMgr <- AggrDataManager$new()
NULL

#' @export
AggrDataManager <- R6::R6Class(
  classname = 'AggrDataManager',
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
        FileName = NULL,
        Data = NULL
      )
    },

    print = function() {
      print('AggrDataManager')
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
        data <- hivModelling::ReadInputData(fileName)
        dataNames <- names(data)
        dataFiles <- lapply(
          dataNames,
          function(dataName) {
            dt <- data[[dataName]]
            list(
              name = dataName,
              use = TRUE,
              years = c(min(dt$Year), max(dt$Year))
            )
          }
        )
        populationNames <- names(data[[1]])[-1]
      },
      error = function(e) {
        status <<- 'FAIL'
        msg <<-
          'There was a difficulty encountered when reading the data file. It has not been loaded.'
      })

      if (status == 'SUCCESS') {
        private$Catalogs$FileName <- fileName
        private$Catalogs$Data <- data
        private$AppMgr$SetCompletedStep('AGGR_READ')
        PrintAlert('Data file {.file {fileName}} loaded')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg,
          DataFiles = dataFiles,
          PopulationNames = names(data[[1]])[-1]
        )
      } else {
        PrintAlert('Loading data file {.file {fileName}} failed', type = 'danger')
        payload <- list(
          ActionStatus = status,
          ActionMessage = msg
        )
      }

      private$SendMessage('AGGR_DATA_READ', payload)

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
    }
  ),

  active = list(
    FileName = function() {
      return(private$Catalogs$FileName)
    },

    Data = function() {
      return(private$Catalogs$Data)
    }
  )
)

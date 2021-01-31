CreateDownload <- function(type, format, output, appMgr) {
  if (type == 'ADJUSTED_DATA') {
    timeStamp <- appMgr$CaseMgr$LastAdjustmentResult$TimeStamp
    if (format %in% c('rds')) {
      data <- appMgr$CaseMgr$LastAdjustmentResult
    } else {
      data <- appMgr$CaseMgr$LastAdjustmentResult$Data
    }
    fileNamePrefix <- 'AdjustedData'
    outputControlName <- sprintf('downAdjData%s', toupper(format))
  } else if (type == 'MAIN_REPORT') {
    timeStamp <- GetTimeStamp()
    data <- appMgr$ReportArtifacts
    fileNamePrefix <- 'AdjustmentsReport'
    outputControlName <- sprintf('report%s', toupper(format))
  }

  output[[outputControlName]] <- downloadHandler(
    filename = function() {
      if (type == 'ADJUSTED_DATE') {
        sprintf('%s_%s.%s', fileNamePrefix, timeStamp, format)
      } else {
        sprintf('%s_%s.%s', fileNamePrefix, timeStamp, switch(
          format,
          'html' = 'html',
          'pdf' = 'pdf',
          'latex' = 'zip',
          'word' = 'docx'
        ))
      }

    },
    content = function(file) {
      if (type == 'ADJUSTED_DATA') {
        WriteDataFile(data, file)
      } else {
        RenderReportToFile(
          reportFilePath = GetReportFileNames()['Main Report'],
          format = sprintf('%s_document', format),
          params = data,
          outputFilePath = file
        )
      }
    }
  )
}

Events <- function(
  input,
  output,
  session,
  appMgr
) {
  # Case-based data upload event
  observeEvent(input$caseUploadBtn, {
    fileInfo <- input$caseUploadBtn
    appMgr$SendMessage(
      'CASE_BASED_DATA_UPLOADED',
      list(
        ActionStatus = 'SUCCESS',
        ActionMessage = 'Data has been uploaded successfully',
        FileName = fileInfo$name[1],
        FileSize = fileInfo$size[1],
        FileType = fileInfo$type[1],
        FilePath = fileInfo$datapath[1]
      )
    )
    appMgr$CaseMgr$ReadData(fileInfo$datapath)
  })

  observeEvent(input$attrMapping, {
    appMgr$CaseMgr$ApplyAttributesMapping(input$attrMapping)
  })

  observeEvent(input$groupingPresetSelect, {
    type <- input$groupingPresetSelect
    distr <- appMgr$CaseMgr$OriginDistribution
    groups <- GetOriginGroupingPreset(type, distr)

    appMgr$SendMessage(
      type = 'CASE_BASED_DATA_ORIGIN_GROUPING_PREPARED',
      payload = list(
        ActionStatus = 'SUCCESS',
        ActionMessage = 'Origin grouping has been prepared',
        OriginGroupingType = type,
        OriginGrouping = groups
      )
    )
  })

  observeEvent(input$originGrouping, {
    appMgr$CaseMgr$ApplyOriginGrouping(input$originGrouping)
  })

  observeEvent(input$summaryFilters, {
    diagYearRange <- c(
      input$summaryFilters$DiagYear$MinYear,
      input$summaryFilters$DiagYear$MaxYear
    )

    notifQuarterRange <- c(
      input$summaryFilters$NotifQuarter$MinYear,
      input$summaryFilters$NotifQuarter$MaxYear
    )

    if (
      any(is.null(diagYearRange)) ||
      any(is.null(notifQuarterRange)) ||
      is.null(appMgr$CaseMgr$PreProcessedData)
    ) {
      return(NULL)
    }

    data <- appMgr$CaseMgr$PreProcessedData[
      is.na(YearOfHIVDiagnosis) |
      is.na(NotificationTime) |
      (
        YearOfHIVDiagnosis %between% diagYearRange &
        NotificationTime %between% notifQuarterRange
      )
    ]

    missPlotData <- GetMissingnessPlots(data)
    repDelPlotData <- GetReportingDelaysPlots(data)

    summary <- list(
      SelectedCount = nrow(data),
      TotalCount = nrow(appMgr$CaseMgr$PreProcessedData),
      MissPlotData = missPlotData,
      RepDelPlotData = repDelPlotData
    )

    appMgr$SendMessage(
      type = 'CASE_BASED_SUMMARY_DATA_PREPARED',
      payload = list(
        ActionStatus = 'SUCCESS',
        ActionMessage = 'Summary has been prepared',
        Summary = summary
      )
    )

    appMgr$SetCompletedStep('CASE_BASED_SUMMARY')
  }, ignoreInit = TRUE)

  observeEvent(input$runAdjustBtn, {
    params <- input$runAdjustBtn
    adjustmentSpecs <- GetAdjustmentSpecsWithParams(params)
    appMgr$CaseMgr$RunAdjustments(adjustmentSpecs, params$Filter)
  })

  observeEvent(input$cancelAdjustBtn, {
    appMgr$CaseMgr$CancelAdjustments()
  })

  observeEvent(appMgr$CaseMgr$AdjustmentTask$HTMLRunLog, {
    appMgr$SendMessage(
      'ADJUSTMENTS_RUN_LOG_SET',
      payload = list(
        ActionStatus = 'SUCCESS',
        RunLog = appMgr$CaseMgr$AdjustmentTask$HTMLRunLog
      )
    )
  })

  observeEvent(appMgr$CaseMgr$LastAdjustmentResult, {
    CreateDownload('ADJUSTED_DATA', 'csv', output, appMgr)
    CreateDownload('ADJUSTED_DATA', 'rds', output, appMgr)
    CreateDownload('ADJUSTED_DATA', 'dta', output, appMgr)
  })

  observeEvent(input$createReportBtn, {
    reportSpec <- input$createReportBtn
    appMgr$CreateReport(reportSpec)
  })

  observeEvent(input$cancelCreatingReportBtn, {
    appMgr$CancelReport()
  })

  observeEvent(appMgr$ReportArtifacts, {
    CreateDownload('MAIN_REPORT', 'html', output, appMgr)
    CreateDownload('MAIN_REPORT', 'pdf', output, appMgr)
    CreateDownload('MAIN_REPORT', 'latex', output, appMgr)
    CreateDownload('MAIN_REPORT', 'word', output, appMgr)
  })

  # observeEvent(input$aggrUploadBtn, {
  #   fileInfo <- input$aggrUploadBtn
  #   appMgr$SendMessage(
  #     'AGGR_DATA_UPLOADED',
  #     'SUCCESS',
  #     list(
  #       FileName = fileInfo$name[1],
  #       FileSize = fileInfo$size[1],
  #       FileType = fileInfo$type[1],
  #       FilePath = fileInfo$datapath[1]
  #     )
  #   )
  #   appMgr$AggrMgr$ReadData(fileInfo$datapath)
  # })


  # observeEvent(appMgr$FinalAdjustedCaseBasedData$Table, {
  #   dt <- appMgr$FinalAdjustedCaseBasedData$Table
  #   if (!is.null(dt)) {
  #     availableStrata <- GetAvailableStrata(appMgr$FinalAdjustedCaseBasedData$Table)
  #   } else {
  #     availableStrata <- NULL
  #   }

  #   appMgr$SendEventToReact('shinyHandler', list(
  #     Type = 'AVAILABLE_STRATA_SET',
  #     Status = 'SUCCESS',
  #     Payload = list(
  #       AvailableStrata = availableStrata
  #     )
  #   ))
  # }, ignoreNULL = FALSE)

  # observeEvent(input$xmlModel, {
  #   appMgr$HIVModelParameters <- input$xmlModel
  # })

  # observeEvent(appMgr$HIVModelParameters, {
  #   appMgr$SendEventToReact('shinyHandler', list(
  #     Type = 'MODELS_PARAMS_SET',
  #     Status = 'SUCCESS',
  #     Payload = list(
  #       Params = appMgr$HIVModelParameters
  #     )
  #   ))
  # })

  # observeEvent(input$runModelBtn, {
  #   params <- input$runModelBtn
  #   print(params)
  #   appMgr$FitHIVModel(
  #     settings = list(Verbose = TRUE),
  #     parameters = params
  #   )
  # })

  # observeEvent(appMgr$HIVModelTask$HTMLRunLog, {
  #   appMgr$SendEventToReact('shinyHandler', list(
  #     Type = 'MODELS_RUN_LOG_SET',
  #     Status = 'SUCCESS',
  #     Payload = list(
  #       RunLog = appMgr$HIVModelTask$HTMLRunLog
  #     )
  #   ))
  # })

  # observeEvent(appMgr$HIVModelTask$Status, {
  #   if (appMgr$HIVModelTask$Status == 'RUNNING') {
  #     appMgr$SendEventToReact('shinyHandler', list(
  #       Type = 'MODELS_RUN_STARTED',
  #       Status = 'SUCCESS',
  #       Payload = list()
  #     ))
  #   } else if (appMgr$HIVModelTask$Status == 'STOPPED') {
  #     appMgr$HIVModelResults <- appMgr$HIVModelTask$Result
  #     appMgr$SendEventToReact('shinyHandler', list(
  #       Type = 'MODELS_RUN_FINISHED',
  #       Status = 'SUCCESS',
  #       Payload = list()
  #     ))
  #   }
  # })

  # observeEvent(input$cancelModelBtn, {
  #   appMgr$CancelHIVModelFit()
  # })

  # observeEvent(input$runBootstrapBtn, {
  #   params <- input$runBootstrapBtn
  #   print(params)
  #   appMgr$RunBootstrap(
  #     bsCount = params$count,
  #     type = params$type
  #   )
  # })

  # observeEvent(appMgr$BootstrapTask$HTMLRunLog, {
  #   appMgr$SendEventToReact('shinyHandler', list(
  #     Type = 'BOOTSTRAP_RUN_LOG_SET',
  #     Status = 'SUCCESS',
  #     Payload = list(
  #       RunLog = appMgr$BootstrapTask$HTMLRunLog
  #     )
  #   ))
  # })

  # observeEvent(appMgr$BootstrapTask$Status, {
  #   if (appMgr$BootstrapTask$Status == 'RUNNING') {
  #     appMgr$SendEventToReact('shinyHandler', list(
  #       Type = 'BOOTSTRAP_RUN_STARTED',
  #       Status = 'SUCCESS',
  #       Payload = list()
  #     ))
  #   } else if (appMgr$BootstrapTask$Status == 'STOPPED') {
  #     appMgr$HIVBootstrapModelResults <- appMgr$BootstrapTask$Result
  #     appMgr$SendEventToReact('shinyHandler', list(
  #       Type = 'BOOTSTRAP_RUN_FINISHED',
  #       Status = 'SUCCESS',
  #       Payload = list()
  #     ))
  #   }
  # })

  # observeEvent(input$cancelBootstrapBtn, {
  #   appMgr$CancelBootstrapTask()
  # })

  # observeEvent(input$generateReportBtn, {
  #   appMgr$GenerateReport()
  # })

  # observeEvent(appMgr$ReportTask$Status, {
  #   if (appMgr$ReportTask$Status == 'RUNNING') {
  #     appMgr$SendEventToReact('shinyHandler', list(
  #       Type = 'GENERATING_REPORT_STARTED',
  #       Status = 'SUCCESS',
  #       Payload = list()
  #     ))
  #   } else if (appMgr$ReportTask$Status == 'STOPPED') {
  #     appMgr$Report <- appMgr$ReportTask$Result
  #     appMgr$SendEventToReact('shinyHandler', list(
  #       Type = 'REPORT_SET',
  #       Status = 'SUCCESS',
  #       Payload = list(
  #         Report = appMgr$Report
  #       )
  #     ))
  #   }
  # })
}

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

  observeEvent(input$saveStateBtn, {
    appMgr$SaveState()
  })

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

  observeEvent(input$aggrUploadBtn, {
    fileInfo <- input$aggrUploadBtn
    appMgr$SendMessage(
      'AGGR_DATA_UPLOADED',
      list(
        ActionStatus = 'SUCCESS',
        ActionMessage = 'Data has been uploaded successfully',
        FileName = fileInfo$name[1],
        FileSize = fileInfo$size[1],
        FileType = fileInfo$type[1],
        FilePath = fileInfo$datapath[1]
      )
    )
    appMgr$AggrMgr$ReadData(fileInfo$datapath)
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

  observeEvent(appMgr$CaseMgr$Data, {
    result <- GetAvailableStrata(appMgr$CaseMgr$Data)
    variables <- lapply(names(result$Variables), function(varName) {
      list(
        Name = varName,
        Code = unname(result$Variables[[varName]])
      )
    })

    appMgr$SendMessage(
      'AVAILABLE_STRATA_SET',
      payload = list(
        ActionStatus = 'SUCCESS',
        AvailableVariables = variables,
        AvailableStrata = jsonlite::toJSON(result$Strata)
      )
    )
  })

  observeEvent(input$xmlModel, {
    appMgr$HIVModelMgr$SetParameters(input$xmlModel)
  })

  observeEvent(input$runModelBtn, {
    runSettings <- input$runModelBtn
    params <- runSettings$Params
    popCombination <- runSettings$PopCombination
    aggrDataSelection <- runSettings$AggrDataSelection

    appMgr$HIVModelMgr$RunMainFit(
      settings = list(Verbose = FALSE),
      parameters = params,
      popCombination = popCombination,
      aggrDataSelection = aggrDataSelection
    )
  })

  observeEvent(input$cancelModelBtn, {
    appMgr$HIVModelMgr$CancelMainFit()
  })

  observeEvent(appMgr$HIVModelMgr$MainFitTask$HTMLRunLog, {
    appMgr$SendMessage(
      'MODELS_RUN_LOG_SET',
      payload = list(
        ActionStatus = 'SUCCESS',
        RunLog = appMgr$HIVModelMgr$MainFitTask$HTMLRunLog
      )
    )
  })

  observeEvent(input$runBootstrapBtn, {
    params <- input$runBootstrapBtn
    appMgr$HIVModelMgr$RunBootstrapFit(
      bsCount = as.integer(params$count),
      bsType = params$type
    )
  })

  observeEvent(appMgr$HIVModelMgr$BootstrapFitTask$HTMLRunLog, {
    appMgr$SendMessage(
      'BOOTSTRAP_RUN_LOG_SET',
      payload = list(
        ActionStatus = 'SUCCESS',
        RunLog = appMgr$HIVModelMgr$BootstrapFitTask$HTMLRunLog
      )
    )
  })

  observeEvent(input$cancelBootstrapBtn, {
    appMgr$HIVModelMgr$CancelBootstrapFit()
  })
}

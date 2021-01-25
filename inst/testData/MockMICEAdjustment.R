list(
  # Adjustment name ----
  Name = 'Mock MICE Adjustment',

  # Adjustment type ----
  Type = 'MULTIPLE_IMPUTATIONS',

  # Adjustment subtype ----
  SubType = 'MICE',

  # Input parameters to the adjustment function ----
  Parameters = list(
    # Parameter 1: a specification with label, type and default value
    nimp = list(
      label = 'Number of imputations',
      value = 2L,
      input = 'numeric'
    ),
    # Parameter 2
    nit = list(
      label = 'Number of mice iterations',
      value = 10L,
      input = 'numeric'
    ),
    # Parameter 3
    nsdf = list(
      label = 'Number of degrees of freedom for spline of diagnosis calendar year',
      value = 4L,
      min = 3L,
      max = 5L,
      step = 1L,
      ticks = TRUE,
      round = TRUE,
      input = 'slider'
    ),
    # Parameter 4
    imputeRD = list(
      label = 'Impute reporting delays inputs',
      value = FALSE,
      input = 'checkbox'
    )
  ),

  # Names of packages that must be made available to the adjustment function ----
  RequiredPackageNames = c('mice'),

  ## Adjustment function ----
  AdjustmentFunction = function(inputData, parameters) {

    require(data.table, quietly = TRUE)

    # Perform imputations per data set.
    # This is the actual worker function.
    WorkerFunction <- function(i, nit, nimp, nsdf, imputeRD) {

      cat('\n')
      cat(sprintf('Processing gender: %s\n', names(dataSets)[i]))

      dataSet <- dataSets[[i]]

      artifacts <- list()

      # Define covariates
      xColNamesAll <- c('AIDS')
      # Define outcomes
      yColNamesAll <- c('Age', 'SqCD4', 'Transmission', 'GroupedRegionOfOrigin')

      # Determine which columns to pass to the mice package

      # At least 2 distinct values present
      XFilterFunc <- function(colName) length(unique(dataSet[[colName]])) >= 2
      # At least one non-NA
      YFilterFunc <- function(colName) !all(is.na(dataSet[[colName]]))

      # Keep only column names meeting requirement
      xColNames <- Filter(XFilterFunc, xColNamesAll)
      yColNames <- Filter(YFilterFunc, yColNamesAll)

      indexColNames <- c('Imputation', 'Id')
      impColNames <- union(indexColNames, yColNames)
      dataSetColNames <- setdiff(
        colnames(dataSet),
        union(yColNames, c('Imputation', 'LogTweakedMaxPossibleDelay'))
      )

      imp <- dataSet[rep(seq_len(.N), nimp)]
      imp[, Imputation := rep(seq_len(nimp), each = nrow(dataSet))]
      mi <- rbind(dataSet, imp)
      mi[, Id := seq_len(nrow(.SD)), by = .(Imputation)]

      setcolorder(mi, union(indexColNames, dataSetColNames))

      ConvertDataTableColumns(mi, c(Imputation = function(x) as.integer(as.character(x))))

      mi[, FirstCD4Count := SqCD4^2]

      return(list(Data = mi, Artifacts = artifacts))
    }

    # 1. Save original order for later
    inputData[, OrigSort := .I]

    # 2. Measures years from earlier diagnosis year
    inputData[, DY := YearOfHIVDiagnosis - min(YearOfHIVDiagnosis, na.rm = TRUE)]

    # 3. Split by gender to data sets
    dataSets <- split(inputData, by = c('Gender'))

    # 4. Execute the worker function per data set
    outputData <- lapply(
      seq_along(dataSets),
      WorkerFunction,
      nit = parameters$nit,
      nimp = parameters$nimp,
      nsdf = parameters$nsdf,
      imputeRD = parameters$imputeRD
    )

    # 5. Combine all data sets
    names(outputData) <- names(dataSets)
    data <- rbindlist(lapply(outputData, '[[', 'Data'))
    artifacts <- lapply(outputData, '[[', 'Artifacts')

    # 6. Restore original order per Imputation
    setorder(data, Imputation, OrigSort)

    # 7. Clean up
    data[, ':='(
      Id = NULL,
      OrigSort = NULL,
      DY = NULL
    )]

    return(
      list(
        Data = data,
        Artifacts = artifacts
      )
    )
  }
)

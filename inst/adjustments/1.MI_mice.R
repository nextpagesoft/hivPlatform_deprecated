list(
  # Adjustment name ----
  Name = 'Multiple Imputation using Chained Equations - MICE',

  # Adjustment type ----
  Type = 'MULTIPLE_IMPUTATIONS',

  # Adjustment subtype ----
  SubType = 'MICE',

  # Input parameters to the adjustment function ----
  Parameters = list(
    # Parameter 1: a specification with label, type and default value
    nimp = list(
      label = 'Number of imputations',
      value = 5L,
      input = 'numeric'
    ),
    # Parameter 2
    nit = list(
      label = 'Number of mice iterations',
      value = 5L,
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
    # Parameter 5
    # runInParallel = list(
    #   label = 'Run in parallel',
    #   value = FALSE,
    #   input = 'checkbox')
  ),

  # Adjustment function ----
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

      if (imputeRD) {
        dataSet[, LogTweakedMaxPossibleDelay := log(TweakedMaxPossibleDelay)]

        xColNamesAll <- union(xColNamesAll, c('LogTweakedMaxPossibleDelay'))
        yColNamesAll <- union(yColNamesAll, c('VarX'))
      }

      # Determine which columns to pass to the mice package

      # At least 2 distinct values present
      XFilterFunc <- function(colName) length(unique(dataSet[[colName]])) >= 2
      # At least one non-NA
      YFilterFunc <- function(colName) !all(is.na(dataSet[[colName]]))

      # Keep only column names meeting requirement
      xColNames <- Filter(XFilterFunc, xColNamesAll)
      yColNames <- Filter(YFilterFunc, yColNamesAll)

      # Keep for reporting
      artifacts[['X_COLS']] <- list(All = xColNamesAll, Kept = xColNames)
      artifacts[['Y_COLS']] <- list(All = yColNamesAll, Kept = yColNames)

      # Create splines with proper names and intercept
      splineBasisMatrix <- try(as.data.table(splines::ns(dataSet$DY, df = nsdf)), silent = TRUE)
      if (IsError(splineBasisMatrix)) {
        splineBasisMatrix <- data.table()
      } else {
        setnames(splineBasisMatrix, paste0('SplineKnot.', colnames(splineBasisMatrix)))
      }

      intercept <- 1L

      # Define covariates of joint imputation model
      X <- cbind(Intercept = intercept, splineBasisMatrix)
      if (length(xColNames) > 0) {
        X <- cbind(dataSet[, ..xColNames], X)
      }

      if (length(yColNames) > 0) {
        # Define outcomes of joint imputation model
        Y <- dataSet[, ..yColNames]

        # Unused levels must be removed
        X <- droplevels(X)
        Y <- droplevels(Y)

        # Run model
        cat('Performing imputation.\n')
        mids <- mice::mice(cbind(Y, X), m = nimp, maxit = nit)
        artifacts[['Mids']] <- mids

        imp <- setDT(mice::complete(mids, action = 'long', include = TRUE))
        setnames(imp, old = c('.imp', '.id'), new = c('Imputation', 'Id'))

      } else {
        imp <- data.table(Imputation = 0L, Id = seq_len(nrow(Y)))
      }

      indexColNames <- c('Imputation', 'Id')
      impColNames <- union(indexColNames, yColNames)
      dataSetColNames <- setdiff(
        colnames(dataSet),
        union(yColNames, c('Imputation', 'LogTweakedMaxPossibleDelay'))
      )

      mi <- cbind(imp[, ..impColNames], dataSet[, ..dataSetColNames])

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

PrepareMigrantData <- function(
  data,
  seed = NULL
) {
  minDate <- as.Date('1980-01-01')
  maxDate <- as.Date('3000-01-01')
  yearDays <- 365.25

  # Pre-process data -------------------------------------------------------------------------------
  colNames <- c(
    'Imputation', 'RecordId', 'Gender', 'Transmission', 'Age', 'DateOfArrival',
    'ReportingCountry', 'FullRegionOfOrigin', 'GroupedRegionOfOrigin',
    'DateOfHIVDiagnosis', 'AcuteInfection', 'FirstCD4Count', 'LatestCD4Count',
    'DateOfFirstCD4Count', 'DateOfLatestCD4Count', 'LatestVLCount', 'DateOfLatestVLCount',
    'DateOfArt', 'DateOfAIDSDiagnosis', 'YearOfHIVDiagnosis'
  )
  data <- data[, ..colNames]

  # Remap low level grouped region of origin to the high level set
  groupingMap <- data.table(
    LowLevel = c(
      'EASTERN EUROPE', 'EUROPE-OTHER', 'SUB-SAHARAN AFRICA', 'AFRICA-OTHER',
      'CARIBBEAN-LATIN AMERICA'
    ),
    HighLevel = c('EUROPE', 'EUROPE', 'AFRICA', 'AFRICA', 'OTHER')
  )
  data[, GroupedRegionOfOriginLowLevel := GroupedRegionOfOrigin]
  data[
    groupingMap,
    GroupedRegionOfOrigin := i.HighLevel,
    on = .(GroupedRegionOfOriginLowLevel = LowLevel)
  ]
  data[is.na(GroupedRegionOfOrigin), GroupedRegionOfOrigin := 'UNKNOWN']
  data[, GroupedRegionOfOrigin :=
    factor(stringi::stri_trans_totitle(as.character(GroupedRegionOfOrigin)))]

  PrintH2('Checking data structure validity')
  columnSpecs <- GetListObject(
    GetSystemFile('referenceData/requiredColumns.R'),
    includeFileName = FALSE
  )
  columnSpecs[['Imputation']] <- list(
    type = 'integer',
    defaultValue = NA_integer_
  )
  dataStructValidity <- GetInputDataValidityStatus(data, columnSpecs[colNames])
  if (dataStructValidity$Valid) {
    PrintAlert('Data valid', type = 'success')
  } else {
    PrintAlert('Data invalid', type = 'danger')
  }

  # Merge ReportingOrigin
  data[countryData, ReportingOrigin := i.TESSyCode, on = .(ReportingCountry = Code)]

  # Add DateOfBirth
  data[, DateOfBirth := DateOfHIVDiagnosis - Age * yearDays]

  # Generate at risk date
  data[, AtRiskDate := pmax(DateOfBirth + 10 * yearDays, minDate)]
  data[
    !(AcuteInfection %in% c('UNK', 'NA', NA_character_)),
    AtRiskDate := pmax(AtRiskDate, DateOfHIVDiagnosis - 0.5 * yearDays)
  ]

  # Years from risk onset to HIV diagnosis
  data[, U := as.numeric(DateOfHIVDiagnosis - AtRiskDate) / 365.25]

  # Initialize filters -----------------------------------------------------------------------------
  data[, Excluded := '']
  data[Excluded == '' & is.na(FullRegionOfOrigin), Excluded := 'Full region of origin is missing']
  data[
    Excluded == '' & !is.na(FullRegionOfOrigin) & FullRegionOfOrigin == 'REPCOUNTRY',
    Excluded := 'Regions of origin and reporting are the same'
  ]
  data[
    Excluded == '' & !(Transmission %in% c('MSM', 'IDU', 'HETERO')),
    Excluded := 'Transmission is not of mode "MSM", "IDU", or "HETERO"'
  ]
  data[
    Excluded == '' & DateOfArrival < DateOfBirth,
    Excluded := 'Date of arrival is before date of birth'
  ]
  data[Excluded == '' & is.na(Age), Excluded := 'Age is missing']
  data[Excluded == '' & Age <= 15, Excluded := 'Age is below 16']
  data[Excluded == '' & U <= 0, Excluded := 'Date of HIV diagnosis is before risk onset']

  # Impute date of arrival -------------------------------------------------------------------------
  data[, YearsToArrival := as.numeric(DateOfArrival - DateOfBirth) / yearDays]
  data[, PropBeforeArrival := YearsToArrival / Age]
  data[, ImputeData :=
    Excluded == '' &
      (is.na(PropBeforeArrival) | between(PropBeforeArrival, 0, 1)) &
      (
        !is.na(Gender) & !is.na(Transmission) & !is.na(Age) & !is.na(FirstCD4Count) &
          !is.na(GroupedRegionOfOrigin) & !is.na(YearOfHIVDiagnosis)
      )
  ]

  # Get data to be imputed and a sample of full data for the imputation
  imputeData <- data[
    ImputeData == TRUE,
    .(
      Imputation = factor(Imputation),
      Gender,
      Transmission,
      Age,
      FirstCD4Count,
      GroupedRegionOfOrigin,
      YearOfHIVDiagnosis = as.factor(YearOfHIVDiagnosis),
      PropBeforeArrival
    )
  ]
  selNotNA <- imputeData[, !is.na(PropBeforeArrival)]
  # Prepare logit transformation
  imputeData[selNotNA & between(PropBeforeArrival, 0, 0.00001), PropBeforeArrival := 0.00001]
  imputeData[selNotNA & between(PropBeforeArrival, 0.99999, 1), PropBeforeArrival := 0.99999]
  imputeData[selNotNA, PropBeforeArrivalLogit := log(PropBeforeArrival / (1 - PropBeforeArrival))]
  imputeData[, PropBeforeArrival := NULL]
  imputeWhere <- data.table(
    Imputation = FALSE,
    Gender = FALSE,
    Transmission = FALSE,
    Age = FALSE,
    FirstCD4Count = FALSE,
    GroupedRegionOfOrigin = FALSE,
    YearOfHIVDiagnosis = FALSE,
    PropBeforeArrivalLogit = !selNotNA
  )

  set.seed(seed)
  miceImputation <- suppressWarnings(mice::mice(
    imputeData,
    where = imputeWhere,
    m = 1,
    maxit = 5,
    printFlag = FALSE
  ))
  imputeData <- setDT(mice::complete(miceImputation, action = 1))

  data[ImputeData == TRUE, PropBeforeArrivalLogit := imputeData$PropBeforeArrivalLogit]
  data[, PropBeforeArrivalImputed := exp(PropBeforeArrivalLogit) / (1 + exp(PropBeforeArrivalLogit))] # nolint
  data[, DateOfArrivalImputed := DateOfBirth + (Age * PropBeforeArrivalImputed) * yearDays]

  # Print statistics
  imputeStat <- data[,
    .(
      CountBeforeImputation = sum(ImputeData & !is.na(DateOfArrival)),
      CountAfterImputation = sum(ImputeData & !is.na(DateOfArrivalImputed)),
      CountImputed = sum(ImputeData & !is.na(DateOfArrivalImputed)) - sum(ImputeData & !is.na(DateOfArrival)), # nolint
      CountTotal = .N
    ),
    by = .(Imputation)
  ]

  PrintH1('Counts of imputed dates of arrival')
  print(knitr::kable(
    imputeStat,
    format = 'simple',
    escape = FALSE,
    col.names = c('Imputation', 'Before imputation', 'After imputation', 'Imputed', 'Total')
  ))

  data[, ':='(
    DateOfArrival = DateOfArrivalImputed,
    DateOfArrivalImputed = NULL
  )]

  # ------------------------------------------------------------------------------------------------

  data[Excluded == '' & is.na(DateOfArrival), Excluded := 'Date of arrival is missing']

  missStat <- rbind(
    data[Excluded != '', .(Count = .N), by = .(Excluded)][order(-Count)],
    data[Excluded != '', .(Excluded = 'Total excluded', Count = .N)],
    data[Excluded == '', .(Excluded = 'Total used in estimation', Count = .N)]
  )

  PrintH1('Statistics of exclusions')
  print(knitr::kable(
    missStat,
    format = 'simple',
    escape = FALSE
  ))

  # Process data -----------------------------------------------------------------------------------
  base <- data[Excluded == '']

  # Generate unique identifier
  base[, ':='(
    UniqueId = rleid(Imputation, RecordId),
    Ord = rowid(Imputation, RecordId)
  )]

  # Add mode of infection
  modeMapping <- data.table(
    Transmission = c('MSM', 'IDU', 'HETERO'),
    Mode = factor(c('MSM', 'IDU', 'MSW'), levels = c('MSM', 'IDU', 'MSW', 'OTHER/UNK'))
  )
  base[
    modeMapping,
    Mode := i.Mode,
    on = .(Transmission)
  ]

  # Years since 1/1/1980
  base[, Calendar := as.numeric(DateOfHIVDiagnosis - minDate) / 365.25]

  # Years from migration to HIV diagosis
  base[, Mig := as.numeric(DateOfHIVDiagnosis - DateOfArrival) / 365.25]

  base[, KnownPrePost := fcase(Mig < 0, 'Pre', Mig > U, 'Post', default = 'Unknown')]

  # CD4 dataset
  cd4 <- base[, .(
    UniqueId,
    YVar_1 = FirstCD4Count,
    YVar_2 = LatestCD4Count,
    DateOfExam_1 = pmax(DateOfHIVDiagnosis, DateOfFirstCD4Count, na.rm = TRUE),
    DateOfExam_2 = DateOfLatestCD4Count,
    Indi = 'CD4'
  )]
  cd4 <- melt(
    cd4,
    measure.vars = patterns('^DateOfExam', '^YVar'),
    value.name = c('DateOfExam', 'YVar'),
    variable.name = 'Time'
  )
  cd4[, Time := NULL]

  # VL dataset
  rna <- base[, .(
    UniqueId,
    YVar = LatestVLCount,
    DateOfExam = DateOfLatestVLCount,
    Indi = 'RNA'
  )]
  rna[!is.na(YVar) & YVar == 0, YVar := 25]

  # Combine both markers
  cd4VL <- rbind(cd4, rna, use.names = TRUE)

  # Merge markers with base
  baseCD4VL <- merge(cd4VL, base, by = c('UniqueId'))
  # Keep observations prior to ART initiation and AIDS onset
  baseCD4VL <- baseCD4VL[
    !is.na(DateOfExam) &
      DateOfExam <= na.replace(DateOfArt, maxDate) &
      DateOfExam <= na.replace(DateOfAIDSDiagnosis, maxDate)
  ]

  # Exlude negative times
  baseCD4VL[, DTime := as.numeric(DateOfExam - DateOfHIVDiagnosis) / 365.25]
  baseCD4VL <- baseCD4VL[DTime >= -15 / 365.25]
  baseCD4VL[DTime < 0, DTime := 0]

  # Indicators of a CD4 or VL measurement
  baseCD4VL[, ':='(
    Consc = as.integer(Indi == 'CD4'),
    Consr = as.integer(Indi == 'RNA')
  )]
  baseCD4VL[,
    Only := fcase(
      any(Indi == 'CD4') & any(Indi == 'RNA'), 'Both',
      any(Indi == 'CD4') & !any(Indi == 'RNA'), 'CD4 only',
      !any(Indi == 'CD4') & any(Indi == 'RNA'), 'VL only'
    ),
    by = .(UniqueId)
  ]

  # Transform CD4 and VL
  baseCD4VL[Consc == 1, YVar := sqrt(YVar)]
  baseCD4VL[Consr == 1, YVar := log10(YVar)]

  # Times of measurements
  baseCD4VL[, ':='(
    CobsTime = DTime * Consc,
    RobsTime = DTime * (1 - Consc),
    RLogObsTime2 = log(DTime + 0.013) * (1 - Consc)
  )]

  # Cases with no pre-ART/AIDS markers (CD4, VL) at all
  baseAIDS <- base[!(UniqueId %chin% baseCD4VL$UniqueId)]
  baseAIDS[, DTime := as.numeric(DateOfAIDSDiagnosis - DateOfHIVDiagnosis) / 365.25]
  baseAIDS[DTime < 0, DTime := 0]

  return(list(
    Data = list(
      CD4VL = baseCD4VL,
      AIDS = baseAIDS
    ),
    Stats = list(
      Missingness = missStat,
      Imputation = imputeStat
    )
  ))
}

#' PreProcessInputDataBeforeSummary
#'
#' Pre-processes input data before making data summary and passing it to adjustment scripts.
#'
#' @param inputData Input data. Required.
#' @param seed Random seed. Optional. Default = NULL
#'
#' @return list object
#'
#' @examples
#' \dontrun{
#' PreProcessInputDataBeforeSummary(inputData)
#' }
#'
#' @export
PreProcessInputDataBeforeSummary <- function( # nolint
  inputData,
  seed = NULL
) {
  stopifnot(!missing(inputData))

  if (is.null(inputData)) {
    return(NULL)
  }

  # Merge RegionOfBirth
  inputData[countryData, RegionOfBirth := i.TESSyCode, on = .(CountryOfBirth = Code)]
  inputData[
    !is.na(CountryOfBirth) & CountryOfBirth %chin% ReportingCountry,
    RegionOfBirth := 'REPCOUNTRY'
  ]

  # Merge RegionOfNationality
  inputData[countryData, RegionOfNationality := i.TESSyCode, on = .(CountryOfNationality = Code)]
  inputData[
    !is.na(CountryOfNationality) & CountryOfNationality %chin% ReportingCountry,
    RegionOfNationality := 'REPCOUNTRY'
  ]

  # Create detailed 'Migrant' variable: FullRegionOfOrigin based on Region Of Origin
  inputData[, FullRegionOfOrigin := RegionOfOrigin]
  # Update if missing with Region of Birth
  inputData[is.na(FullRegionOfOrigin), FullRegionOfOrigin := RegionOfBirth]
  # Update if 'ABROAD' with Region of Birth
  inputData[
    FullRegionOfOrigin %chin% 'ABROAD' & !is.na(RegionOfBirth) &
      !CountryOfBirth %chin% ReportingCountry,
    FullRegionOfOrigin := RegionOfBirth
  ]
  # Update if missing with Region of Nationality
  inputData[is.na(FullRegionOfOrigin), FullRegionOfOrigin := RegionOfNationality]
  # Update if 'ABROAD' with Region of Nationality
  inputData[
    FullRegionOfOrigin %chin% 'ABROAD' & !is.na(RegionOfNationality) &
      !CountryOfNationality %chin% ReportingCountry,
    FullRegionOfOrigin := RegionOfNationality
  ]
  inputData[is.na(FullRegionOfOrigin), FullRegionOfOrigin := 'UNK']

  # # Cache country codes for countries of Sub-Saharan Africa
  # ssaCountryCodes <- countryData[SubRegionName == 'Sub-Saharan Africa', unique(Code)]
  # # Create GroupOfOrigin variable 1, 2, 3...
  # inputData[, GroupOfOrigin := factor(NA, levels = c('Reporting Country', 'Other Country', 'SSA'))]
  # # ...based on RegionOfOrigin if not NA
  # inputData[
  #   !is.na(RegionOfOrigin),
  #   GroupOfOrigin := ifelse(
  #     RegionOfOrigin == 'REPCOUNTRY', 1L, ifelse(!RegionOfOrigin %chin% 'SUBAFR', 2L, 3L)
  #   )
  # ]
  # # ...based on CountryOfBirth if not NA
  # inputData[
  #   is.na(GroupOfOrigin) & !is.na(CountryOfBirth),
  #   GroupOfOrigin := ifelse(
  #     CountryOfBirth == ReportingCountry, 1L, ifelse(!CountryOfBirth %chin% ssaCountryCodes, 2L, 3L)
  #   )
  # ]
  # # ...based on CountryOfNationality if not NA
  # inputData[
  #   is.na(GroupOfOrigin) & !is.na(CountryOfNationality),
  #   GroupOfOrigin := ifelse(
  #     CountryOfNationality == ReportingCountry,
  #     1L,
  #     ifelse(!CountryOfNationality %chin% ssaCountryCodes, 2L, 3L)
  #   )
  # ]

  # Transform CD4
  inputData[, SqCD4 := sqrt(FirstCD4Count)]

  # Add year of diagnosis. It is used in many places, so cache it here
  inputData[, YearOfHIVDiagnosis := year(DateOfHIVDiagnosis)]

  # Imput missing Gender and YearOfHIVDiagnosis
  selGenderMissing <- inputData[, is.na(Gender)]
  selGenderReplaced <- selGenderMissing & inputData$Transmission %chin% 'MSM'
  selGenderImputed <- selGenderMissing & !selGenderReplaced
  # If Gender missing and Transmission is MSM, then set Male gender
  inputData[selGenderReplaced, Gender := 'M']
  # A single imputation based on categorical year and transmission
  if (any(selGenderImputed)) {
    imputeData <- inputData[, .(
      Gender = as.factor(Gender),
      YearOfHIVDiagnosis = as.factor(YearOfHIVDiagnosis),
      Transmission = Transmission
    )]
    imputeWhere <- data.table(
      Gender = selGenderImputed,
      YearOfHIVDiagnosis = FALSE,
      Transmission = FALSE
    )
    set.seed(seed)
    miceImputation <- suppressWarnings(mice::mice(
      imputeData,
      where = imputeWhere,
      m = 1,
      maxit = 5,
      printFlag = TRUE
    ))
    imputeData <- setDT(mice::complete(miceImputation, action = 1))

    inputData[selGenderImputed, Gender := imputeData$Gender[selGenderImputed]]
  }

  # AIDS close to diagnosis
  inputData[, AIDS := factor(ifelse(
    !is.na(DateOfAIDSDiagnosis),
    ifelse(DateOfAIDSDiagnosis <= DateOfHIVDiagnosis, 'AIDS-Yes', 'AIDS-No'),
    'AIDS-No'
  ))]

  # Create helper columns for filtering data on diagnosis and notification time
  inputData[, ':='(
    NotificationTime = year(DateOfNotification) + 1 / 4 * quarter(DateOfNotification) - 0.125,
    DiagnosisTime = YearOfHIVDiagnosis + 1 / 4 * quarter(DateOfHIVDiagnosis)  - 0.125
  )]

  # Transform columns to factor
  inputData[, Gender := droplevels(factor(Gender, levels = c('M', 'F', 'O')))]
  inputData[, Transmission := factor(Transmission)]

  artifacts <- list(
    MissGenderReplaced = sum(selGenderReplaced),
    MissGenderImputed = sum(selGenderImputed)
  )

  return(artifacts)
}

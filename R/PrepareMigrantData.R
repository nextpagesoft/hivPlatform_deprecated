PrepareMigrantData <- function(
  fileName = 'D:/VirtualBox_Shared/BE.csv'
) {
  covs <- ReadDataFile(fileName)[, c(
    'recordid', 'yearofarrivalisodate', 'art', 'artdateisodate', 'cd4latest',
    'cd4latestdateisodate', 'dateofaidsdiagnosisisodate', 'dateofdiagnosisisodate',
    'firstcd4dateisodate', 'vllatest', 'vllatestdateisodate', 'acuteinfection'
  )]

  adjData <- ReadDataFile('D:/VirtualBox_Shared/BE_adjusted.rds')

  # Filter
  adjDataMigr <- adjData[
    Imputation == 1 &
      Transmission %in% c('MSM', 'IDU', 'HETERO') &
      Age > 10 &
      !is.na(DateOfArrival) &
      !is.na(FullRegionOfOrigin)
  ]

  # Generate mode of infection
  adjDataMigr[, Mode := NA_character_]
  adjDataMigr[Transmission == 'MSM', Mode := 'MSM']
  adjDataMigr[Transmission == 'IDU', Mode := 'IDU']
  adjDataMigr[Transmission == 'HETERO', Mode := 'MSW']
  adjDataMigr[is.na(Transmission), Mode := 'Other/Unknown']
  adjDataMigr[, Mode := factor(Mode, levels = c('MSM', 'IDU', 'MSW', 'Other/Unknown'))]

  # Gender
  currentLevels <- levels(adjDataMigr$Gender)
  newLevels <- c('Male', 'Female')
  levels(adjDataMigr$Gender) <- newLevels[match(currentLevels, c('M', 'F'))]

  # Region of origin
  regionMapping <- data.table(
    FullRegionOfOrigin = c(
      'CENTEUR', 'EASTEUR', 'WESTEUR', 'EUROPE', 'NORTHAM', 'NORTHAFRMIDEAST', 'SUBAFR',
      'SOUTHASIA', 'EASTASIAPAC'
    ),
    GroupedRegion = c(
      'Europe', 'Europe', 'Europe', 'Europe', 'Europe', 'Africa', 'Africa', 'Asia', 'Asia'
    )
  )
  adjDataMigr[
    regionMapping,
    GroupedRegion := i.GroupedRegion,
    on = .(FullRegionOfOrigin)
  ]
  adjDataMigr[is.na(GroupedRegion), GroupedRegion := 'Unknown']
  adjDataMigr[,
    GroupedRegion := factor(
      GroupedRegion,
      levels = union(unique(regionMapping$GroupedRegion), 'Unknown')
    )
  ]

  # Create birth date
  adjDataMigr[, DateOfBirth := DateOfHIVDiagnosis - Age * 365.25]

  # Calendar time since 1/1/1980
  adjDataMigr[, Calendar := as.numeric(DateOfHIVDiagnosis - as.Date('1980-1-1')) / 365.25]

  # Indicator of a AcuteInfection
  adjDataMigr[, AcuTest := !is.na(AcuteInfection)]

  # Generate at risk date
  adjDataMigr[, AtRiskDate := as.Date('1980-1-1')]
  adjDataMigr[
    AcuTest == FALSE,
    AtRiskDate := pmax(DateOfBirth + 10 * 365.25, as.Date('1980-1-1'))
  ]
  adjDataMigr[
    AcuTest == TRUE,
    AtRiskDate := pmax(
      DateOfBirth + 10 * 365.25, as.Date('1980-1-1'), DateOfHIVDiagnosis - 0.5 * 365.25
    )
  ]
}

GetMigrantData <- function(
  fileName = 'D:/VirtualBox_Shared/BE_adjusted.rds'
) {
  # covs <- ReadDataFile('D:/VirtualBox_Shared/BE.csv')[, c(
  #   'recordid', 'yearofarrivalisodate', 'art', 'artdateisodate', 'cd4latest',
  #   'cd4latestdateisodate', 'dateofaidsdiagnosisisodate', 'dateofdiagnosisisodate',
  #   'firstcd4dateisodate', 'vllatest', 'vllatestdateisodate', 'acuteinfection'
  # )]
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
  stopifnot(all(currentLevels %in% c('M', 'F')))
  newLevels <- c('Male', 'Female')
  levels(adjDataMigr$Gender) <- newLevels[match(currentLevels, c('M', 'F'))]

  # Region of origin
  regionMapping <- data.table(
    FullRegionOfOrigin = c(
      'CENTEUR', 'EASTEUR', 'WESTEUR', 'EUROPE', 'NORTHAM',
      'NORTHAFRMIDEAST', 'SUBAFR',
      'SOUTHASIA', 'EASTASIAPAC'
    ),
    GroupedRegion = c(
      'Europe', 'Europe', 'Europe', 'Europe', 'Europe',
      'Africa', 'Africa',
      'Asia', 'Asia'
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

  # Years since 1/1/1980
  adjDataMigr[, Calendar := as.numeric(DateOfHIVDiagnosis - as.Date('1980-1-1')) / 365.25]

  # Indicator of an acute infection
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

  # Years from risk onset to HIV diagnosis
  adjDataMigr[, U := as.numeric(DateOfHIVDiagnosis - AtRiskDate) / 365.25]
  # There should not be any negative U's
  adjDataMigr <- adjDataMigr[U > 0]

  # Years from migration to HIV diagosis
  adjDataMigr[, Mig := as.numeric(DateOfHIVDiagnosis - DateOfArrival) / 365.25]

  adjDataMigr[, KnownPrePost := 'Unknown']
  adjDataMigr[Mig < 0, KnownPrePost := 'Pre']
  adjDataMigr[Mig > U, KnownPrePost := 'Post']

  # Base dataset
  base <- adjDataMigr[, .(
    Patient = RecordId, Gender, Mode, AgeDiag = Age, GroupedRegion, Calendar, Art, DateOfArt,
    DateOfHIVDiagnosis, DateOfAIDSDiagnosis, DateOfArrival, DateOfBirth, AtRiskDate, U, Mig,
    KnownPrePost
  )]

  # CD4 dataset
  cd4 <- adjDataMigr[, .(
    Patient = RecordId,
    Value_1 = FirstCD4Count,
    Value_2 = LatestCD4Count,
    DateOfExam_1 = pmax(DateOfHIVDiagnosis, DateOfFirstCD4Count, na.rm = TRUE),
    DateOfExam_2 = DateOfLatestCD4Count,
    Indi = 'cd4'
  )]
  cd4 <- melt(
    cd4,
    measure.vars = patterns('^DateOfExam', '^Value'),
    value.name = c('DateOfExam', 'Value'),
    variable.name = 'Time'
  )
  setorderv(cd4, c('Patient', 'Time'))
  cd4[, Time := NULL]

  # VL dataset
  rna <- adjDataMigr[, .(
    Patient = RecordId,
    Value = LatestVLCount,
    DateOfExam = DateOfLatestVLCount,
    Indi = 'rna'
  )]
  rna[!is.na(Value) & Value == 0, Value := 25]

  # Combine both markers
  cd4VL <- rbind(cd4, rna, use.names = TRUE)

  # Merge markers with base
  baseCD4VL <- merge(cd4VL, base, by = 'Patient')
  setorderv(baseCD4VL, c('Patient', 'Indi', 'DateOfExam'))
  # Keep observations prior to ART initiation and AIDS onset
  baseCD4VL <- baseCD4VL[
    !is.na(DateOfExam) &
      DateOfExam <= na.replace(DateOfArt, as.Date('3000-01-01')) &
      DateOfExam <= na.replace(DateOfAIDSDiagnosis, as.Date('3000-01-01'))
  ]

  # Exlude negative times
  baseCD4VL[, DTime := as.numeric(DateOfExam - DateOfHIVDiagnosis) / 365.25]
  baseCD4VL <- baseCD4VL[DTime >= -15 / 365.25]
  baseCD4VL[DTime < 0, DTime := 0]

  # Create a numeric id
  baseCD4VL[, Id := .I, by = .(Patient)]

  # Indicators of a CD4 or VL measurement
  baseCD4VL[, ':='(
    Consc = as.integer(Indi == 'cd4'),
    Consr = as.integer(Indi == 'rna')
  )]
  setorderv(baseCD4VL, c('Id', 'Consc', 'DateOfExam'))

  # Rename the measurement value
  setnames(baseCD4VL, old = 'Value', new = 'YVar')

  # Transform cd4 and vl
  baseCD4VL[Consc == 1, YVar := sqrt(YVar)]
  baseCD4VL[Consr == 1, YVar := log10(YVar)]

  # Times of measurments
  baseCD4VL[, ':='(
    CobsTime = DTime * Consc,
    RobsTime = DTime * (1 - Consc),
    RLogObsTime2 = log(DTime + 0.013) * (1 - Consc)
  )]

  baseCD4VL[, ':='(SumConsc = sum(Consc), SumConsr = sum(Consr)), by = .(Id)]
  baseCD4VL[SumConsc > 0 & SumConsr > 0, Only := 'Both']
  baseCD4VL[SumConsc > 0 & SumConsr == 0, Only := 'CD4 only']
  baseCD4VL[SumConsc == 0 & SumConsr > 0, Only := 'VL only']

  # Cases with no pre-ART/AIDS markers (CD4,VL) at all
  baseAIDS <- base[!(Patient %in% baseCD4VL$Patient)]
  baseAIDS[, Id := .I, by = .(Patient)]
  baseAIDS[, DTime := as.numeric(DateOfAIDSDiagnosis - DateOfHIVDiagnosis) / 365.25]
  baseAIDS[DTime < 0, DTime := 0]

  return(list(
    CD4VL = baseCD4VL,
    AIDS = baseAIDS
  ))
}

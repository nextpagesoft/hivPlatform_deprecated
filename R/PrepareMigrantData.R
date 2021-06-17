PrepareMigrantData <- function(
  data
) {
  # Reference data
  regionMapping <- data.table(
    FullRegionOfOrigin = c(
      'CENTEUR', 'EASTEUR', 'WESTEUR', 'EUROPE', 'NORTHAM',
      'NORTHAFRMIDEAST', 'SUBAFR',
      'SOUTHASIA', 'EASTASIAPAC'
    ),
    GroupedRegion = factor(c(
      'Europe', 'Europe', 'Europe', 'Europe', 'Europe',
      'Africa', 'Africa',
      'Asia', 'Asia'
    ), levels = c('Europe', 'Africa', 'Asia', 'Unknown'))
  )

  modeMapping <- data.table(
    Transmission = c('MSM', 'IDU', 'HETERO'),
    Mode = factor(c('MSM', 'IDU', 'MSW'), levels = c('MSM', 'IDU', 'MSW', 'Other/Unknown'))
  )

  # Filter
  base <- data[
    Transmission %in% c('MSM', 'IDU', 'HETERO') &
    !is.na(Age) & Age > 10 &
    !is.na(DateOfArrival) &
    !is.na(FullRegionOfOrigin),
    .(
      Imputation, RecordId, Gender, Transmission, Age, DateOfArrival, FullRegionOfOrigin,
      DateOfHIVDiagnosis, AcuteInfection, FirstCD4Count, LatestCD4Count, DateOfFirstCD4Count,
      DateOfLatestCD4Count, LatestVLCount, DateOfLatestVLCount, DateOfArt, DateOfAIDSDiagnosis
    )
  ]

  # Generate at risk date
  base[, AtRiskDate := pmax(DateOfHIVDiagnosis - Age * 365.25 + 10 * 365.25, as.Date('1980-1-1'))]
  base[!is.na(AcuteInfection), AtRiskDate := pmax(AtRiskDate, DateOfHIVDiagnosis - 0.5 * 365.25)]

  # Years from risk onset to HIV diagnosis
  base[, U := as.numeric(DateOfHIVDiagnosis - AtRiskDate) / 365.25]
  # There should not be any negative U's
  base <- base[U > 0]

  # Generate unique identifier
  base[, ':='(
    UniqueId = rleid(Imputation, RecordId),
    Ord = rowid(Imputation, RecordId)
  )]

  # Add mode of infection
  base[
    modeMapping,
    Mode := i.Mode,
    on = .(Transmission)
  ]

  # Add grouped region of origin
  base[
    regionMapping,
    GroupedRegion := i.GroupedRegion,
    on = .(FullRegionOfOrigin)
  ]
  base[is.na(GroupedRegion), GroupedRegion := 'Unknown']

  # Years since 1/1/1980
  base[, Calendar := as.numeric(DateOfHIVDiagnosis - as.Date('1980-1-1')) / 365.25]

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
      DateOfExam <= na.replace(DateOfArt, as.Date('3000-01-01')) &
      DateOfExam <= na.replace(DateOfAIDSDiagnosis, as.Date('3000-01-01'))
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
    CD4VL = baseCD4VL,
    AIDS = baseAIDS
  ))
}

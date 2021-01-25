list(
  RecordId = list(
    attribute = 'RecordId',
    desciption =
      'Unique identifier for each record within and across the national surveillance system',
    type = 'character',
    defaultValue = NA_character_,
    candidateOrigColNames = c('recordid')
  ),
  ReportingCountry = list(
    attribute = 'ReportingCountry',
    description = 'The country reporting the record',
    type = 'character',
    values = union('UNK', countryData$Code),
    defaultValue = NA_character_,
    candidateOrigColNames = c('reportingcountry')
  ),
  Age = list(
    attribute = 'Age',
    description = 'Exact age at diagnosis of HIV. Age as a crude number is preferred',
    type = 'numeric',
    defaultValue = NA_real_,
    candidateOrigColNames = c('age')
  ),
  Gender = list(
    attribute = 'Gender',
    description = 'Gender',
    type = 'character',
    values = c('', 'UNK', 'F', 'M', 'O'),
    defaultValue = NA_character_,
    candidateOrigColNames = c('gender')
  ),
  Transmission = list(
    attribute = 'Transmission',
    description = 'Describes the most probable route of Transmission',
    type = 'character',
    values = c('', 'UNK', 'HAEMO', 'HETERO', 'IDU', 'MSM', 'MTCT', 'NOSO', 'TRANSFU'),
    defaultValue = NA_character_,
    candidateOrigColNames = c('transmission')
  ),
  FirstCD4Count = list(
    attribute = 'FirstCD4Count',
    description = 'CD4 cell count at time of diagnosis',
    type = 'numeric',
    defaultValue = NA_real_,
    candidateOrigColNames = c('cd4_num')
  ),
  LatestCD4Count = list(
    attribute = 'LatestCD4Count',
    description = 'Latest CD4',
    type = 'numeric',
    defaultValue = NA_character_,
    candidateOrigColNames = c('cd4latest')
  ),
  LatestVLCount = list(
    attribute = 'LatestVLCount',
    description = 'Latest viral load test count',
    type = 'numeric',
    defaultValue = NA_character_,
    candidateOrigColNames = c('vllatest')
  ),
  AcuteInfection = list(
    attribute = 'AcuteInfection',
    description = 'Acute infection type',
    type = 'character',
    defaultValue = NA_character_,
    candidateOrigColNames = c('acuteinfection')
  ),
  Art = list(
    attribute = 'Art',
    description = 'Indicator of antiretroviral therapy',
    type = 'character',
    defaultValue = NA_character_,
    candidateOrigColNames = c('art')
  ),
  HIVStatus = list(
    attribute = 'HIVStatus',
    description = 'Status of HIV',
    type = 'character',
    defaultValue = NA_character_,
    candidateOrigColNames = c('hivstatus')
  ),
  CountryOfBirth = list(
    attribute = 'CountryOfBirth',
    description = 'Country of birth of patient',
    type = 'character',
    values = union(c('', 'UNK'), countryData$Code),
    defaultValue = NA_character_,
    candidateOrigColNames = c('countryofbirth')
  ),
  CountryOfNationality = list(
    attribute = 'CountryOfNationality',
    description = 'Country of nationality of patient',
    type = 'character',
    values = union(c('', 'UNK'), countryData$Code),
    defaultValue = NA_character_,
    candidateOrigColNames = c('countryofnationality')
  ),
  RegionOfOrigin = list(
    attribute = 'RegionOfOrigin',
    description = 'Region of origin of patient',
    type = 'character',
    values = c(
      '', 'UNK', 'ABROAD', 'AUSTNZ', 'CAR', 'CENTEUR', 'EASTASIAPAC', 'EASTEUR', 'EUROPE', 'LATAM',
      'NORTHAFRMIDEAST', 'NORTHAM', 'REPCOUNTRY', 'SOUTHASIA', 'SUBAFR', 'WESTEUR'
    ),
    defaultValue = NA_character_,
    candidateOrigColNames = c('regionoforigin')
  ),
  PlaceOfNotification = list(
    attribute = 'PlaceOfNotification',
    description = 'Place of notification',
    type = 'character',
    defaultValue = NA_character_,
    candidateOrigColNames = c('placeofnotification')
  ),
  PlaceOfResidence = list(
    attribute = 'PlaceOfResidence',
    description = 'Place of residence',
    type = 'character',
    defaultValue = NA_character_,
    candidateOrigColNames = c('placeofresidence')
  ),
  DateOfNotification = list(
    attribute = 'DateOfNotification',
    description = 'Date of notification',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('dateofnotificationisodate')
  ),
  DateOfHIVDiagnosis = list(
    attribute = 'DateOfHIVDiagnosis',
    description = 'Date of HIV diagnosis',
    type = 'date',
    defaultValue = as.Date(NA),
    restrictedValues = as.Date(NA),
    candidateOrigColNames = c('dateofdiagnosisisodate')
  ),
  DateOfAIDSDiagnosis = list(
    attribute = 'DateOfAIDSDiagnosis',
    description = 'Date of AIDS diagnosis',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('dateofaidsdiagnosisisodate')
  ),
  DateOfFirstCD4Count = list(
    attribute = 'DateOfFirstCD4Count',
    description = 'Date of First CD4 cell count',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('firstcd4dateisodate')
  ),
  DateOfLatestCD4Count = list(
    attribute = 'DateOfLatestCD4Count',
    description = 'Date of latest CD4 count',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('cd4latestdateisodate')
  ),
  DateOfLatestVLCount = list(
    attribute = 'DateOfLatestVLCount',
    description = 'Date of latest viral load test',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('vllatestdateisodate')
  ),
  DateOfDeath = list(
    attribute = 'DateOfDeath',
    description = 'Date of death',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('dateofdeathisodate')
  ),
  DateOfArrival = list(
    attribute = 'DateOfArrival',
    description = 'Date of arrival',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('yearofarrivalisodate')
  ),
  DateOfArt = list(
    attribute = 'DateOfArt',
    description = 'Date of latest antiretroviral therapy',
    type = 'date',
    defaultValue = as.Date(NA),
    candidateOrigColNames = c('artdateisodate')
  )
)

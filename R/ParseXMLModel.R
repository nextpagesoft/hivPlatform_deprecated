#' ParseXMLModel
#'
#' Parse XML HIV model
#'
#' @param xml XML string
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' xml <- "
#' <?xml version='1.0' encoding='utf-8'?>
#'   <Model>
#'     <FileVersion>2</FileVersion>
#'     <Meta>
#'       <Name>Full data model</Name>
#'       <Author></Author>
#'       <Description></Description>
#'       <InputDataPath>../Data/test NL</InputDataPath>
#'       <OutputResultsPath>../Results/FullData</OutputResultsPath>
#'       <RiskGroups>
#'         <RiskGroup>
#'           <Name>pop_0</Name>
#'           <CreatedByDefault>true</CreatedByDefault>
#'           <RiskCategories>
#'             <RiskCategory>
#'               <Name>pop_0</Name>
#'               <IsSelected>true</IsSelected>
#'             </RiskCategory>
#'           </RiskCategories>
#'           <FitMinYear>1980</FitMinYear>
#'         </RiskGroup>
#'       </RiskGroups>
#'     </Meta>
#'     <IncidenceModel>
#'       <Run>true</Run>
#'       <MinYear>1980</MinYear>
#'       <MaxYear>2016</MaxYear>
#'       <MinFitPos>1979</MinFitPos>
#'       <MaxFitPos>1979</MaxFitPos>
#'       <MinFitCD4>1984</MinFitCD4>
#'       <MaxFitCD4>2016</MaxFitCD4>
#'       <MinFitAIDS>1980</MinFitAIDS>
#'       <MaxFitAIDS>1995</MaxFitAIDS>
#'       <MinFitHIVAIDS>1996</MinFitHIVAIDS>
#'       <MaxFitHIVAIDS>2016</MaxFitHIVAIDS>
#'       <DiagnosisRates>
#'         <Interval>
#'           <Description>Time interval 1</Description>
#'           <StartYear>1980</StartYear>
#'           <Jump>False</Jump>
#'           <ChangingInInterval>False</ChangingInInterval>
#'           <DifferentByCD4>False</DifferentByCD4>
#'         </Interval>
#'         <Interval>
#'           <Description>Time interval 2</Description>
#'           <StartYear>1984</StartYear>
#'           <Jump>True</Jump>
#'           <ChangingInInterval>False</ChangingInInterval>
#'           <DifferentByCD4>False</DifferentByCD4>
#'         </Interval>
#'         <Interval>
#'           <Description>Time interval 3</Description>
#'           <StartYear>1996</StartYear>
#'           <Jump>False</Jump>
#'           <ChangingInInterval>False</ChangingInInterval>
#'           <DifferentByCD4>False</DifferentByCD4>
#'         </Interval>
#'         <Interval>
#'           <Description>Time interval 4</Description>
#'           <StartYear>2000</StartYear>
#'           <Jump>False</Jump>
#'           <ChangingInInterval>False</ChangingInInterval>
#'           <DifferentByCD4>False</DifferentByCD4>
#'         </Interval>
#'         <Interval>
#'           <Description>Time interval 5</Description>
#'           <StartYear>2005</StartYear>
#'           <Jump>False</Jump>
#'           <ChangingInInterval>False</ChangingInInterval>
#'           <DifferentByCD4>False</DifferentByCD4>
#'         </Interval>
#'         <Interval>
#'           <Description>Time interval 6</Description>
#'           <StartYear>2010</StartYear>
#'           <Jump>False</Jump>
#'           <ChangingInInterval>False</ChangingInInterval>
#'           <DifferentByCD4>False</DifferentByCD4>
#'         </Interval>
#'       </DiagnosisRates>
#'       <Country>NL</Country>
#'       <KnotsCount>6</KnotsCount>
#'       <StartIncZero>true</StartIncZero>
#'       <DistributionFit>Negative binomial</DistributionFit>
#'       <RDisp>50</RDisp>
#'       <Delta4Fac>0</Delta4Fac>
#'       <MaxIncCorr>true</MaxIncCorr>
#'       <SplineType>B-splines</SplineType>
#'       <FullData>true</FullData>
#'       <Bootstrap>
#'         <IterCount>20</IterCount>
#'         <StartIter>0</StartIter>
#'       </Bootstrap>
#'       <LHS>
#'         <Run>false</Run>
#'         <StartIter>0</StartIter>
#'       </LHS>
#'     </IncidenceModel>
#'     <LondonModel>
#'       <Run>false</Run>
#'       <RunType1>true</RunType1>
#'       <RunType2>true</RunType2>
#'       <MinYear>1984</MinYear>
#'       <MaxYear>2016</MaxYear>
#'       <BootstrapIterCount>50000</BootstrapIterCount>
#'       <RateW>2</RateW>
#'     </LondonModel>
#'   </Model>
#' "
#' ParseXMLModel(xml)
#' }
#'
#' @export
ParseXMLModel <- function(
  xml
) {
  params <- xml2::as_list(xml2::read_xml(xml))

  timeIntervals <- lapply(unname(params$Model$IncidenceModel$DiagnosisRates), function(el) {
    list(
      startYear = as.integer(el$StartYear[[1]]),
      jump = as.logical(el$Jump[[1]]),
      changeInInterval = as.logical(el$ChangingInInterval[[1]]),
      diffByCD4 = as.logical(el$ChangingInInterval[[1]])
    )
  })

  timeIntervals <- lapply(seq_along(timeIntervals), function(i) {
    timeIntervals[[i]]$endYear <- ifelse(
      i < length(timeIntervals),
      timeIntervals[[i + 1]]$startYear,
      as.integer(params$Model$IncidenceModel$MaxYear[[1]])
    )
    return(timeIntervals[[i]])
  })

  settings <- list(
    minYear = as.integer(params$Model$IncidenceModel$MinYear[[1]]),
    maxYear = as.integer(params$Model$IncidenceModel$MaxYear[[1]]),
    minFitPos = as.integer(params$Model$IncidenceModel$MinFitPos[[1]]),
    maxFitPos = as.integer(params$Model$IncidenceModel$MaxFitPos[[1]]),
    minFitCD4 = as.integer(params$Model$IncidenceModel$MinFitCD4[[1]]),
    maxFitCD4 = as.integer(params$Model$IncidenceModel$MaxFitCD4[[1]]),
    minFitAIDS = as.integer(params$Model$IncidenceModel$MinFitAIDS[[1]]),
    maxFitAIDS = as.integer(params$Model$IncidenceModel$MaxFitAIDS[[1]]),
    minFitHIVAIDS = as.integer(params$Model$IncidenceModel$MinFitHIVAIDS[[1]]),
    maxFitHIVAIDS = as.integer(params$Model$IncidenceModel$MaxFitHIVAIDS[[1]]),
    country = params$Model$IncidenceModel$Country[[1]],
    knotsCount = as.integer(params$Model$IncidenceModel$KnotsCount[[1]]),
    startIncZero = as.logical(params$Model$IncidenceModel$StartIncZero[[1]]),
    distributionFit = ifelse(
      params$Model$IncidenceModel$DistributionFit[[1]] == 'Negative binomial',
      'NEGATIVE_BINOMIAL',
      'POISSON'
    ),
    rDisp = as.integer(params$Model$IncidenceModel$RDisp[[1]]),
    delta4Fac = as.integer(params$Model$IncidenceModel$Delta4Fac[[1]]),
    maxIncCorr = as.logical(params$Model$IncidenceModel$MaxIncCorr[[1]]),
    splineType = ifelse(
      params$Model$IncidenceModel$SplineType[[1]] == 'B-splines',
      'B-SPLINE',
      'M-SPLINE'
    ),
    fullData = as.logical(params$Model$IncidenceModel$FullData[[1]]),
    timeIntervals = timeIntervals
  )

  return(settings)
}

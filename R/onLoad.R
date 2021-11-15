.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler('OriginGroupingArray', ConvertOriginGroupingArray, force = TRUE)
  shiny::registerInputHandler('AttrMappingArray', ConvertAttrMappingArray, force = TRUE)
  shiny::registerInputHandler('AggrFilters', ConvertAggrFilters, force = TRUE)
  shiny::registerInputHandler('HIVModelParams', ConvertHIVModelParams, force = TRUE)

  invisible(NULL)
}

.onUnload <- function(libname, pkgname) {
  shiny::removeInputHandler('OriginGroupingArray')
  shiny::removeInputHandler('AttrMappingArray')
  shiny::removeInputHandler('AggrFilters')
  shiny::removeInputHandler('HIVModelParams')

  invisible(NULL)
}

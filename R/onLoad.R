.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler('OriginGroupingArray', ConvertOriginGroupingArray)
  shiny::registerInputHandler('AttrMappingArray', ConvertAttrMappingArray)
  shiny::registerInputHandler('HIVModelParams', ConvertHIVModelParams)

  invisible(NULL)
}

.onUnload <- function(libname, pkgname) {
  shiny::removeInputHandler('OriginGroupingArray')
  shiny::removeInputHandler('AttrMappingArray')
  shiny::removeInputHandler('HIVModelParams')

  invisible(NULL)
}

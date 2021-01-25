# Load data.table library manually. For some reason it is not loaded automatically on shinyapps
library(data.table)

# Load hivEstimatesAccuracy2 package
pkgload::load_all(path = '.', export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

options(shiny.maxRequestSize = 100 * 1024^2)
shiny::shinyApp(AppUI, AppServer)

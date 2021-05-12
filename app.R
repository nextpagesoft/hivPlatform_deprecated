# Manual load required on shiny server
library(data.table)

# Load hivPlatform package
pkgload::load_all(path = '.', export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

options(shiny.maxRequestSize = 100 * 1024^2)
shiny::shinyApp(AppUI, AppServer)

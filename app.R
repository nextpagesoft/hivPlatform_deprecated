# Manual load required on shiny server
library(data.table)

# Load hivPlatform package
try(pkgload::unload(), silent = TRUE)
pkgload::load_all(path = '.', export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

options(shiny.maxRequestSize = 100 * 1024^2)
shiny::shinyApp(AppUI, AppServer)

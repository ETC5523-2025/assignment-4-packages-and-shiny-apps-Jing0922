#' Launch my shiny app
#'
#' @export
#' @importFrom shiny runApp
run_app <- function() {
  appDir <- system.file("NitrateExplorer", package = "NitrateExplorer")

  shiny::runApp(appDir)
}


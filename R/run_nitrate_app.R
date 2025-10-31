#' Run the Nitrate Explorer Shiny Application
#'
#' Launches the Nitrate Explorer shiny application, which allows users to visualize and analyze Nitrate data.
#'
#' @returns None. This function runs the Shiny app.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_nitrate_app()
#' }
run_nitrate_app <- function() {
  appDir <- system.file("NitrateExplorer", package = "NitrateExplorer")

  shiny::runApp(appDir, display.mode = "normal")
}


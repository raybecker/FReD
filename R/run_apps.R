#' Run the Replication Explorer
#'
#' Running this function will launch the FReD Replication Explorer shiny app
#'
#' @return Replication Explorer shiny app
#' @param offer_install Should user be prompted to install required packages if they are missing?
#' @export
#' @examples
#' if (interactive()) {
#'   # To run the CReplication Explorer app:
#'   run_explorer()
#' }
run_explorer <- function(offer_install = interactive()) {

  # find and launch the app
  appDir <- system.file("fred_explorer", app, package = "fred")

  shiny::runApp(appDir, display.mode = "normal")
}

#' Run the Replication Annotator
#'
#' Running this function will launch the FReD Replication Annotator shiny app
#'
#' @return Replication Annotator shiny app
#' @param offer_install Should user be prompted to install required packages if they are missing?
#' @export
#' @examples
#' if (interactive()) {
#'   # To run the CReplication Annotator app:
#'   run_annotator()
#' }
run_annotator <- function(offer_install = interactive()) {

  # find and launch the app
  appDir <- system.file("fred_annotator", app, package = "fred")

  shiny::runApp(appDir, display.mode = "normal")
}

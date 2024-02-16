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

#' Get the date of last modification
#'
#' This function returns the date of last modification to be displayed in an app. It represents
#' either the date when a specific element of the app was last modified, or the date of the most
#' recent modification to the package DESCRIPTION file (which represents broader updates).
#'
#' @param app_folder The name of the app folder within the package (i.e. within the inst folder)

get_last_modified <- function(app_folder = "fred_explorer") {
  # Get the path to the app folder within the package
  appDirPath <- system.file(app_folder, package = "FReD")
  appDirPath <- normalizePath(appDirPath)

  # List all files in the directory recursively and get their last modified times
  files <- list.files(appDirPath, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    warning("No files found in the directory")
    most_recent_file_time <- 0
  } else {
    file_times <- lapply(files, function(f) file.info(f)$mtime)
    file_times_combined <- do.call(c, file_times)
    most_recent_file_time <- max(file_times_combined)
  }

  # Get the last modified time of the DESCRIPTION file
  descPath <- system.file("DESCRIPTION", package = "FReD")
  descPath <- normalizePath(descPath)
  desc_time <- file.info(descPath)$mtime

  # Find the most recent modification time between files and DESCRIPTION
  most_recent_time <- max(c(most_recent_file_time, desc_time))

  format(most_recent_time, "%d %B, %Y")
}


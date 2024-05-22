#' Run a shiny app within the package - potentially as an RStudio job
#'
#' This function is used to run the shiny apps within the package
#'
#' @return A shiny app
#' @param offer_install Should user be prompted to install required packages if they are missing?
#' @param app The name of the app folder within the package (i.e. within the inst folder)
#' @param in_background Should the app be run in the background (i.e. not block the R console)? Default to TRUE if RStudio is used.
#' @param port The port to run the app on (can usually be left at the default value)


run_app <- function(offer_install = interactive(), app = "fred_explorer", in_background = NULL, port = 3838) {
  assert_logical(in_background, null.ok = TRUE)

  if (is.null(in_background)) {
    in_background <- check_rstudio()
  } else if (in_background == TRUE) {
    in_background <- check_rstudio()
    if (!in_background) {
      message("Not running within RStudio (or `rstudioapi` package not available), so app will launch in R Console")
    }
  }

  # find and launch the app
  appDir <- system.file(app, package = "FReD")

  if (!in_background) {
    shiny::runApp(appDir, display.mode = "normal")
  } else {
    f <- tempfile(fileext = ".R")
    launch_code <- glue::glue("
          appDir <- '{appDir}'
if (appDir == '') {{
  stop('Could not find the app directory. Try re-installing FReD.', call. = FALSE)
}}
shiny::runApp(appDir = '{appDir}', display.mode = 'normal', quiet = TRUE, host = '127.0.0.1', port = {port})
                          ")
    writeLines(launch_code, f)

    eval(parse(text = glue::glue("invisible(rstudioapi::jobRunScript(path = '{f}', importEnv = TRUE, exportEnv = 'R_GlobalEnv', name = '{app} Shiny app'))")))
    message("Waiting for app launch")
    Sys.sleep(5)
    browseURL(url = glue::glue("http://127.0.0.1:{port}"))
    rstudioapi::executeCommand(commandId = "activateConsole")
    message("If the app does not show in your browser, try refreshing after 10-20 seconds - it can take a moment to download and prepare the dataset.")
  }
}


check_rstudio <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    if (rstudioapi::isAvailable()) {
      return(TRUE)
    }
  }
  FALSE
}

#' Run the Replication Explorer
#'
#' Running this function will launch the FReD Replication Explorer shiny app
#'
#' @return Replication Explorer shiny app
#' @inheritParams run_app
#' @export
#' @examples
#' if (interactive()) {
#'   # To run the Replication Explorer app:
#'   run_explorer()
#' }
run_explorer <- function(offer_install = interactive(), in_background = NULL, port = 3838) {
  run_app(offer_install = offer_install, app = "fred_explorer", in_background = in_background, port = port)
}


#' Run the Replication Annotator
#'
#' Running this function will launch the FReD Replication Annotator shiny app
#'
#' @return Replication Annotator shiny app
#' @inheritParams run_app
#' @export
#' @examples
#' if (interactive()) {
#'   # To run the Replication Annotator app:
#'   run_annotator()
#' }
run_annotator <- function(offer_install = interactive(), in_background = NULL, port = 3838) {
  run_app(offer_install = offer_install, app = "fred_annotator", in_background = in_background, port = port)
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

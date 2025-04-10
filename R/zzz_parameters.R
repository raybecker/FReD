#' Setting Parameters for the FReD Package
#'
#' Generally, the FReD package is designed to work with the latest FReD dataset.
#' However, you might want to use an older version, or even your own. For such advanced
#' use, you can set parameters by setting environment variables manually or in your `.Renviron` file.
#' The following environment variables can be set (before loading the package):
#'
#' - `FRED_DATA_URL`: The URL of the FReD dataset, needs to return the .xlsx file.
#' - `FRED_DATA_FILE`: The path to the .xlsx file, if you have downloaded it already (or want it to be saved to a particular location). If the file exists, it will be used - otherwise, the file will be downloaded and saved there.
#' - `RETRACTIONWATCH_DATA_FILE`: The path to the RetractionWatch database, if you have downloaded it already (or want it to be saved to a particular location). If the file exists, it will be used - otherwise, the file will be downloaded and saved there.
#' - `RETRACTIONWATCH_URL`: The URL to download the RetractionWatch database. Needs to return the .csv file.
#' - `FRED_OFFLINE`: Should FReD work offline (TRUE) or online (FALSE). If TRUE, FReD will not download the latest data every time it is loaded. Defaults to FALSE.
#'
#' @examples
#' ## Not run:
#' Sys.setenv(FRED_DATA_URL = "http://your_url")
#' ## End(Not run)
#' @name setting-parameters
NULL


.onLoad <- function(libname, pkgname) {
  parameters <- list(
    "FRED_DATA_URL" = "https://osf.io/z5u9b/download",
    "FRED_DATA_FILE" = tempfile(fileext = ".xlsx"),
    "RETRACTIONWATCH_DATA_FILE" = tempfile(fileext = ".csv"),
    "RETRACTIONWATCH_URL" = "https://api.labs.crossref.org/data/retractionwatch?lukas.wallrich@gmail.com",
    "FRED_OFFLINE" = FALSE
  )

  for (param in names(parameters)) {
    env_value <- Sys.getenv(param, unset = NA)

    if (!is.na(env_value)) {
      next
    } else {
      do.call(Sys.setenv, stats::setNames(list(as.character(parameters[[param]])), param))
    }
  }
}

# Function to get the current parameters
# Will also download FRED_DATA_FILE from FRED_DATA_URL if it does not exist
get_param <- function(param, auto_download = TRUE) {
  if (param == "FRED_DATA_FILE") {
    fred_file <- Sys.getenv("FRED_DATA_FILE")
    if (fred_file == "" || !file.exists(fred_file)) {
      if (auto_download) {
        tmp <- tempfile(fileext = ".xlsx")
        Sys.setenv("FRED_DATA_FILE" = tmp)
        download.file(Sys.getenv("FRED_DATA_URL"), tmp)
        fred_file <- tmp
      } else {
        stop("FRED_DATA_FILE does not exist. Please set FRED_DATA_FILE to the path of the FReD dataset.")
      }
    }
    return(fred_file)
  }

  if (param == "RETRACTIONWATCH_DATA_FILE") {
    rw_file <- Sys.getenv("RETRACTIONWATCH_DATA_FILE")
    if (rw_file == "" || !file.exists(rw_file)) {
      if (auto_download) {
        tmp <- tempfile(fileext = ".csv")
        Sys.setenv("RETRACTIONWATCH_DATA_FILE" = tmp)
        download.file(Sys.getenv("RETRACTIONWATCH_URL"), tmp)
        rw_file <- tmp
      } else {
        stop("RETRACTIONWATCH_DATA_FILE does not exist. Please set RETRACTIONWATCH_DATA_FILE to the path of the RetractionWatch database.")
      }
    }
    return(rw_file)
  }

  res <- Sys.getenv(param)
  if (res %in% c("TRUE", "FALSE")) res <- as.logical(res)
  if (res == "") message("Beware: ", param, " is not set.")
  return(res)
}

#' Set FReD to work offline (or back to online)
#'
#' By default, FReD loads the latest data and meta-data every time it is loaded.
#' If you work offline, or don't need the latest data, you may prefer to work from
#' cached data. For that, you can call 'use_FReD_offline()'. If you want this to
#' persist even when FReD is reloaded, you can use `Sys.setenv(FRED_OFFLINE = TRUE)`
#' (or FALSE) as needed, and include it in your `.Renviron` file to persist across
#' sessions.
#'
#' @param state Should FReD work offline (TRUE) or online (FALSE)
#' @export

use_FReD_offline <- function(state = TRUE) {
  assert_logical(state)
  Sys.setenv(FRED_OFFLINE = as.character(state))
}

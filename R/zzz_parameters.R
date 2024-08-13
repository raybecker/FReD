# Set cache for package, to avoid repeated function calls
.cache <- new.env()

#' Setting Parameters for the FReD Package
#'
#' Generally, the FReD package is designed to work with the latest FReD dataset.
#' However, you might want to use an older version, or even your own. For such advanced
#' use, you can set parameters, either using `Sys.setenv(FRED_DATA_URL = "http://your_url")`
#' or by setting `options(FRED_DATA_URL = "http://your_url")`. The following parameters
#' can be set (before loading the package):
#'
#' - `FRED_DATA_URL`: The URL of the FReD dataset, needs to return the .xlsx file.
#' - `FRED_DATA_FILE`: The path to the .xlsx file, if you have downloaded it already (or want it to be saved to a particular location). If the file exists, it will be used - otherwise, the file will be downloaded and saved there.
#'
#' @examples
#' Sys.setenv(FRED_DATA_URL = "http://your_url")
#' options(FRED_DATA_URL = "http://your_url")
#'
#' @name setting-parameters
NULL


.onLoad <- function(libname, pkgname) {
  parameters <- c("FRED_DATA_URL" = "https://osf.io/z5u9b/download",
                  "FRED_DATA_FILE" = tempfile(fileext = ".xlsx"),
                  "RETRACTIONWATCH_DATA_FILE" = tempfile(fileext = ".csv"),
                  "RETRACTIONWATCH_URL" = "https://api.labs.crossref.org/data/retractionwatch?lukas.wallrich@gmail.com")

    for (param in names(parameters)) {
      env_value <- Sys.getenv(param, unset = NA)

      if (!is.na(env_value) && nzchar(env_value)) {
        options(stats::setNames(list(env_value), param))
      } else {
        options(stats::setNames(list(parameters[param]), param))
      }
    }
}

# Function to get the current parameters
# Will also download FRED_DATA_FILE from FRED_DATA_URL if it does not exist
get_param <- function(param, auto_download = TRUE) {
  if (param == "FRED_DATA_FILE" && !file.exists(getOption("FRED_DATA_FILE"))) {
    if (auto_download) {
      download.file(getOption("FRED_DATA_URL"), getOption("FRED_DATA_FILE"))
    } else {
      stop("FRED_DATA_FILE does not exist. Please set FRED_DATA_FILE to the path of the FReD dataset.")
    }
  }
  if (param == "RETRACTIONWATCH_DATA_FILE" && !file.exists(getOption("RETRACTIONWATCH_DATA_FILE"))) {
    if (auto_download) {
      download.file(getOption("RETRACTIONWATCH_URL"), getOption("RETRACTIONWATCH_DATA_FILE"))
    } else {
      stop("RETRACTIONWATCH_DATA_FILE does not exist. Please set RETRACTIONWATCH_DATA_FILE to the path of the RetractionWatch database, or pass the download URL to the function.")
    }
  }
  res <- getOption(param)
  if (is.null(res)) message("Beware: ", param, " is not set.")
  return(res)
}


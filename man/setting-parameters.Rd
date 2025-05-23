% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz_parameters.R
\name{setting-parameters}
\alias{setting-parameters}
\title{Setting Parameters for the FReD Package}
\description{
Generally, the FReD package is designed to work with the latest FReD dataset.
However, you might want to use an older version, or even your own. For such advanced
use, you can set parameters by setting environment variables manually or in your \code{.Renviron} file.
The following environment variables can be set (before loading the package):
}
\details{
\itemize{
\item \code{FRED_DATA_URL}: The URL of the FReD dataset, needs to return the .xlsx file.
\item \code{FRED_DATA_FILE}: The path to the .xlsx file, if you have downloaded it already (or want it to be saved to a particular location). If the file exists, it will be used - otherwise, the file will be downloaded and saved there.
\item \code{RETRACTIONWATCH_DATA_FILE}: The path to the RetractionWatch database, if you have downloaded it already (or want it to be saved to a particular location). If the file exists, it will be used - otherwise, the file will be downloaded and saved there.
\item \code{RETRACTIONWATCH_URL}: The URL to download the RetractionWatch database. Needs to return the .csv file.
\item \code{FRED_OFFLINE}: Should FReD work offline (TRUE) or online (FALSE). If TRUE, FReD will not download the latest data every time it is loaded. Defaults to FALSE.
}
}
\examples{
## Not run:
Sys.setenv(FRED_DATA_URL = "http://your_url")
## End(Not run)
}

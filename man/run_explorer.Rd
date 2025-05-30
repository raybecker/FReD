% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_apps.R
\name{run_explorer}
\alias{run_explorer}
\title{Run the Replication Explorer}
\usage{
run_explorer(
  offer_install = interactive(),
  in_background = NULL,
  auto_close = interactive(),
  port = 3838,
  timeout = 30
)
}
\arguments{
\item{offer_install}{Should user be prompted to install required packages if they are missing?}

\item{in_background}{Should the app be run in the background (i.e. not block the R console)? Default to TRUE if RStudio is used.}

\item{auto_close}{Should the app be automatically ended when the browser is closed (or refreshed)?}

\item{port}{The port to run the app on (can usually be left at the default value)}

\item{timeout}{The timeout for waiting for the app to become available if launched in background (in seconds)}
}
\value{
Replication Explorer shiny app
}
\description{
Running this function will launch the FReD Replication Explorer shiny app
}
\examples{
if (interactive()) {
  # To run the Replication Explorer app:
  run_explorer()
}
}

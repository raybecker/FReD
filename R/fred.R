# Global imports - only for shiny
#' @import shiny
#' @import bslib
#' @import shinycssloaders
#' @import ggplot2
#' @import dplyr
#' @import checkmate


.check_req_packages <- function(x, note = "") {
  res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
  if (!all(res)) {
    if (!interactive()) {
      stop(note, "Some required packages are not installed. Make sure you have
               these packages: ", paste0(x[!res], collapse = ", "),
           call. = FALSE
      )
    }
    op <- options("warn")
    on.exit(options(op))
    options(warn = 1)
    warning(note, "The following packages are required for this function but
                   cannot be loaded: ", paste0(x[!res], collapse = ", "),
            call. = FALSE)
    choice <- readline(prompt = "Should I try to install these packages? (Y/N)")
    if (choice %in% c("Y", "y")) {
      utils::install.packages(x[!res])
      res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
      if (!all(res)) {
        stop("Not all packages could be installed successfully. The following could still not be loaded: ", paste0(x[!res], collapse = ", "),
             call. = FALSE
        )
      }
      return(TRUE)
    }
    stop("Cannot proceed without these packages.", call. = FALSE)
  }
}

#' Create FReD dataset citation
#'
#' Pulls current contributor list and dynamicalky creates a *markdown-formatted* citation for the FReD dataset.
#'
#' @param data_file Path to the FReD dataset, defaults to the current FReD dataset on OSF
#' @param cache Should the citation be returned from cache, if already requested during this session? Defaults to TRUE.
#' @return A markdown-formatted citation for the FReD dataset, including the current dataset version.

create_citation <- function(data_file = get_param("FRED_DATA_FILE"), cache = TRUE) {
  if (cache && exists("citation", .cache, inherits = FALSE)) {
    return(.cache$citation)
  }
  contributors <- openxlsx::read.xlsx(data_file, sheet = "Contributors FReD")
  contributors <- contributors[contributors$Added.to.FReD.website.as.contributor, ]
  contributors$first <- substr(contributors$First.name, 1, 1)
  contributors$middle <- ifelse(!is.na(contributors$Middle.name), paste(" ", substr(contributors$Middle.name, 1, 1), ".", sep = ""), "")
  contributors$apa <- paste0(contributors$Surname, ", ",
                             contributors$first, ".",
                             contributors$middle)
  c_names <- paste(contributors$apa, collapse = ", ")

  version <- get_dataset_changelog() %>% stringr::str_extract('(?<=\\*\\*Version:\\*\\* )\\d+\\.\\d+\\.\\d+')
  cit <- glue::glue("{c_names} (2024). _FReD: FORRT Replication Database, version {version}._ [https://dx.doi.org/10.17605/OSF.IO/9r62x] _*shared first authorship_")

  .cache$citation <- cit

  cit
  }

#' Get the dataset changelog from OSF
#'
#' Downloads and reads the changelog file from OSF, and returns it as a character string.
#'
#' @param changelog_file The URL of the changelog file on OSF
#' @param cache Should the changelog be returned from cache, if already requested during this session? Defaults to TRUE.

get_dataset_changelog <- function(changelog_file = "https://osf.io/fj3xc/download", cache = TRUE) {
  if (cache && exists("changelog", .cache, inherits = FALSE)) {
    return(.cache$changelog)
  }
  temp <- tempfile(fileext = ".md")
  download.file(changelog_file, temp)
  changelog <- readLines(temp, warn = FALSE) %>% paste(collapse = "\n")
  .cache$changelog <- changelog
  changelog
}

#' Load the FReD dataset
#'
#' This function loads the FReD dataset into R, and conducts variable transformations to  prepare for analyses.
#'
#' @param data Path to the FReD dataset (defaults to current FReD data on OSF)
#' @return A data frame with the processed FReD dataset
#' @export


load_fred_data <- function(data = get_param("FRED_DATA_FILE")) {

  read_fred() %>%
    clean_variables() %>%
    add_common_effect_sizes() %>%
    align_effect_direction() %>%
    add_uncertainty() %>%
    add_replication_power() %>%
    code_replication_outcomes() %>%
    augment_for_zcurve()
}



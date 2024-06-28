#' Load variable descriptions
#'
#' This reads names and variable descriptions of key variables from FReD. This can used for subsetting and describing the dataset.
#'
#' @param sheet_name Path to the variable descriptions
#' @param data Path to the FReD dataset (defaults to current FReD data on OSF)
#' @return A data frame with variable names (`Variable`) and descriptions (`Description`)


load_variable_descriptions <- function(sheet_name = "Key Variables", data = get_param("FRED_DATA_FILE")) {
  variable_descriptions <-  safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = sheet_name, startRow = 2)
  return(variable_descriptions)
}

#' Bind Rows with Character Columns
#'
#' A wrapper for `dplyr::bind_rows` that ensures any columns that are character
#' in one of the data frames are converted to character in all data frames before binding.
#' This reduces the likelihood of errors, but does not give up on type-checking entirely.
#'
#' @param ... Data frames to combine. Each argument should be a data frame.
#' @param .id An optional string that will be used to create a column in the output.
#' If supplied, this will create a new column with the name given by `.id`, and each
#' row will have a value corresponding to the argument name from which it came.
#'
#' @return A data frame created by binding the rows of the input data frames.
#' @keywords internal

bind_rows_with_characters <- function(..., .id = NULL) {
  dfs <- list(...)

  all_cols <- unique(unlist(lapply(dfs, colnames)))

  # Convert columns to character if any column in any dataframe is character
  dfs <- lapply(dfs, function(df) {
    for (col in all_cols) {
      if (col %in% colnames(df)) {
        if (any(sapply(dfs, function(x) col %in% colnames(x) && is.character(x[[col]])))) {
          df[[col]] <- as.character(df[[col]])
        }
      }
    }
    return(df)
  })
  dplyr::bind_rows(dfs, .id = .id)
}


#' Read the FReD dataset
#'
#' This function loads the FReD dataset into R. It merges the data from the different sheets into one data frame.
#'
#' @param data Path to the FReD dataset (defaults to current FReD data on OSF)
#' @return A data frame with the FReD dataset

read_fred <- function(data = get_param("FRED_DATA_FILE")) {

  red <- safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = "Data") # .xlsx file
  red <- red[-(1:2), ] # exclude labels and "X" column
  forrt  <- safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = "FORRT R&R (editable)", startRow = 1)
  forrt <- forrt[-(1:2), ] # exclude labels and "X" column
  forrt <- forrt[!(forrt$doi_original %in% red$doi_original), ] # exclude forrt entries of original study that already appear in FReD (based on DOIs)

  # additional studies
  as <-  safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = "Additional Studies to be added", startRow = 2)
  as$id <- paste("uncoded_studies_", rownames(as), sep = "")
  as <- as[as$`Study.listed.in.ReD?` != "1.0", ] # exclude additional studies that are already listed in the main dataset
  as <- as[!is.na(as$doi_original), ] # exclude studies for which doi_original is unavailable because they will not be finable in the annotator anyway

  numeric_variables <- c("n_original", "n_replication", "es_orig_value", "es_rep_value",
                         "validated", "published_rep", "same_design", "same_test",
                         "original_authors",
                         "significant_original", "significant_replication", "power",
                         "es_orig_RRR", "es_rep_RRR")

  red[, numeric_variables] <- sapply(red[ , numeric_variables], as.numeric)
  forrt[, numeric_variables] <- sapply(forrt[ , numeric_variables], as.numeric)

  # merge the data, aligning column types where one is character (as empty colums are imported as numeric)
  bind_rows_with_characters(red, forrt, as)
}

#' Clean variables
#' Perform some specific operations (e.g., recoding some NA as "") required to get the Shiny apps to work.
#' This may be a temporary solution, as much of it should likely be handled through validation in the data sheet, and at import time.

#' @param fred_data FReD dataset
#' @return FReD dataset with cleaned variables

clean_variables <- function(fred_data) {

  # recode variables for app to work
  fred_data$pc_tags <- NA
  fred_data$pc_contributors <- NA
  fred_data$description <- ifelse(is.na(fred_data$description), "", fred_data$description)
  fred_data$contributors <- ifelse(is.na(fred_data$contributors), fred_data$pc_contributors, fred_data$contributors)
  fred_data$tags <- ifelse(is.na(fred_data$tags), fred_data$pc_tags, fred_data$tags)
  fred_data$subjects <- NA
  fred_data$description <- ifelse(is.na(fred_data$description), fred_data$pc_title, fred_data$description)

  fred_data$closeness <- NA
  fred_data$result <- ifelse(fred_data$result == "0", NA, fred_data$result)

  # compute year the original study was published (match 1800-2099 only, and require consecutive numbers)
  fred_data$orig_year <- as.numeric(gsub(".*((18|19|20)\\d{2}).*", "\\1", fred_data$ref_original))

  # # delete duplicates and non-replication studies
  fred_data <- fred_data[fred_data$notes != "duplicate" | is.na(fred_data$notes), ] # ADDED: study exclusions due to duplicates
  fred_data <- fred_data[fred_data$notes != "No actual replication conducted" | is.na(fred_data$notes), ] # ADDED: some registrations had no corresponding replication study

  # remove entries with reasons for exclusions
  fred_data <- fred_data[is.na(fred_data$exclusion), ]

  # Collapse validated categories (# 2: error detected and corrected)
  fred_data$validated <- ifelse(fred_data$validated == 1 | fred_data$validated == 2, 1, fred_data$validated)

  fred_data

}


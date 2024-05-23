library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
if (FALSE) library(FReD)


if (!exists("create_citation")) {
  attach(getNamespace("FReD")) # To enable use of un-exported functions
}

if (!exists("create_citation")) stop("Failed to attach FReD namespace.")

df <- load_fred_data()

df_display <- df[, c("description", "es_original", "es_replication", "n_original", "n_replication", "osf_link", "contributors", "result", "result2", "ref_original", "ref_replication")]
df_display$es_original <- round(df_display$es_original, 3)
df_display$es_replication <- round(df_display$es_replication, 3)


dataset_variables <- load_variable_descriptions()

df$ref_original <- gsub("(.{70,}?)\\s", "\\1\n", df$ref_original) # line breaks

forestplotheight <- "17000px"

# WEBSITE TEXT --------------------------------------------------------------

source("website_text.R", local = TRUE) # Evaluate in calling environment, otherwise fails on app start

## Add custom theme (formatting)
custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#382f2f",
  primary = "#a62828",
  secondary = "#FF374B",
  base_font = "Calibri"
)




df <- read_fred() %>%
  clean_variables() %>%
  add_common_effect_sizes() %>%
  align_effect_direction() %>%
  add_uncertainty() %>%
  add_replication_power() %>%
  code_replication_outcomes() %>%
  augment_for_zcurve()

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



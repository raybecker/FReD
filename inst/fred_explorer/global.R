
df <- read_fred() %>%
  clean_variables() %>%
  add_common_effect_sizes() %>%
  align_effect_direction() %>%
  add_uncertainty() %>%
  add_replication_power() %>%
  code_replication_outcomes()



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



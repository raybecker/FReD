### TO DO
# - add data
# - add option to select findings and have the meta-analytical effect size calculated
# - add moderators
# - allow authors to have a second name listed in the Reference
# - repair tooltip for checker violin plot (=scatter plot); it seems that this doesn't refresh when changing the table input
# - modify summarizer so that computation of the REMLs doesn't take so long
# - discipline and effect need to be added in SoSCi
# - once the new ReD variables are set, the SoSci-Script (at least the output) needs to be corrected

source("helpers.R")

required_packages <- c(
  "shiny", "readxl", "shinycssloaders", "dplyr", "DT", "ggplot2",
  "forcats", "gridExtra", "reshape", "plotly", "httr",
  "metafor", "openxlsx", "plyr", "pwr", "psychometric", "zcurve", "bslib", "stringr",
  "rcrossref")

# Offer users to install missing packages or fail explicitly
.check_req_packages(required_packages)

# Also need plain library calls for rsconnect
library(shiny)
library(readxl)
library(shinycssloaders)
library(dplyr)
library(DT)
library(ggplot2)
library(forcats)
library(gridExtra)
#library(ggpubr)
library(reshape)
library(plotly)
library(httr)
library(metafor)
library(openxlsx)
library(plyr)
library(pwr)
library(psychometric)
library(zcurve)
library(bslib)
library(stringr)
library(rcrossref) # not needed here but listed so that it gets cited (was used to get DOIs for all original studies and some replication studies)


# BASIC INFO --------------------------------------------------------------

version <- "Version 0.4.6"
date <- "09 February, 2024" # enter last update here
forestplotheight <- "17000px" # make it so that forest plot is readable
red_link <- "https://osf.io/z5u9b/download"

source("changelog.R", local = TRUE) # Evaluate in calling environment, otherwise may fail on app start

### Variable explanations
variables <- c("description"
               , "es_original"
               , "es_replication"
               , "n_original"
               , "n_replication"
               , "osf_link"
               , "contributors"
               , "result"
               , "ref_original
               ", "ref_replication"
               , "tags"
               , "notes")

explanations <- c("Short description of the main phenomenon/hypothesis of the replication study"
                  , "Original effect size converted to Bravais Pearson correlation"
                  , "Replication effect size converted to Bravais Pearson correlation"
                  , "Original sample size"
                  , "Replication sample size"
                  , "Link to the OSF project or post-completion results report"
                  , "Contributors of the replication study (first name, last name; separated by comma)"
                  , "Result of the replication study (informative failure to replicate, informative success to replciate, or inconclusive"
                  , "Full APA7 reference to the report describing the original study (including study number if necessary)"
                  , "Full APA7 reference to the report describing the replication study (if possible; including study number if necessary)"
                  , "Tags to make the study easily findable"
                  , "Any further notes")

dataset_variables <- data.frame("Variable" = variables, "Description" = explanations)



# DATA --------------------------------------------------------------------

## Static dataset ----------------------------------------------------------

message("Loading data")
data_file <- tempfile(fileext = ".xlsx")
download.file(red_link, data_file)

# Open processed dataset
red <- openxlsx::read.xlsx(data_file, sheet = "Data") # .xlsx file

red <- red[-(1:2), ] # exclude labels and "X" column

# additional studies
as <- openxlsx::read.xlsx(data_file, sheet = "Additional Studies to be added", startRow = 2)
as$id <- paste("uncoded_studies_", rownames(as), sep = "")
forrt  <- openxlsx::read.xlsx(data_file, sheet = "FORRT R&R (editable)", startRow = 1)
forrt <- forrt[-(1:2), ] # exclude labels and "X" column

message("Loaded data")

## Submissions ------------------------------------------------------
# ### comment this in to have data from the submission portal downloaded directly into the app; note that some of the code is deprecated and should be replaced by code from soscisubmissions.R
# link <- "https://www.soscisurvey.de/replicate/?act=BhquqCVmVnqokhQEVCCaAfrS"
# rd <- read.csv(link, sep = "\t", encoding = "latin1", na.strings = "-9")[1:4, ] # only read first 4 lines so that the app cannot be broken by r > 1 or similar entries XXX
#
# # rd[rd == -9] <- NA # replace -9 values with NAs
#
# rd <- rd[!is.na(rd$EN02_05), ] # remove empty entries
# rd <- rd[rd$STARTED != "2023-03-10 14:06:34", ]
#
# rd$n_original    <- as.numeric(rd$EN02_05)
# rd$n_replication <- as.numeric(rd$EN02_06)
# rd$ref_original    <- rd$EN01_01
# rd$ref_replication <- rd$EN01_02
# rd$es_orig_value <- as.numeric(as.character(gsub("\'", "", rd$EN02_01)))
# rd$es_rep_value <-  as.numeric(as.character(gsub("\'", "", rd$EN02_02)))
# rd$es_orig_estype <- rd$EN02_03
# rd$es_rep_estype <-  rd$EN02_04
# rd$published_rep <- rd$EN03-1
# rd$validated <- NA
# rd$validated_person <- ""
# rd$description <- rd$EN04_01
# rd$contributors <- rd$EN05_01
# rd$result <- dplyr::recode(rd$EN08, "1" = "success", "2" = "informative failure to replicate", "3" = "practical failure to replicate", "4" = "inconclusive")
# rd$id_sample <- ifelse(rd$EN09 == "1", paste("sosci_", row_number(rd$EN09), sep = "")
#                        , paste("sosci_", row_number(rd$EN09), "_sample", gsub(" ", ".", rd$EN09_02), sep = ""))
# rd$id <- rd$id_sample
# rd$osf_link <- rd$EN10_01
# rd$notes <- rd$EN11_01
# rd$same_design <- as.numeric(dplyr::recode(rd$EN12_01, "1" = "0", "2" = "1"))
# rd$same_test <-   as.numeric(dplyr::recode(rd$EN12_02, "1" = "0", "2" = "1"))
# rd$original_authors <- as.numeric(dplyr::recode(rd$EN14, "1" = "0", "2" = "1"))
# rd$tags <- gsub(", ,", "", paste(rd$EN06_01 # put tags behind each other and avoid empty tags (", ,")
#                                  , rd$EN06_02
#                                  , rd$EN06_03
#                                  , rd$EN06_04
#                                  , rd$EN06_05
#                                  , sep = ", "))
# rd$date_entered <- rd$LASTDATA
# rd$preregistration <- rd$EN13_01
# rd$closeness_instructions <- rd$EN27_01
# rd$closeness_measures <- rd$EN27_02
# rd$closeness_stimuli <- rd$EN27_03
# rd$closeness_procedure <- rd$EN27_04
# rd$closeness_location <- rd$EN27_05
# rd$closeness_renumeration <- rd$EN27_06
# rd$closeness_participants <- rd$EN27_07
# rd$closeness_exclusions <- rd$EN27_08
# rd$closeness_language <- rd$EN27_09
# rd$closeness_nationality <- rd$EN27_10
# rd$differences <- rd$EN28_01
# rd$source <- "Individual submissions"
#
# # remove cryptic variables
# rd <- rd[ , 46:ncol(rd)]
#
# numeric_variables <- c("n_original"
#                        , "n_replication"
#                        , "es_orig_value"
#                        , "es_rep_value"
#                        , "validated"
#                        , "published_rep"
#                        , "same_design"
#                        , "same_test"
#                        , "original_authors"
#                        # , "es_original"
#                        # , "es_replication"
#                        # , "ci.lower_original"
#                        # , "ci.upper_original"
#                        # , "ci.lower_replication"
#                        # , "ci.upper_replication"
#                        # , "significant_original"
#                        # , "significant_replication"
#                        # , "power"
#                        # , "es_orig"
#                        # , "es_rep"
#                        # , "es_orig_RRR"
#                        # , "es_rep_RRR"
# )
#
# rd[, numeric_variables] <- sapply(rd[ , numeric_variables], as.numeric)
#
#
#
#
#
# ## Merge datasets ----------------------------------------------------------
#
# red <- base::merge(rd, red, all = TRUE)

numeric_variables <- c("n_original"
                       , "n_replication"
                       , "es_orig_value"
                       , "es_rep_value"
                       , "validated"
                       , "published_rep"
                       , "same_design"
                       , "same_test"
                       , "original_authors"
                       , "es_original"
                       , "es_replication"
                       , "ci.lower_original"
                       , "ci.upper_original"
                       , "ci.lower_replication"
                       , "ci.upper_replication"
                       , "significant_original"
                       , "significant_replication"
                       , "power"
                       # , "es_orig"
                       # , "es_rep"
                       , "es_orig_RRR"
                       , "es_rep_RRR"
)

red[, numeric_variables] <- sapply(red[ , numeric_variables], as.numeric)

# exclusions
red <- red[is.na(red$exclusion), ] # remove entries with reasons for exclusions




## Compute dataset values --------------------------------------------------



# remove effect size types for which conversion is not integrated yet
# red <- red[tolower(red$es_orig_estype) %in% (c("or", "r", "η²" ,"r²", "d")), ]
# red <- red[!is.na(red$es_orig_estype)]
#
# red <- red[tolower(red$es_orig_estype) %in% (c("or", "r", "η²" ,"r²", "d")), ]
# red <- red[!is.na(red$es_orig_estype)]


## convert effect sizes
# original
red[!is.na(red$es_orig_estype) & tolower(red$es_orig_estype) == "or", "es_original"]   <- try(esc::pearsons_r(or =  red[!is.na(red$es_orig_estype) & red$es_orig_estype == "OR", "es_orig_value"]), silent = TRUE)
red[!is.na(red$es_orig_estype) & tolower(red$es_orig_estype) == "d", "es_original"]    <- try(esc::pearsons_r(d =   red[!is.na(red$es_orig_estype) & red$es_orig_estype == "d", "es_orig_value"]), silent = TRUE)
red[!is.na(red$es_orig_estype) & tolower(red$es_orig_estype) == "η²", "es_original"]   <- try(esc::pearsons_r(eta = red[!is.na(red$es_orig_estype) & red$es_orig_estype == "η²", "es_orig_value"]), silent = TRUE)
red[!is.na(red$es_orig_estype) & tolower(red$es_orig_estype) == "etasq", "es_original"]   <- try(esc::pearsons_r(eta = red[!is.na(red$es_orig_estype) & red$es_orig_estype == "etasq", "es_orig_value"]), silent = TRUE)
red[!is.na(red$es_orig_estype) & tolower(red$es_orig_estype) == "r", "es_original"]    <- try(                      red[!is.na(red$es_orig_estype) & red$es_orig_estype == "r", "es_orig_value"], silent = TRUE)
red[!is.na(red$es_orig_estype) & tolower(red$es_orig_estype) == "r²", "es_original"]   <- try(sqrt(                 red[!is.na(red$es_orig_estype) & red$es_orig_estype == "r²", "es_orig_value"]), silent = TRUE)

# replication
red[!is.na(red$es_rep_estype) & tolower(red$es_rep_estype) == "or", "es_replication"] <- try(esc::pearsons_r(or =   red[!is.na(red$es_rep_estype) & red$es_rep_estype == "OR", "es_rep_value"]), silent = TRUE)
red[!is.na(red$es_rep_estype) & tolower(red$es_rep_estype) == "d", "es_replication"]  <- try(esc::pearsons_r(d =    red[!is.na(red$es_rep_estype) & red$es_rep_estype == "d", "es_rep_value"]), silent = TRUE)
red[!is.na(red$es_rep_estype) & tolower(red$es_rep_estype) == "η²", "es_replication"] <- try(esc::pearsons_r(eta =  red[!is.na(red$es_rep_estype) & red$es_rep_estype == "η²", "es_rep_value"]), silent = TRUE)
red[!is.na(red$es_rep_estype) & tolower(red$es_rep_estype) == "etasq", "es_replication"] <- try(esc::pearsons_r(eta =  red[!is.na(red$es_rep_estype) & red$es_rep_estype == "etasq", "es_rep_value"]), silent = TRUE)
red[!is.na(red$es_rep_estype) & tolower(red$es_rep_estype) == "r", "es_replication"]  <- try(                       red[!is.na(red$es_rep_estype) & red$es_rep_estype == "r", "es_rep_value"], silent = TRUE)
red[!is.na(red$es_rep_estype) & tolower(red$es_rep_estype) == "r²", "es_replication"] <- try(sqrt(                  red[!is.na(red$es_rep_estype) & red$es_rep_estype == "R²", "es_rep_value"]), silent = TRUE)

## compute effect sizes from test statistics
# # original, f
# red[!is.na(red$teststatistic_orig) & is.na(red$es_original) & tolower(substr(red$teststatistic_orig,1 ,1)) == "f", "es_original"] <- try(esc::esc_f(f =
#                  as.numeric(sub(".*=", "\\1", red[!is.na(red$teststatistic_orig) & is.na(red$es_original) & tolower(substr(red$teststatistic_orig,1 ,1)) == "f", "teststatistic_orig"]))
#                   , totaln = as.numeric(red[!is.na(red$teststatistic_orig) & is.na(red$es_original) & tolower(substr(red$teststatistic_orig,1 ,1)) == "f", "n_original"]), es.type = "r"), silent = TRUE)
#
# # replication f
# red[!is.na(red$teststatistic_rep) & is.na(red$es_replication) & tolower(substr(red$teststatistic_rep,1 ,1)) == "f", "es_replication"] <- try(esc::esc_f(f =
#
# # original, t
# red[!is.na(red$teststatistic_orig) & is.na(red$es_original) & tolower(substr(red$teststatistic_orig,1 ,1)) == "t", "es_original"] <- try(esc::esc_t(t = as.numeric(sub(".*=", "\\1", red[!is.na(red$teststatistic_orig) & is.na(red$es_original) & tolower(substr(red$teststatistic_orig,1 ,1)) == "t", "teststatistic_orig"])), totaln = as.numeric(red[!is.na(red$teststatistic_orig) & is.na(red$es_original) & tolower(substr(red$teststatistic_orig,1 ,1)) == "t", "n_original"]), es.type = "r"), silent = TRUE)
# # replication t
# red[!is.na(red$teststatistic_rep) & is.na(red$es_replication) & tolower(substr(red$teststatistic_rep,1 ,1)) == "t", "es_replication"] <- try(esc::esc_t(t = as.numeric(sub(".*=", "\\1", red[!is.na(red$teststatistic_rep) & is.na(red$es_replication) & tolower(substr(red$teststatistic_rep,1 ,1)) == "t", "teststatistic_rep"])), totaln = as.numeric(red[!is.na(red$teststatistic_rep) & is.na(red$es_replication) & tolower(substr(red$teststatistic_rep,1 ,1)) == "t", "n_replication"]), es.type = "r"), silent = TRUE)



# windsorize values
red$es_original <- ifelse(red$es_original > 1, 1, ifelse(red$es_original < -1, -1, red$es_original))
red$es_replication <- ifelse(red$es_replication > 1, 1, ifelse(red$es_replication < -1, -1, red$es_replication))

# compute standared error for correlations
red$vi_orig <- metafor::escalc(ri = red$es_original, ni = red$n_original, measure = "COR")$vi
red$vi_rep <- metafor::escalc(ri = red$es_replication, ni = red$n_replication, measure = "COR")$vi



# recode variables for app to work
red$pc_tags <- NA
red$pc_contributors <- NA
red$description <- ifelse(is.na(red$description), "", red$description)
red$contributors <- ifelse(is.na(red$contributors), red$pc_contributors, red$contributors)
red$tags <- ifelse(is.na(red$tags), red$pc_tags, red$tags)
red$subjects <- NA
red$description <- ifelse(is.na(red$description), red$pc_title, red$description)

red$closeness <- NA
red$result <- ifelse(red$result == "0", NA, red$result)


# make it so that original effects are always positive and replication effects are positive if they are in the same direction as the original finding
red$es_replication <- ifelse(red$es_original < 0, red$es_replication * -1, red$es_replication)
red$es_original <- abs(red$es_original)

# compute year the original study was published
red$orig_year <- as.numeric(substr(gsub("\\D", "", red$ref_original), 1, 4))
red$orig_year <- ifelse(red$orig_year > 2100, NA, red$orig_year)

# # delete duplicates and non-replication studies
red <- red[red$notes != "duplicate" | is.na(red$notes), ] # ADDED: study exclusions due to duplicates
red <- red[red$notes != "No actual replication conducted" | is.na(red$notes), ] # ADDED: some registrations had no corresponding replication study



# compute CIs
for (i in 1:nrow(red)) {

  # original effect
  ci_original <- psychometric::CIr(r = as.numeric(red[i, "es_original"]), n = as.numeric(red[i, "n_original"]))
  red[i, "ci.lower_original"] <- ci_original[1]
  red[i, "ci.upper_original"] <- ci_original[2]

  # replication effect
  ci_replication <- psychometric::CIr(r = as.numeric(red[i, "es_replication"]), n = as.numeric(red[i, "n_replication"]))
  red[i, "ci.lower_replication"] <- ci_replication[1]
  red[i, "ci.upper_replication"] <- ci_replication[2]
}

red$significant_original <-    as.factor(ifelse(red$ci.lower_original > 0 | red$ci.upper_original < 0, "1", "0"))
red$significant_replication <- as.factor(ifelse(red$ci.lower_replication > 0 | red$ci.upper_replication < 0, "1", "0"))

for (i in 1:nrow(red)) {
  if ( !is.na(red[i, "es_original"]) & !is.na(red[i, "n_replication"]) ) {
    red[i, "power"] <- pwr::pwr.r.test(n = red[i, "n_replication"], r = red[i, "es_original"]
                                       , power = NULL, alternative = "greater")$power
  }
}

red$power <- round(red$power, digits = 3)


## validate entries
red$validated <- ifelse(red$validated == 1 | red$validated == 2, 1, red$validated) # 2: error detected and corrected



### code result with alternative terminology by Lebel et al., 2018; https://etiennelebel.com/documents/lebeletal%282018,ampss%29a-unified-framework-to-quantify-the-credibility-of-scientific-findings.pdf
# signal / no signal
red$signal <- ifelse(red$significant_replication == 1, "signal", "no signal")
red$consistent <- NULL
# # consistent / smaller / larger
# # is.na(red$significant_original) | is.na(red$significant_replication) | red$significant_original == 0 # NA
# # red$ci.upper_replication > red$es_original & red$ci.lower_replication < red$es_original # consistent
# # red$signal == "signal" & red$ci.lower_replication > red$es_original # inconsistent, larger
# # red$signal == "signal" & red$ci.upper_replication < red$es_original  # inconsistent, smaller
# # red$signal == "signal" & red$es_replication < 0  # inconsistent, opposite
# # red$signal == "no signal" & red$ci.upper_replication > red$es_original  # consistent
# # red$signal == "no signal" & red$significant_replication == 0  # inconsistent
#
# # is.na(red$significant_original) | is.na(red$significant_replication) | red$significant_original == 0 ~ "OS n.s."
# # , red$ci.upper_replication > red$es_original & red$ci.lower_replication < red$es_original ~ "consistent"
# # , red$signal == "signal" & red$ci.lower_replication > red$es_original ~ "inconsistent, larger"
# # , red$signal == "signal" & red$ci.upper_replication < red$es_original ~ "inconsistent, smaller"
# # , red$signal == "signal" & red$es_replication < 0  ~ "inconsistent, opposite"
# # , red$signal == "no signal" & red$ci.upper_replication > red$es_original ~ "consistent"
# # , red$signal == "no signal" & red$significant_replication == 0 ~ "inconsistent"



red <- dplyr::mutate(red, consistent = case_when(
  is.na(red$significant_original) | is.na(red$significant_replication) | red$significant_original == 0 ~ "OS n.s."
  , red$ci.upper_replication > red$es_original & red$ci.lower_replication < red$es_original ~ "consistent"
  , red$signal == "signal" & red$ci.lower_replication > red$es_original ~ "inconsistent, larger"
  , red$signal == "signal" & red$ci.upper_replication < red$es_original ~ "inconsistent, smaller"
  , red$signal == "signal" & red$es_replication < 0  ~ "inconsistent, opposite"
  , red$signal == "no signal" & red$ci.upper_replication > red$es_original ~ "consistent"
  , red$signal == "no signal" & red$significant_replication == 0 ~ "inconsistent"
))



red$result <- ifelse(as.numeric(red$significant_original) == 1 & as.numeric(red$significant_replication) == 1, "success", red$result)


red$result2 <- paste(red$signal, red$consistent, sep = " - ")
table(red$result2)


### Values for Z-Curve analysis
## compute standard error
red$se <- sqrt((1-abs(red$es_original)^2)/(red$n_original-2))

## compute z-score
red$z <- abs(red$es_original/red$se)


# create datasets for filtering and display
red_temp <- red
red_display <- red[, c("description", "es_original", "es_replication", "n_original", "n_replication", "osf_link", "contributors", "result", "result2", "ref_original", "ref_replication")]
red_display$es_original <- round(red_display$es_original, 3)
red_display$es_replication <- round(red_display$es_replication, 3)

red$ref_original <- gsub("(.{70,}?)\\s", "\\1\n", red$ref_original) # line breaks


# WEBSITE TEXT --------------------------------------------------------------

message("Source text")
source("website_text.R", local = TRUE) # Evaluate in calling environment, otherwise fails on app start

message("Sourced text")

## Add custom theme (formatting)
custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#382f2f",
  primary = "#a62828",
  secondary = "#FF374B",
  base_font = "Calibri"
)



# APP ----------------------------------------------------------------------


ui <- fluidPage(
  theme = custom_theme
  , navbarPage(title = ""
               , tabPanel(img(src = "fred.png", height = 67/2.5, width = 715/2.5), fluidRow(
                 column(6, info)
               ))
               , tabPanel("Replicability Tracker"
                          , fluidRow(
                            sidebarPanel(
                              #   textInput("titles", "Titles (e.g., Heat-Priming):")
                              # , textInput("tags", "Tags (e.g., anchoring):")
                              # , textInput("contributors", "Contributors (e.g., Smith):")
                              sliderInput("minpower", "Minimum Power", min = .05, max = .999, value = .05)
                              , selectInput("source", "Browse Large-Scale Projects:"
                                            , choices = c("All studies" = "All studies"
                                                          , "Individual submissions" = "Individual submissions"
                                                          , "FORRT Replications and Reversals" = "FORRT"
                                                          , "CORE (Feldman JDM Replications)" = "CORE"
                                                          , "CRSP special issue"
                                                          , "Data Replicada" =  "datareplicada"
                                                          , "Many Labs 1" =  "ML1"
                                                          , "Many Labs 2" =  "ML2"
                                                          , "Many Labs 3" =  "ML3"
                                                          , "OpAQ (Anchoring Effects)" = "OpAQ"
                                                          , "OpenMKT.org Replications Catalog" =  "openmkt"
                                                          , "OSF Registries" = "OSF Registries"
                                                          , "Reproducibility Project Psychology (OSC, 2015)" = "OSC 2015"
                                                          , "RRR1 (verbal overshadowing)" = "RRR1"
                                                          # , "RRR2 (X)" = "RRR2"
                                                          , "RRR3 (grammar on intentionality effect)" = "RRR3"
                                                          , "RRR4 (ego depletion)" = "RRR4"
                                                          , "RRR8 (professor priming)" = "RRR8"
                                                          , "RRR9 (hostility priming)" = "RRR9"
                                                          , "Soto (Big5 Correlations)" = "Soto"
                                                          , "SSRP (Camerer et al., 2018)" = "SSRP"
                                            )
                                            , selected = "All studies")
                              , checkboxInput("validated", "Show validated entries only", value = TRUE)
                              , checkboxInput("codedentries", "Show coded entries only", value = TRUE)
                              , width = 2)
                            , column(8
                                     , dataset_info
                                     # Red-and-green Barplot
                                     , withSpinner(plotly::plotlyOutput("barplot", width = "100%", height = "250px"))
                                     # Table for filtering
                                     , withSpinner(DT::DTOutput("table"))
                                     # RPP-Scatterplot
                                     , scatterplot_title
                                     , withSpinner(plotly::plotlyOutput("overviewplot", width = "100%", height = 800))
                                     , scatterplot_explanation
                                     # Barplot 2
                                     , barplot2_title
                                     , withSpinner(plotly::plotlyOutput("barplot2", width = "100%", height = "250px"))
                                     , barplot2_explanation
                                     # Z-Curve
                                     , breaks
                                     , zcurve_title, breaks
                                     , withSpinner(shiny::plotOutput("zcurve_plot"))
                                     , zcurve_explanation, breaks
                                     # , withSpinner(shiny::tableOutput("overview")), dataset_info1b
                            )))


               , tabPanel("Study Overview"
                          , fluidRow(
                            column(8, forest_info, withSpinner(plotly::plotlyOutput("forestplot", width = "100%", height = forestplotheight)))
                            # , withSpinner(shiny::tableOutput("overview")), dataset_info1b
                          ))

               , tabPanel("Dataset", dataset_explanation, breaks
                          , downloadButton("reddownload", label = "Download dataset")
                          , breaks
                          , dataset_headline
                          , withSpinner(DT::DTOutput("dataset"))
                          , breaks
                          , variables_headline
                          , withSpinner(DT::DTOutput("variables"))
               )

               , tabPanel("Correlates of Replicability"
                          , correlates_info
                          , correlates_decade
                          , withSpinner(plotly::plotlyOutput("correlate_decade"))
                          , correlates_journal
                          , withSpinner(plotly::plotlyOutput("correlate_journal", width = "100%", height = "2000px"))
               )

               , tabPanel("Moderators [alpha]"
                          , moderators_info
                          , shiny::selectInput("moderator", label = "Moderators"
                                               , choices = list(
                                                 "Original Effect Size" = "es_original"
                                                 , "Journal" = "orig_journal"
                                                 , "Year of Original Publication" = "orig_year"
                                                 , "Power of Replication Study" = "power"
                                               ))
                          , fluidRow(withSpinner(plotly::plotlyOutput("flexibleplot", width = "100%", height = 600))
                          )
                          , fluidRow(
                            column(6, withSpinner(DT::DTOutput("flexiblemodtable")))
                            , column(6, withSpinner(shiny::htmlOutput("flexiblemoderatortext")))
                          )
               )

               # , tabPanel("Summarizer [alpha]"
               #            , fluidRow(
               #                    column(12
               #                           , checker_info
               #                           , withSpinner(DT::DTOutput("checkertable"))
               #                           , withSpinner(DT::DTOutput("flexiblecheckertable"))
               #                           , withSpinner(shiny::htmlOutput("flexiblesummarizertext"))
               #                    ), column(6
               #
               #                           , withSpinner(plotly::plotlyOutput("checker_violin", width = "100%", height = "400px"))
               #                       )
               #                       , column(6
               #                           # , withSpinner(plotly::plotlyOutput("checker_maplot")
               #                           , withSpinner(plotly::plotlyOutput("checker_bar")
               #                                 # , checker_matable
               #                                )
               #                       )
               #            ))

               , tabPanel("References-Checker [alpha]"
                          , rc_info
                          , textAreaInput("refcheck", "References"
                                          , value = "Judge, T. A., & Bono, J. E. (2000). Five-factor model of personality and transformational leadership. Journal of Applied Psychology, 85, 751-765. 10.1037/0021-9010.85.5.751"
                                          , width = "1000px"
                                          , height = "200px"
                                          , placeholder = NULL
                          )
                          , withSpinner(plotly::plotlyOutput("references_barplot"))
                          , withSpinner(tableOutput("references_doi"))
               )

               , tabPanel("References"
                          , references_headline, references_list
                          , references_redpublications, references_list_redpublications
                          , packages_headline, packages_list
               )

               , tabPanel("FAQ"
                          , faqs
               )

               , tabPanel("About"
                          , about, breaks
                          , img(src = "ub.png", height  = 100)
                          , img(src = "um.png", height  = 50)
                          , img(src = "nwo.png", height = 100)
                          , breaks
                          , breaks
                          , changelog
                          , img(src = "fred.png", height = 80)
                          # , breaks
                          # , img(src = "FORRT.svg", height = 100)
                          , tags$style(HTML("
                                .navbar-default .navbar-brand {color:black;}
        .navbar-default .navbar-brand:hover {color:black;}
        .navbar { background-color:#EAEAEA;}
        .navbar-default .navbar-nav > li > a {color: dark grey;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color:black;background-color:#fc2d2d;}
        .navbar-default .navbar-nav > li > a:hover {color:black;background-color:#A6A6A6;text-decoration}
                               "))



               )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {


  # Overview Table ----------------------------------------------------------


  output$table <- DT::renderDT(server = FALSE, {

    ## apply filters
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    red_temp <- red_temp[red_temp$power >= input$minpower, ]

    # source
    if (input$source == "All studies") {
      red_temp <- red_temp
    } else {
      red_temp <- red_temp[red_temp$source == input$source, ]
    }

    # validated
    if (input$validated == TRUE) {
      red_temp <- red_temp[!is.na(red_temp$validated), ]
    }

    # only show show coded entries?
    if (input$codedentries == TRUE) {
      red_temp <- red_temp[!is.na(red_temp$result), ]
    }
    red_temp[is.na(red_temp$result), "result"] <- "not coded"

    # exclude NAs
    # red_temp <- red_temp[!is.na(red_temp$result), ]


    # red_temp_filtered <- red_temp[, c("description", "n_original", "n_replication", "power", "result")]
    red_temp_filtered <- red_temp[, c("description", "tags", "contributors"
                                      # , "es_original", "es_replication"
                                      , "result", "ref_original", "ref_replication")]

    DT::datatable(
      red_temp_filtered
      , extensions = "Buttons"
      , selection = "none"
      , options = list(scrollX = TRUE
                       , dom = "Bfrtip"
                       , buttons = c('copy', 'csv', 'excel')
                       , pageLength = 5
                       # , lengthMenu = c(5, 10, 100) # XXX not working yet
      ), rownames = FALSE
      # , options = list(pageLength = 5)
      # , rownames = FALSE
    )

    # DT::datatable(
    #   red_temp[, c("description", "tags", "result", "ref_original", "ref_replication")]
    #   , extensions = "Buttons"
    #   , options = list(scrollX = TRUE
    #                    , dom = "Bfrtip"
    #                    , buttons = c('copy', 'csv', 'excel')
    #                    , pageLength = 5
    #                    # , lengthMenu = c(5, 10, 100) # XXX not working yet
    #   ), rownames = FALSE
    # )
  }
  )


  # Overview Plot -----------------------------------------------------------




  output$overviewplot <- plotly::renderPlotly({

    ## apply filters
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    red_temp <- red_temp[red_temp$power >= input$minpower, ]

    # source
    if (input$source == "All studies") {
      red_temp <- red_temp
    } else {
      red_temp <- red_temp[red_temp$source == input$source, ]
    }

    # validated
    if (input$validated == TRUE) {
      red_temp <- red_temp[!is.na(red_temp$validated), ]
    }

    # exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result), ]

    red_temp$significant_original <- as.factor(red_temp$significant_original)
    red_temp$significant_replication <- as.factor(red_temp$significant_replication)

    ## Choose only entries that are also displayed in the table
    s1 <- input$table_rows_current  # rows on the current page
    s2 <- input$table_rows_all      # rows on all pages (after being filtered)
    s3 <- input$table_rows_selected # selected rows
    red_temp <- red_temp[s2, ]

    red_temp$scatterplotdescription <- paste(red_temp$description, "\nr(original) = "
                                             , round(red_temp$es_original, 3)
                                             , ", r(replication) = "
                                             , round(red_temp$es_replication, 3)
                                             , sep = "")

    pointsize <- ifelse(nrow(red_temp) < 10, 5, ifelse(nrow(red_temp) < 100, 4, 3))

    scatterplot <-
      ggplot(red_temp, aes(x = es_original, y = es_replication, text = scatterplotdescription)) +
      geom_hline(aes(yintercept = 0),linetype = 2) +
      geom_abline(intercept = 0, slope = 1, color = "Grey60") +
      geom_point(aes(fill = significant_replication), color = "Grey30", shape = 21, alpha = .8) +
      # geom_point(aes(size = power, fill=significant_replication), color = "Grey30", shape = 21,alpha = .8) +
      geom_point(aes(fill = significant_replication), size = pointsize, color="Grey30", shape = 21, alpha = .8) +



      # highlighted studies
      # geom_point(data = red_temp[s3, ], mapping = aes(size = power), fill= "Grey30",color="Grey30",shape=4) +
      geom_point(data = red_temp[s3, ], fill = "#0077d9",color = "#f2ef1b", shape = 4) +

      geom_rug(aes(color=significant_original),size=1,sides="b",alpha=.6) +
      geom_rug(aes(color=significant_replication),size=1,sides="l",alpha=.6) +
      scale_x_continuous(name="Original Effect Size",limits=c(0,1),breaks=c(0,.25,.5,.75,1)) +
      scale_y_continuous(name="Replication Effect Size",limits=c(-.5,1),breaks=c(-.5,-.25,0,.25,.5,.75,1)) +
      # ggtitle("") + #xlab("") + ylab("") +
      # scale_size_continuous(name="Power",range=c(.5,3.5)) +
      scale_color_discrete(guide = "none") +
      scale_fill_discrete(guide = "none") +
      theme_bw() +
      # theme(legend.position=c(.9,.6), plot.margin = unit(c(-2,-1.5,2,2), "lines")) +
      theme(legend.position = "none")


    overviewplotly <- plotly::ggplotly(scatterplot, tooltip = "text") %>% plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

  }) # , height = 800





  # Forest Plot -------------------------------------------------------------


  output$forestplot <- plotly::renderPlotly({


    ## apply filters
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    # use only studies with a replication effect size
    red_temp <- red_temp[!is.na(red_temp$es_replication), ]

    # use only studies with a reference for the original finding
    red_temp <- red_temp[!is.na(red_temp$ref_original), ]

    # make descriptions shorter
    red_temp$description <- gsub("(.{70,}?)\\s", "\\1\n", red_temp$description) # line breaks

    # make reference shorter
    red_temp$ref_original <- gsub("(.{70,}?)\\s", "\\1\n", red_temp$ref_original) # line breaks

    red_temp_selected <- red_temp

    xlims <- seq(from = -1, 1, .25)

    red_temp$description <- factor(red_temp$description, levels = unique(red_temp$description[order(red_temp$es_replication)]))

    forest <- ggplot(data = red_temp, aes(x = es_replication, y = ref_original)) +
      geom_vline(xintercept = 0, col = "dark grey", lwd = 1) +
      # Replication effect sizes
      geom_point() +
      geom_errorbar(aes(xmin = ci.lower_replication, xmax = ci.upper_replication)) +

      # Original effect sizes
      geom_point(aes(x = es_original, y = ref_original), color = "dark grey", alpha = .5) +
      geom_errorbar(aes(xmin = ci.lower_original, xmax = ci.upper_original), color = "dark grey") +

      # highlighted studies
      geom_point(data = red_temp_selected, aes(x = es_replication, y = ref_original), color = ifelse(nrow(red_temp) == nrow(red_temp_selected), "black", "red")) +

      # Theme and formatting
      theme_classic() + geom_vline(xintercept = xlims, col = rgb(0,0,0,.05), lwd = 0.5, lty = 1) +
      theme(text = element_text(size = 14)) +
      xlim(c(floor(min(red_temp$ci.lower_original)), ceiling(max(red_temp$ci.upper_original, na.rm = TRUE)))) +
      xlab("r") +
      ylab("") +
      theme(legend.position="none") +
      theme(text = element_text(size = 10)) +
      scale_y_discrete(limits=rev) +
      ggtitle(paste("Blobbogram\n"
                    , sum(!is.na(red_temp$es_original))
                    , "Effect sizes available.\n"
                    # , length(unique(red_temp$ref_original))
                    # , "Original studies were examined in replication studies."
      ))

    p <- ggplotly(forest) %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)
  })




  # Bar Plot -------------------------------------------------------------

  output$barplot <- plotly::renderPlotly({


    ## apply filters
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    red_temp <- red_temp[red_temp$power >= input$minpower, ]

    # source
    if (input$source == "All studies") {
      red_temp <- red_temp
    } else {
      red_temp <- red_temp[red_temp$source == input$source, ]
    }

    # validated
    if (input$validated == TRUE) {
      red_temp <- red_temp[!is.na(red_temp$validated), ]
    }

    # only show show coded entries?
    if (input$codedentries == TRUE) {
      red_temp <- red_temp[!is.na(red_temp$result), ]
    }
    red_temp[is.na(red_temp$result), "result"] <- "not coded"

    # exclude NAs
    # red_temp <- red_temp[!is.na(red_temp$result), ]

    ## Choose only entries that are also displayed in the table
    s1 <- input$table_rows_current  # rows on the current page
    s2 <- input$table_rows_all      # rows on all pages (after being filtered)
    s3 <- input$table_rows_selected # selected rows
    red_temp <- red_temp[s2, ]

    bardata <- as.data.frame(base::table(red_temp$result, useNA = "always")/nrow(red_temp))
    names(bardata) <- c("Result", "Proportion")
    bardata$Proportion <- round(bardata$Proportion, 4)*100

    bardata$description <- paste(bardata$Result, ": ", bardata$Proportion, "%", sep = "")

    barchart <- ggplot(bardata, aes(x = "", fill = Result, y  = Proportion, text = description)) + geom_bar(position = "fill", stat = "identity") +
      theme_bw() + ylab("Percentage") + xlab("") + coord_flip() +
      scale_fill_manual("Result", values = c(
        "success" = "#30c25a"
        , "informative failure to replicate" = "#f0473e"
        , "practical failure to replicate" = "#f2bbb8"
        , "inconclusive" = "#60bef7")) + # , NA = "grey"
      ggtitle(paste(nrow(red_temp), "of", nrow(red), "studies selected."))
    p <- ggplotly(barchart, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)
  })





  # Barplot Result2 ---------------------------------------------------------

  output$barplot2 <- plotly::renderPlotly({


    ## apply filters
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]


    # # text inputs
    # if (nchar(input$tags) > 0) {
    #   red_temp <- red_temp[grepl(tolower(as.character(input$tags)),         tolower(red_temp$tags)), ]
    # }
    #
    # if (nchar(input$titles) > 0) {
    #   red_temp <- red_temp[grepl(tolower(as.character(input$titles)),       tolower(red_temp$description)), ]
    # }
    #
    # if (nchar(input$contributors) > 0) {
    #   red_temp <- red_temp[grepl(tolower(as.character(input$contributors)), tolower(red_temp$contributors)), ]
    # }

    red_temp <- red_temp[red_temp$power >= input$minpower, ]

    # source
    if (input$source == "All studies") {
      red_temp <- red_temp
    } else {
      red_temp <- red_temp[red_temp$source == input$source, ]
    }

    # validated
    if (input$validated == TRUE) {
      red_temp <- red_temp[!is.na(red_temp$validated), ]
    }


    ## Exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result2), ]

    ## Choose only entries that are also displayed in the table
    s1 <- input$table_rows_current  # rows on the current page
    s2 <- input$table_rows_all      # rows on all pages (after being filtered)
    s3 <- input$table_rows_selected # selected rows
    red_temp <- red_temp[s2, ]


    bardata <- as.data.frame(base::table(red_temp$result2, useNA = "always")/nrow(red_temp))
    names(bardata) <- c("Result", "Proportion")
    bardata$Proportion <- round(bardata$Proportion, 4)*100

    bardata$description <- paste(bardata$Result, ": ", bardata$Proportion, "%", sep = "")

    barchart <- ggplot(bardata, aes(x = Result, fill = Result, y  = Proportion, text = description)) +
      geom_bar(stat = "identity") +
      theme_bw() + ylab("Percentage") + xlab("") + coord_flip() +
      scale_fill_manual("Result", values = c(
        "no signal - inconsistent" = "#9c0505"
        , "signal - consistent" = "#05e361"
        , "no signal - OS n.s." = "grey"
        , "NA - OS n.s." = "grey"
        , "signal - inconsistent, smaller" = "#a4d11b"
        , "signal - inconsistent, larger" = "#77bd06"
        , "signal - OS n.s." = "grey"
        , "no signal - consistent" = "#b4d4a5"
        # , "NA" = "grey"
      )) + # , NA = "grey"
      ggtitle(paste(nrow(red_temp), "of", nrow(red), "studies selected."))
    p <- ggplotly(barchart, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)
  })



  # Z-Curve -----------------------------------------------------------------

  output$zcurve_plot <- shiny::renderPlot({

    ## apply filters
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    red_temp <- red_temp[red_temp$power >= input$minpower, ]

    # source
    if (input$source == "All studies") {
      red_temp <- red_temp
    } else {
      red_temp <- red_temp[red_temp$source == input$source, ]
    }

    # validated
    if (input$validated == TRUE) {
      red_temp <- red_temp[!is.na(red_temp$validated), ]
    }

    # exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result), ]


    ## Choose only entries that are also displayed in the table
    s1 <- input$table_rows_current  # rows on the current page
    s2 <- input$table_rows_all      # rows on all pages (after being filtered)
    s3 <- input$table_rows_selected # selected rows
    red_temp <- red_temp[s2, ]


    # # make descriptions shorter
    # red_temp$description <- gsub("(.{70,}?)\\s", "\\1\n", red_temp$description) # line breaks

    # use only studies with complete data
    red_temp <- red_temp[!is.na(red_temp$z), ]

    # run z-curve analysis
    zc <- zcurve::zcurve(z = red_temp$z, method = "EM", bootstrap = 0)

    orr <- round(mean(red_temp$result == "success", na.rm = TRUE), 2)
    err <- zc$coefficients[1]

    # create plot
    zcurve::plot.zcurve(zc, annotation = TRUE, CI = TRUE, main = paste("Observed Replication Rate: ", orr
                                                                       # , "\nCorrected ERR: ", round(1.85*err-0.573, digits = 2)
                                                                       , sep = ""))

  })






  # Dataset -----------------------------------------------------------------



  output$dataset <- DT::renderDT(DT::datatable(red_display
                                               , rownames = FALSE))

  # variables
  output$variables <- DT::renderDT(DT::datatable(dataset_variables
                                                 , rownames = FALSE, options = list(pageLength = 20)))




  # Correlates of R ---------------------------------------------------------


  output$correlate_decade <- plotly::renderPlotly({

    red <- plyr::rbind.fill(red, as)
    red <- plyr::rbind.fill(red, forrt)
    red[is.na(red$result), "result"] <- "not coded yet"

    # Aggregate results so that there is one value for each original study
    red_agg <- aggregate(result ~ ref_original, data = red, FUN = function(x) {paste(unique(x), collapse = ", ")})

    # recode mixed results
    red_agg$result <- dplyr::recode(red_agg$result
                                    , "success" = "success"
                                    , "informative failure to replicate" = "informative failure to replicate"
                                    , "inconclusive" = "inconclusive"
                                    , "practical failure to replicate" = "practical failure to replicate"
                                    , "not coded yet" = "not coded yet"
                                    , .default = "mixed")

    red_agg$year_orig <- as.numeric(substr(gsub("\\D", "", red_agg$ref_original), 1, 4))
    red_agg$year_orig <- ifelse(red_agg$year_orig > 2050, NA, red_agg$year_orig)
    red_agg$decade_orig <- as.numeric(substr(gsub("\\D", "", red_agg$ref_original), 1, 3))
    red_agg$decade_orig <- as.numeric(ifelse(!is.na(red_agg$decade_orig), paste(red_agg$decade_orig, "0", sep = ""), red_agg$decade_orig))
    red_agg$decade_orig <- ifelse(red_agg$decade_orig > 2050, NA, red_agg$decade_orig)


    ### DECADE (ORIGINAL)
    red_agg$row <- 1:nrow(red_agg)
    reprate_decade <- aggregate(row ~ decade_orig * result, data = red_agg, FUN = "length")
    names(reprate_decade) <- c("decade_orig", "Result", "k")

    p <- ggplot(reprate_decade, aes(x = decade_orig, y = k, col = Result)) +
      geom_point(position = "identity") +
      geom_line(position = "identity") +
      # geom_area(position = "stack") +
      theme_bw() +
      labs(x = "Decade the Original Finding was Published", y = "Number of Replication Findings"
           , title = paste("Aggregated replication outcomes by decade for k = ", sum(reprate_decade$k), " replicated original studies", sep = "")) +
      scale_color_manual("Result", values = c(
        "success" = "#30c25a"
        , "informative failure to replicate" = "#f0473e"
        , "practical failure to replicate" = "#f2bbb8"
        , "inconclusive" = "#60bef7"
        , "mixed" = "#ffc000"))


    plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

  })






  output$correlate_journal <- plotly::renderPlotly({

    ### JOURNAL (ORIGINAL)
    red <- plyr::rbind.fill(red, as)
    red <- plyr::rbind.fill(red, forrt)
    red[is.na(red$result), "result"] <- "not coded yet"

    # red[red$source == "FORRT" & is.na(red$validated), "result"] <- "not coded yet"

    # Aggregate results so that there is one value for each original study
    red_agg <- aggregate(result ~ ref_original + orig_journal, data = red, FUN = function(x) {paste(unique(x), collapse = ", ")})

    # recode mixed results
    red_agg$result <- dplyr::recode(red_agg$result
                                    , "success" = "success"
                                    , "informative failure to replicate" = "informative failure to replicate"
                                    , "inconclusive" = "inconclusive"
                                    , "practical failure to replicate" = "practical failure to replicate"
                                    , "not coded yet" = "not coded yet"
                                    , .default = "mixed")

    # remove faulty rows
    red_agg <- red_agg[red_agg$orig_journal != "signal", ]
    red_agg <- red_agg[red_agg$orig_journal != "no signal", ]
    red_agg <- red_agg[red_agg$orig_journal != "success", ]

    red_agg$row <- 1:nrow(red_agg)
    reprate_journal <- aggregate(row ~ orig_journal * result, data = red_agg, FUN = "length")
    names(reprate_journal) <- c("journal_orig", "Result", "k")

    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "consistent", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "inconsistent", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "mixed", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "success", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "informative failure to replicate", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "practical failure to replicate", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "inconclusive", ]

    p <- ggplot(reprate_journal, aes(x = journal_orig, y = k, fill = Result)) +
      geom_bar(stat = "identity") +
      theme_bw() +

      labs(x = "", y = "Number of Replicated Original Studies"
           , title = paste("Aggregated replication outcomes by journal for k = ", sum(reprate_journal$k), " replicated original studies.", sep = "")) +
      scale_fill_manual("Result", values = c(
        "success" = "#30c25a"
        , "informative failure to replicate" = "#f0473e"
        , "practical failure to replicate" = "#f2bbb8"
        , "inconclusive" = "#60bef7"
        , "mixed" = "#f0c91f"
      )) +
      scale_x_discrete(limits = rev) +
      coord_flip()

    plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

  })



  # MODERATORS --------------------------------------------------------------

  # Reactives for repeated elements -----------------------------------------

  preprocessed_data <- reactive({
    es <- red
    es$mod <- es[, input$moderator]
    es <- es[!is.na(es$mod), ]
    es <- es[!is.na(es$ref_original), ]
    es$se <- sqrt((1 - abs(es$es_original)^2) / (es$n_original - 2))
    es
  })

  model_computation <- reactive({
    es <- preprocessed_data()
    message("Estimate metafor")
    mod <- metafor::rma.mv(yi = es_replication
                             , V = se^2
                             , random = ~1 | ref_original
                             , tdist = TRUE
                             , data = es
                             , mods = ~ mod - 1
                             , method = "ML")

    message("Done estimating")
    mod
  })

  # Moderator Plot ----------------------------------------------------------

  output$flexibleplot <- renderPlotly({
    es <- preprocessed_data()
    mod <- es$mod
    p <- ggplot2::ggplot(data = es, aes(y = es_replication, color = ref_original)) +
      geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() +
      labs(x = input$moderator, y = "Replication Effect Size (r)", color = "Reference")

    if (is.numeric(mod)) {
      p <- p + aes(x = mod) + geom_point() + geom_smooth(aes(color = NULL), formula = y ~ x)
    } else {
      p <- p + aes(x = fct_rev(mod)) + geom_violin(fill = NA) + geom_jitter(aes(color = ref_original), width = .1) + coord_flip()
    }

    plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
  })

  # Moderator Model ---------------------------------------------------------
  output$flexiblemoderatormodel <- renderPrint({
    model <- model_computation()
    print(model)
  })

  # Moderator Text ----------------------------------------------------------
  output$flexiblemoderatortext <- renderText({
    model <- model_computation()

    HTML(paste("<br><br>The effect of ", "<b>", input$moderator, "</b>", " on replication effect sizes is ", ifelse(model[["QMp"]] < .05, "", "<b>not</b> ")
               , "significant at the 5% level. Test of moderators: <i>F</i>(", model[["QMdf"]][1], ", ", model[["QMdf"]][2]
               , ") = ", round(model[["QM"]], digits = 2), ", <i>p</i> "
               , ifelse(round(model[["QMp"]], digits = 3) == 0, "< .001", paste("=", round(model[["QMp"]], digits = 3)))
               , "."
               , sep = ""))

  })

  # Moderator Table ---------------------------------------------------------

    output$flexiblemodtable <- renderDT({
    es <- preprocessed_data()
    model <- model_computation()
    mod <- es$mod
    # check moderator type (factor or metric)
    if (is.numeric(mod)) { # metric

      modtable <- psych::describe(es$mod, fast = TRUE) #[c(2:5, 8, 9)]
      rownames(modtable) <- substring(input$moderator, first = 4)
      modtable[, 2:6] <- round(as.data.frame(modtable)[, 2:6], digits = 2)
      modelbeta <- metafor::rma.mv(yi = es_replication
                                   , V = se^2
                                   , random = ~1 | ref_original
                                   , tdist = TRUE
                                   , data = es
                                   , mods = ~ mod
                                   , method = "ML")
      modtable$beta <- round(modelbeta$b[2], digits = 2)
      modtable$vars <- NULL
      modtable$range <- NULL
      modtable$se <- NULL
    } else { # factor
      modtable <- data.frame("Moderator_Levels" = substring(rownames(model$b), first = 4)
                             , "r" =        round(model$b, digits = 3)
                             , "ci_lower" = round(model$ci.lb, digits = 3)
                             , "ci_upper" = round(model$ci.ub, digits = 3)
                             # , "k" = as.numeric(paste(table(es$mod)))
      )
    }


    # print table
    DT::datatable(modtable, options = list(options = list(pageLength = 200, dom = 't'))
                  , rownames = FALSE)
    })


  # REFERENCE CHECKER -------------------------------------------------------

  output$references_barplot <- plotly::renderPlotly({

    if (nchar(input$refcheck) > 0) {
      entries <- as.character(input$refcheck)
    }

    if (nchar(input$refcheck) > 0) {
      entries <- as.character(input$refcheck)
    }

    entries <- unlist(base::strsplit(entries, split = "\n")) # |-
    dois <- tolower(stringr::str_extract(entries, "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"))

    # combine coded and uncoded studies
    red <- plyr::rbind.fill(red, as)
    red <- plyr::rbind.fill(red, forrt)
    red[is.na(red$result), "result"] <- "not coded yet"

    # Check which entries  exist in the ReD
    intersection <- dois[dois %in% red$doi_original]

    # ReD subset
    red_temp <- red[(tolower(red$doi_original) %in% dois), ]
    red_temp <- red_temp[!is.na(red_temp$doi_original), ]

    bardata <- as.data.frame(base::table(red_temp$result, useNA = "always")/nrow(red_temp))
    names(bardata) <- c("Result", "Proportion")
    bardata$Proportion <- round(bardata$Proportion, 4)*100

    bardata$description <- paste(bardata$Result, ": ", bardata$Proportion, "%", sep = "")

    barchart <- ggplot(bardata, aes(x = "", fill = Result, y  = Proportion, text = description)) + geom_bar(position = "fill", stat = "identity") +
      theme_bw() + ylab("Percentage") + xlab("") + coord_flip() +
      scale_fill_manual("Result", values = c(
        "success" = "#30c25a"
        , "informative failure to replicate" = "#f0473e"
        , "practical failure to replicate" = "#f2bbb8"
        , "inconclusive" = "#60bef7")) + # , NA = "grey"
      ggtitle(paste(nrow(red_temp), "Replication findings were identified. These stem from", length(unique(red_temp$doi_original)), "different publication(s)."))
    p <- ggplotly(barchart, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)

  })

  output$references_doi <- shiny::renderTable({

    if (nchar(input$refcheck) > 0) {
      entries <- as.character(input$refcheck)
    }

    entries <- unlist(base::strsplit(entries, split = "\n")) # |-
    dois <- tolower(stringr::str_extract(entries, "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"))

    # combine coded and uncoded studies
    red <- plyr::rbind.fill(red, as)
    red <- plyr::rbind.fill(red, forrt)
    red[is.na(red$result), "result"] <- "not coded yet"

    # Check which entries  exist in the ReD
    intersection <- dois[dois %in% red$doi_original]

    # ReD subset
    red_temp <- red[(tolower(red$doi_original) %in% dois), ]
    red_temp <- red_temp[!is.na(red_temp$doi_original), ]

    red_temp$original <-    red_temp$ref_original   # paste(red_temp$ref_original, red_temp$doi_original, sep = " ") # ADD DOIs if they are not already part of the reference
    red_temp$replication <- red_temp$ref_replication   # paste(red_temp$ref_replication, red_temp$doi_replication, sep = " ")

    print(red_temp[, c("original", "description", "replication", "result")])

  })



  # CHECKER / SUMMARIZER -----------------------------------------------------------------


  # Checkertable ------------------------------------------------------------


  output$checkertable <- DT::renderDT(server = FALSE, {

    # combine coded and uncoded studies
    red <- plyr::rbind.fill(red, as)
    red <- plyr::rbind.fill(red, forrt)
    red[is.na(red$result), "result"] <- "not coded yet"

    ## apply filters
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    # exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result), ]

    # exclude non-validated entries
    red_temp <- red_temp[!is.na(red_temp$validated), ]


    # red_temp_filtered <- red_temp[, c("description", "n_original", "n_replication", "power", "result")]
    # red_temp_filtered <- red_temp[, c("description", "tags", "contributors", "result", "ref_original", "ref_replication")]

    DT::datatable(
      red_temp[, c("description", "tags", "result", "ref_original", "ref_replication")]
      , extensions = "Buttons"
      , options = list(scrollX = TRUE
                       , dom = "Bfrtip"
                       , buttons = c('copy', 'csv', 'excel')
                       , pageLength = 5
                       # , lengthMenu = c(5, 10, 100) # XXX not working yet
      ), rownames = FALSE
    )
  }
  )



  # # Checker Violin Plot -----------------------------------------------------
  #
  #
  #
  #   output$checker_violin <- plotly::renderPlotly({
  #
  #     # # combine coded and uncoded studies
  #     # red <- plyr::rbind.fill(red, as)
  #     # red <- plyr::rbind.fill(red, forrt)
  #     # red[is.na(red$result), "result"] <- "not coded yet"
  #
  #     # this plot is based on the filtered entries from the checkertable
  #     red_temp <- red
  #     red_temp <- red_temp[rev(row.names(red_temp)), ]
  #
  #     # exclude non-validated entries
  #     red_temp <- red_temp[!is.na(red_temp$validated), ]
  #
  #     # use only filtered studies
  #     s1 <- input$checkertable_rows_current  # rows on the current page
  #     s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
  #     s3 <- input$checkertable_rows_selected # selected rows
  #
  #     red_temp <- red_temp[s2, ]
  #
  #     # exclude NAs
  #     red_temp <- red_temp[!is.na(red_temp$result), ]
  #
  #     # compute se
  #     red_temp$se_original <- sqrt((1-abs(as.numeric(red_temp$es_original))^2)/(as.numeric(red_temp$n_original)-2))
  #     red_temp$se_replication <- sqrt((1-abs(as.numeric(red_temp$es_replication))^2)/(as.numeric(red_temp$n_replication)-2))
  #
  #     redlong_original <- red_temp[, c("es_original", "ref_original", "n_original", "se_original")]
  #     redlong_original$type = "Original"
  #     names(redlong_original) <- c("es", "ref", "n", "se", "type")
  #     redlong_original <- redlong_original[!duplicated(redlong_original), ]
  #
  #     redlong_replication <- red_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
  #     redlong_replication$type = "Replication"
  #     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
  #
  #     redlong <- rbind(redlong_original, redlong_replication)
  #     checker_gg <- ggplot(redlong, aes(x = type, y = as.numeric(es), text = ref)) + # , text = ref
  #       # geom_violin(draw_quantiles =  .5) +
  #       geom_jitter(width = .1, height = 0) +
  #       xlab("Study Type") + ylab("r") +
  #       geom_abline(h = 0, slope = 0, lty = 2) +
  #       theme_bw()
  #
  #     checker_plotly <- plotly::ggplotly(checker_gg, tooltip = "text") %>% #
  #       plotly::config(displayModeBar = FALSE) %>%
  #       layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  #
  #     checker_plotly
  #
  #   })


  # # Checker MA Plot ---------------------------------------------------------
  #
  #   output$checker_maplot <- plotly::renderPlotly({
  #
  #     # this plot is based on the filtered entries from the checkertable
  #     red_temp <- red
  #     red_temp <- red_temp[rev(row.names(red_temp)), ]
  #
  #     # exclude non-validated entries
  #     red_temp <- red_temp[!is.na(red_temp$validated), ]
  #
  #     # use only filtered studies
  #     s1 <- input$checkertable_rows_current  # rows on the current page
  #     s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
  #     s3 <- input$checkertable_rows_selected # selected rows
  #
  #     red_temp <- red_temp[s2, ]
  #
  #     # exclude NAs
  #     red_temp <- red_temp[!is.na(red_temp$result), ]
  #
  #     # compute se
  #     red_temp$se_original <- sqrt((1-abs(as.numeric(red_temp$es_original))^2)/(as.numeric(red_temp$n_original)-2))
  #     red_temp$se_replication <- sqrt((1-abs(as.numeric(red_temp$es_replication))^2)/(as.numeric(red_temp$n_replication)-2))
  #
  #     redlong_original <- red_temp[, c("es_original", "ref_original", "n_original", "se_original")]
  #     redlong_original$type = "Original"
  #     names(redlong_original) <- c("es", "ref", "n", "se", "type")
  #     redlong_original <- redlong_original[!duplicated(redlong_original), ]
  #
  #     redlong_replication <- red_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
  #     redlong_replication$type = "Replication"
  #     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
  #
  #     # remova missing values
  #     redlong <- rbind(redlong_original, redlong_replication)
  #
  #
  #     redlong <- redlong[!is.na(redlong$ref),]
  #
  #     model <- metafor::rma.mv(yi = as.numeric(es)
  #                              , V = se^2
  #                              , random = ~1 | ref
  #                              , tdist = TRUE
  #                              , data = redlong
  #                              , mods = ~ as.factor(type) - 1
  #                              , method = "ML")
  #
  #
  #     summary(model)
  #
  #     ma_table <- data.frame("study_type" = as.character(c("Original", "Replication"))
  #                            , "mean_r" =   round(as.numeric(model$beta), 3)
  #                            , "se" =       round(as.numeric(model$se), 3)
  #                            , "lower_ci" = round(as.numeric(model$ci.lb), 3)
  #                            , "upper_ci" = round(as.numeric(model$ci.ub), 3)
  #                            , "n" = c(sum(redlong$type == "Original")
  #                                      , sum(redlong$type == "Replication"))
  #     )
  #
  #     checker_maplot <- ggplot(ma_table, aes(x = study_type, y = mean_r, ymin = lower_ci, ymax = upper_ci)) +
  #       geom_point() +
  #       geom_errorbar() +
  #       theme_bw() +
  #       geom_abline(slope = 0, lty = 2) +
  #       ylim(c(-.1, round(max(as.numeric(model$ci.ub)), 1)+.1)) +
  #       xlab("Study Type") + ylab("Mean Effect Size Estimate (r)")
  #
  #     checker_maplotly <- plotly::ggplotly(checker_maplot) %>% # , tooltip = "text"
  #       plotly::config(displayModeBar = FALSE) %>%
  #       layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  #
  #     checker_maplotly
  #   })


  # Checker Barplot ---------------------------------------------------------

  output$checker_bar <- plotly::renderPlotly({

    # this plot is based on the filtered entries from the checkertable
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    # exclude non-validated entries
    red_temp <- red_temp[!is.na(red_temp$validated), ]

    # use only filtered studies
    s1 <- input$checkertable_rows_current  # rows on the current page
    s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
    s3 <- input$checkertable_rows_selected # selected rows

    red_temp <- red_temp[s2, ]

    # exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result), ]

    ## Exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result), ]

    bardata <- as.data.frame(base::table(red_temp$result, useNA = "always")/nrow(red_temp))
    names(bardata) <- c("Result", "Proportion")
    bardata$Proportion <- round(bardata$Proportion, 4)*100

    bardata$description <- paste(bardata$Result, ": ", bardata$Proportion, "%", sep = "")

    barchart <- ggplot(bardata, aes(x = "", fill = Result, y  = Proportion, text = description)) + geom_bar(position = "fill", stat = "identity") +
      theme_bw() + ylab("Percentage") + xlab("") + coord_flip() +
      scale_fill_manual("Result", values = c(
        "success" = "#30c25a"
        , "informative failure to replicate" = "#f0473e"
        , "practical failure to replicate" = "#f2bbb8"
        , "inconclusive" = "#60bef7")) + # , NA = "grey"
      ggtitle(paste(nrow(red_temp), "of", nrow(red), "studies selected."))
    p <- ggplotly(barchart, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)

    p
  })

  # Checker MA Table --------------------------------------------------------


  output$flexiblecheckertable <- DT::renderDT({

    # this plot is based on the filtered entries from the checkertable
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    # exclude non-validated entries
    red_temp <- red_temp[!is.na(red_temp$validated), ]

    # use only filtered studies
    s1 <- input$checkertable_rows_current  # rows on the current page
    s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
    s3 <- input$checkertable_rows_selected # selected rows

    red_temp <- red_temp[s2, ]

    # exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result), ]

    # compute se
    red_temp$se_original <- sqrt((1-abs(as.numeric(red_temp$es_original))^2)/(as.numeric(red_temp$n_original)-2))
    red_temp$se_replication <- sqrt((1-abs(as.numeric(red_temp$es_replication))^2)/(as.numeric(red_temp$n_replication)-2))

    redlong_original <- red_temp[, c("es_original", "ref_original", "n_original", "se_original")]
    redlong_original$type = "Original"
    names(redlong_original) <- c("es", "ref", "n", "se", "type")
    redlong_original <- redlong_original[!duplicated(redlong_original), ]

    redlong_replication <- red_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
    redlong_replication$type = "Replication"
    names(redlong_replication) <- c("es", "ref", "n", "se", "type")

    # remova missing values
    redlong <- rbind(redlong_original, redlong_replication)


    redlong <- redlong[!is.na(redlong$ref),]

    model <- metafor::rma.mv(yi = as.numeric(es)
                             , V = se^2
                             , random = ~1 | ref
                             , tdist = TRUE
                             , data = redlong
                             , mods = ~ as.factor(type) - 1
                             , method = "ML")


    summary(model)

    ma_table <- data.frame("study_type" = as.character(c("Original", "Replication"))
                           , "mean_r" =   round(as.numeric(model$beta), 3)
                           , "se" =       round(as.numeric(model$se), 3)
                           , "lower_ci" = round(as.numeric(model$ci.lb), 3)
                           , "upper_ci" = round(as.numeric(model$ci.ub), 3)
                           , "n" = c(sum(redlong$type == "Original")
                                     , sum(redlong$type == "Replication"))
    )

    # print table
    DT::datatable(ma_table, options = list(options = list(pageLength = 200, dom = 't'))
                  , rownames = FALSE)
  })


  # Checker MA Text ------------------------------------------------------------

  output$flexiblesummarizertext <- shiny::renderText({


    # this plot is based on the filtered entries from the checkertable
    red_temp <- red
    red_temp <- red_temp[rev(row.names(red_temp)), ]

    # exclude non-validated entries
    red_temp <- red_temp[!is.na(red_temp$validated), ]

    # use only filtered studies
    s1 <- input$checkertable_rows_current  # rows on the current page
    s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
    s3 <- input$checkertable_rows_selected # selected rows

    red_temp <- red_temp[s2, ]

    # exclude NAs
    red_temp <- red_temp[!is.na(red_temp$result), ]

    # compute se
    red_temp$se_original <- sqrt((1-abs(as.numeric(red_temp$es_original))^2)/(as.numeric(red_temp$n_original)-2))
    red_temp$se_replication <- sqrt((1-abs(as.numeric(red_temp$es_replication))^2)/(as.numeric(red_temp$n_replication)-2))

    redlong_original <- red_temp[, c("es_original", "ref_original", "n_original", "se_original")]
    redlong_original$type = "Original"
    names(redlong_original) <- c("es", "ref", "n", "se", "type")
    redlong_original <- redlong_original[!duplicated(redlong_original), ]

    redlong_replication <- red_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
    redlong_replication$type = "Replication"
    names(redlong_replication) <- c("es", "ref", "n", "se", "type")

    # remova missing values
    redlong <- rbind(redlong_original, redlong_replication)


    redlong <- redlong[!is.na(redlong$ref),]

    model <- metafor::rma.mv(yi = as.numeric(es)
                             , V = se^2
                             , random = ~1 | ref
                             , tdist = TRUE
                             , data = redlong
                             , mods = ~ as.factor(type) - 1
                             , method = "ML")

    HTML(ma_text <- paste("<br><br><h5>On average and using a random-effects meta-analysis, original effect sizes were "
                          , ifelse(model$pval[1] < .05, "", "not ")
                          , "significant. Average replication effect sizes were "
                          , ifelse(model$pval[2] < .05, "", "not ")
                          , "significant and "
                          , ifelse(model$ci.ub[2] < model$b[1] & model$ci.lb[1] > model$b[2], "", "not ")
                          , "significantly smaller than original effect sizes. "
                          , "<h6>"
                          , sep = ""))

  })


  # Downloadbutton ----------------------------------------------------------


  output$reddownload <- downloadHandler(
    filename = function() {
      paste("ReD-", Sys.Date(), ".csv", sep="")
    },
    content = function(con) {
      write.csv(red, con, fileEncoding = "WINDOWS-1252") # XXX nochmal prüfen
    }
  )



}

# Run the application
shinyApp(ui = ui, server = server)



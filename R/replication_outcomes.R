
# Functions to assess replication outcomes --------------------------------




### open FReD (for testing purposes)
library(metafor)
library(plotly)
library(predictionInterval)

## open dataset
red_link <- "https://osf.io/z5u9b/download"
# as <- openxlsx::read.xlsx(red_link, sheet = "Additional Studies to be added", startRow = 2) # .xlsx file
ds <- openxlsx::read.xlsx(red_link, sheet = "Data", startRow = 1) # .xlsx file
ds <- ds[-(1:2), ] # exclude labels and "X" column

ds$es_original <- as.numeric(ds$es_original)
ds$n_original <- as.numeric(ds$n_original)
ds$es_replication <- as.numeric(ds$es_replication)
ds$n_replication <- as.numeric(ds$n_replication)

ds$es_original <- ifelse(ds$es_original == 1, .999999, ds$es_original)






# Success criteria --------------------------------------------------------



## choose success criterion
success_criteria <- matrix(c(    "criterion", "name", "explanation", "use"
                               , "significance_r", "Significance (R)", "Replication effect is significantly larger than zero", ""
                               , "significance_or", "Significance (O+R)", "Aggregated effect of O+R is significantly larger than zero", ""
                               , "consistency_nhst", "Consistency between O&R (NHST)", "Replication effect is not significantly different from original effect", ""
                               , "consistency_pi", "Consistency between O&R (PI)", "Replication effect is not significantly different from original effect using prediction intervals", ""
                               , "homogeneity", "Homogeneity" , "Effect sizes from O+R are not heterogeneous accoring to a meta-analytical model", ""
                               , "homogeneity_significance", "Homogeneity + Significance", "Effect sizes from O+R are not heterogeneous and larger than zero accoring to a meta-analytical model", ""
                               ), ncol = 4, byrow = TRUE
                           )

success_criteria <- as.data.frame(success_criteria[2:nrow(success_criteria), ])
names(success_criteria) <- c("criterion", "name", "explanation", "use")
success_criteria

# input_criterion <- success_criteria[1, 1] # significance_r
# input_criterion <- success_criteria[2, 1] # significance_or
# rm(input_criterion)



# Function for effect size comparison -------------------------------------

# create function to compare effect sizes
compare_effectsizes <- function(r1, n1, r2, n2) {
  r.1 <- metafor::escalc(ni = n1, ri = r1, measure = "COR")
  r.2 <- metafor::escalc(ni = n2, ri = r2, measure = "COR")

  metadata <- data.frame(yi = c(r.1$yi, r.2$yi),
                         vi = c(r.1$vi, r.2$vi),
                         study = c("original", "replication"))

  p <- metafor::rma(yi,
           vi,
           data = metadata,
           method = "FE")


  return(p)
  # rm(list(r.1, r.2, metadata, p))
}

# test
i <- 1126
compare_effectsizes(r1 = ds$es_original[i], n1 = ds$n_original[i], r2 = ds$es_replication[i], n2 = ds$n_replication[i])

# Function for replication success ----------------------------------------


assess_replication_success <- function(es_o, n_o, es_r, n_r, criterion) {

  if (criterion == "significance_r") {

    # test if any of the necessary values are missing
    if (!is.na(es_o) &
        !is.na(n_o) &
        !is.na(es_r) &
        !is.na(n_r)
    ) {
      # test if original effect is significantly larger than 0
      if (psychometric::CIr(r = es_o, n = n_o, level = .95)[1] > 0) # used two-tailed because original finding may have been unexpected
      {
        # test if replication effect is significantly larger than 0
        if (psychometric::CIr(r = es_r, n = n_r, level = .95)[1] > 0) {
          outcome <- "+ replication effect is significantly larger than 0"
        } else {
          outcome <- "- replication effect is not significantly larger than 0"
        }
      } else { # if original effect is not significantly larger than 0
        outcome <- "- original effect is not significant"
      }
    } else { # if any of the necessary values are missing
      outcome <- "0 not coded"
    }



  } else if (criterion == "significance_or") {

    # test if any of the necessary values are missing
    if (!is.na(es_o) &
        !is.na(n_o) &
        !is.na(es_r) &
        !is.na(n_r)
    ) {

      # test in meta-analytical model, whether aggregated effect is larger than zero
      ce <- compare_effectsizes(r1 = es_o, n1 = n_o, r2 = es_r, n2 = n_r)

      if (ce$ci.lb < 0) {
        outcome <- "+ aggregated effect is larger than 0"
      } else {
        outcome <- "- aggregated effect is not larger than 0"
      }

    } else { # if any of the necessary values are missing
      outcome <- "0 not coded"
    }

    rm(ce)



  } else if (criterion == "consistency_nhst") {

    # test if any of the necessary values are missing
    if (!is.na(es_o) &
        !is.na(n_o) &
        !is.na(es_r) &
        !is.na(n_r)
    ) {

      # get confidence intervals for replication effect
      ci_r <- psychometric::CIr(r = es_r, n_r)

      # test if original effect is in confidence interval
      if (es_o > ci_r[1] & es_o < ci_r[2]) {
        outcome <- "+ Original effect size is within replication's CI"
      } else {
        outcome <- "- Original effect size is not within replication's CI"
      }



    } else { # if any of the necessary values are missing
      outcome <- "0 not coded"
    }

    rm(ci_r)


  } else if (criterion == "consistency_pi") {


    # test if any of the necessary values are missing
    if (!is.na(es_o) &
        !is.na(n_o) &
        !is.na(es_r) &
        !is.na(n_r)
    ) {


      # get prediction intervals for replication effect
      # roriginal +/- z0.975*sqrt( (1/noriginal-3) + (1/nreplication-3) ).
      # pi_lower <- es_o - qnorm(.975)*sqrt((1/(n_o-3)) + (1/(n_r-3)) )
      # pi_upper <- es_o + qnorm(.975)*sqrt((1/(n_o-3)) + (1/(n_r-3)) )

      pi_lower <- predictionInterval::pi.r(r = es_o, n = n_o)$lower_prediction_interval
      pi_upper <- predictionInterval::pi.r(r = es_o, n = n_o)$upper_prediction_interval



      # test if replication effect size is in original effect's prediction interval

      if (es_r > pi_lower & es_r < pi_upper) {
        outcome <- "+ original study's PI overlaps with replication effect"
      } else {
        outcome <- "- original study's PI does not overlap with replication effect"
      }



    } else { # if any of the necessary values are missing
      outcome <- "0 not coded"
    }

    rm(pi_lower)
    rm(pi_upper)


  } else if (criterion == "homogeneity") {

    # test if any of the necessary values are missing
    if (!is.na(es_o) &
        !is.na(n_o) &
        !is.na(es_r) &
        !is.na(n_r)
    ) {

      # test in meta-analytical model whether there is heterogeneity (difference between effect sizes)
      ce <- compare_effectsizes(r1 = es_o, n1 = n_o, r2 = es_r, n2 = n_r)

      if (ce$QEp < .05) {
        outcome <- "- effects are heterogeneous"
      } else {
        outcome <- "+ effects are not heterogeneous"
      }


    } else { # if any of the necessary values are missing
      outcome <- "0 not coded"
    }

    rm(ce)




  } else if (criterion == "homogeneity_significance") {

    # test if any of the necessary values are missing
    if (!is.na(es_o) &
        !is.na(n_o) &
        !is.na(es_r) &
        !is.na(n_r)
    ) {

      # test in meta-analytical model whether there is heterogeneity (difference between effect sizes) and whether the overall effect size is larger than zero
      ce <- compare_effectsizes(r1 = es_o, n1 = n_o, r2 = es_r, n2 = n_r)

      if (ce$QEp > .05 & ce$ci.lb > 0) {
        outcome <- "+ effect sizes are homogeneous and larger than 0"
      } else {
        outcome <- "- effect sizes are not homogeneous or not larger than 0"
      }


    } else { # if any of the necessary values are missing
      outcome <- "0 not coded"
    }

    rm(ce)

  }

  return(outcome)

  rm(outcome)

}

# ##### testing
# ## run function...
# # on single datapoint
# i <- 1126 # datareplicada
# assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "significance_r")
# assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "significance_or")
# assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "consistency_nhst")
# assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "consistency_pi")
# assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "homogeneity")
# assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "homogeneity_significance")
#
#
# es_o = ds$es_original[i]
# n_o = ds$n_original[i]
# es_r = ds$es_replication[i]
# n_r = ds$n_replication[i]
#
# # on entire dataset
# input_criterion <- "significance_r"
# mapply(FUN = function(es_o, n_o, es_r, n_r, criterion) (assess_replication_success(es_o, n_o, es_r, n_r, criterion)),
#        es_o = ds$es_original, n_o = ds$n_original, es_r = ds$es_replication, n_r = ds$n_replication, criterion = input_criterion)
#
#
# # using a for loop (I could not get mapply to work)
# ds$outcome <- NA
# input_criterion <- "significance_r"
# input_criterion <- "consistency_pi"
# for (i in 1:nrow(ds)) {
#   ds[i, "outcome"] <- assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], input_criterion)
# }
# table(ds$outcome, useNA = "always")



# Compare criteria --------------------------------------------------------

for (i in 1:nrow(ds)) {
  ds[i, "significance_r"] <- assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "significance_r")
  ds[i, "significance_or"] <- assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "significance_or")
  ds[i, "consistency_nhst"] <- assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "consistency_nhst")
  ds[i, "consistency_pi"] <- assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "consistency_pi")
  ds[i, "homogeneity"] <- assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "homogeneity")
  ds[i, "homogeneity_significance"] <- assess_replication_success(es_o = ds$es_original[i], n_o = ds$n_original[i], es_r = ds$es_replication[i], n_r = ds$n_replication[i], "homogeneity_significance")
}

dslong <- reshape::melt(ds[ , c("id", "ref_original", "ref_replication", "significance_r", "significance_or", "consistency_nhst", "consistency_pi", "homogeneity", "homogeneity_significance")], id = c("id", "ref_original", "ref_replication"))
dslong_agg <- aggregate(id ~ value + variable, data = dslong, FUN = "length")
outcome_colors <- c(  '+ aggregated effect is larger than 0' = "lightgreen"
                    , '- aggregated effect is not larger than 0' = "red"
                    , '+ replication effect is significantly larger than 0' = "lightgreen"
                    , '- replication effect is not significantly larger than 0' = "red"
                    , '+ aggregated effect is larger than 0' = "lightgreen"
                    , '- aggregated effect is not larger than 0' = "red"
                    , '+ Original effect size is within replication\'s CI' = "lightgreen"
                    , '- Original effect size is not within replication\'s CI' = "red"
                    , '+ original study\'s PI overlaps with replication effect' = "lightgreen"
                    , '- original study\'s PI does not overlap with replication effect' = "red"
                    , '- effects are heterogeneous' = "red"
                    , '+ effects are not heterogeneous' = "lightgreen"
                    , '+ effect sizes are homogeneous and larger than 0' = "lightgreen"
                    , '- effect sizes are not homogeneous or not larger than 0' = "red"
                    , '0 not coded' = "grey"
                    , '0 original effect is not significant' = "black"
                    )
library(ggplot2)
p <- ggplot(data = dslong_agg, aes(x = variable, y = id, fill = value)) + geom_bar(position = "stack", stat = "identity") + ylab("k") + xlab("Outcome criterion") +
  scale_fill_manual(values = outcome_colors) + coord_flip()
p
# plotly::ggplotly(p)
#
# table(
#  ds$significance_r
# , ds$significance_or
# , ds$consistency_nhst
# , ds$consistency_pi
# , ds$homogeneity
# , ds$homogeneity_significance
#  )
#
# table(
#   ds$significance_r
#   , ds$significance_or)
#
# table(
#   ds$significance_r
#   , ds$consistency_nhst)
#
# table(
#   ds$significance_r
#   , ds$homogeneity)




# ### Comparison of PI calculations
#
# pi_lower <- ds$es_original[i] - qnorm(.975)*sqrt((1/(ds$n_original[i]-3)) + (1/(ds$n_original[i]-3)) )
# pi_upper <- ds$es_original[i] + qnorm(.975)*sqrt((1/(ds$n_original[i]-3)) + (1/(ds$es_original[i]-3)) )
#
# predictionInterval::pi.r(r = ds$es_original[i], n = ds$n_original[i])$lower_prediction_interval
# predictionInterval::pi.r(r = ds$es_original[i], n = ds$n_original[i])$upper_prediction_interval


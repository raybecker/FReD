success_criteria_colors <- tibble::tribble(
  ~criterion,                 ~label,                                      ~color,

  # significance_r outcome_report labels
  "significance_r",           "OS not significant",                        "#D3D3D3",
  "significance_r",           "failure",                                   "#FF7F7F",
  "significance_r",           "success",                                   "#8FBC8F",
  "significance_r",           "failure (reversal)",                        "darkred",

  # significance_agg outcome_report labels
  "significance_agg",         "success",                                   "#8FBC8F",
  "significance_agg",         "failure",                                   "#FF7F7F",
  "significance_agg",         "failure (reversal)",                        "darkred",

  # consistency_ci outcome_report labels
  "consistency_ci",           "success",                                   "#8FBC8F",
  "consistency_ci",           "failure",                                   "#FF7F7F",

  # consistency_pi outcome_report labels
  "consistency_pi",           "success",                                   "#8FBC8F",
  "consistency_pi",           "failure",                                   "#FF7F7F",

  # homogeneity outcome_report labels
  "homogeneity",              "success",                                   "#8FBC8F",
  "homogeneity",              "failure",                                   "#FF7F7F",

  # homogeneity_significance outcome_report labels
  "homogeneity_significance", "OS not significant",                        "#D3D3D3",
  "homogeneity_significance", "success (homogeneous and jointly significantly above 0)", "#8FBC8F",
  "homogeneity_significance", "failure (not homogeneous but jointly significantly above 0)", "#FF7F7F",
  "homogeneity_significance", "failure (effects are not homogeneous and not significant)", "#FF7F7F",
  "homogeneity_significance", "failure (homogeneous but not significant)",           "#FF7F7F",
  "homogeneity_significance", "failure (not homogeneous and not significant)",           "#FF7F7F",

  # small_telescopes outcome_report labels
  "small_telescopes",         "success",                                   "#8FBC8F",
  "small_telescopes",         "failure",                                   "#FF7F7F"
)

success_criterion_note <- c(
  significance_r = "Success was based on whether the replication effect was statistically significant and in the same direction as the original. Note: *p*-values are calculated from raw effect sizes and sample sizes, and may differ from those reported in studies that adjusted for covariates or clustering.",

  significance_agg = "Success was based on whether a meta-analytic combination of original and replication effects was statistically significant. Note: *p*-values are calculated from raw effect sizes and sample sizes, and may differ from those reported in studies that adjusted for covariates.",

  consistency_ci = "Success was based on whether the original effect size fell within the confidence interval of the replication. Note: confidence intervals are based on raw effect sizes and sample sizes, and may differ from those reported in adjusted models.",

  consistency_pi = "Success was based on whether the replication effect size fell within the prediction interval from the original study. Note: prediction intervals are based on raw effect sizes and sample sizes, and may differ from those reported in adjusted models.",

  homogeneity = "Success was based on a test of heterogeneity (Q-test) between original and replication effects. Note: test statistics are based on raw effect sizes and sample sizes, and may differ from those reported in adjusted analyses in the original reports.",

  homogeneity_significance = "Success was based on both effect homogeneity and statistical significance. Replications where the effects were homogeneous and jointly significantly different from zero were considered as successes, while those that were either not homogeneous or not significantly different from zero were considered as failures. Note: all values are based on raw effect sizes and sample sizes, and may differ from those reported in adjusted models.",

  small_telescopes = "Success was based on whether the replication effect exceeded the threshold that would give 33% power in the original study. Note: power was calculated based on a simple test of a correlation, thus not accounting for any specific design features relevant to the study."
)

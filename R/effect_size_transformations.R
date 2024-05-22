#' Convert effect sizes to common metric (r)
#'
#' Takes vectors of effect sizes and their types and converts them to a common metric (r).
#'
#' @param es_values Numeric vector of effect sizes
#' @param es_types Character vector of effect size types (types/wordings that are not supported are flagged in warning)
#' @return Numeric vector of effect sizes in common metric (r)

convert_effect_sizes <- function(es_values, es_types) {
  es_types <- tolower(es_types)

  # TK: dataset has a lot of different ways to refer to the same effect size type
  # TK: should be cleaned up there eventually
  estype_map <- c("or" = "or", "odds ratio" = "or",
                  "d" = "d", "cohen's d" = "d", "hedges' g" = "d", "hedges'g" = "d", "hedge's g" = "d", "hedges g" = "d", "smd" = "d",
                  "η²" = "eta", "etasq" = "eta",
                  "f" = "f",
                  "r" = "r", "φ" = "r", "phi" = "r",
                  "r²" = "r2", "r2" = "r2")

  es_values_r <- rep(NA, length(es_values))

  if (length(setdiff(na.omit(unique(es_types)), names(estype_map))) > 0) {
    warning("Unknown effect size types: ", paste(setdiff(na.omit(unique(es_types)), names(estype_map)), collapse = ", "))
  }

  for (estype in names(estype_map)) {
    idx <- !is.na(es_types) & es_types == estype
    estype <- estype_map[estype]
    if (estype == "r") {
      es_values_r[idx] <- es_values[idx]
    } else if (estype == "r2") {
      es_values_r[idx] <- sqrt(es_values[idx])
    } else {
      # Construct function call dynamically based on effect size type
      es_arg <- list()
      es_arg[[estype]] <- es_values[idx]
      #browser()
      es_values_r[idx] <- try(do.call(esc::pearsons_r, es_arg))
    }
  }

  if (any(is.na(es_values_r) & !is.na(es_values))) {
    warning(sum(is.na(es_values_r) & !is.na(es_values)), " effect sizes could not be transformed")
  }

  if (any(na.omit(abs(es_values_r) > 1))) {
    warning("Some calculated correlations are outside the range of -1 to 1. Check input IDs ", paste(which(abs(es_values_r) > 1), collapse = ", "),
            ". They will set to -1 / + 1 respectively.")
    es_values_r[es_values_r > 1] <- 1
    es_values_r[es_values_r < -1] <- -1
  }

  es_values_r
}

#' Add common effect size columns to FReD dataset
#'
#' Converts original and replication effect sizes to common metric (r) and adds them to the dataset.
#'
#' @param fred_data FReD dataset
#' @param es_value_columns Character vector of column names for effect sizes
#' @param es_type_columns Character vector of column names for effect size types
#' @param es_common_names Names of columns where effect sizes should be saved. If coalesce_values is TRUE, existing values in these columns will be retained *if* no conversion is possible.
#' @param coalesce_values Logical. Should existing values in es_type_columns be retained?
#' @return FReD dataset with additional columns for common effect sizes

add_common_effect_sizes <- function(fred_data, es_value_columns = c("es_orig_value", "es_rep_value"),
                                    es_type_columns = c("es_orig_estype", "es_rep_estype"), es_common_names = c("es_original", "es_replication"),
                                    coalesce_values = TRUE) {
  if (!all.equal(length(es_value_columns), length(es_type_columns), length(es_common_names))) {
    stop("Length of es_value_columns, es_type_columns, and es_common_names must be equal")
  }
  for (i in seq_along(es_value_columns)) {
    if (!es_value_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_value_columns[i], " not found in FReD dataset")
    }
    if (!es_type_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_type_columns[i], " not found in FReD dataset")
    }

    if (coalesce_values && es_common_names[i] %in% names(fred_data)) {
      prev_es <- as.numeric(fred_data[, es_common_names[i]])
      prev_es[prev_es > 1] <- NA
      prev_es[prev_es < -1] <- NA
      fred_data[, es_common_names[i]] <- dplyr::coalesce(convert_effect_sizes(fred_data[, es_value_columns[i]], fred_data[, es_type_columns[i]]), prev_es)
    } else {
      fred_data[, es_common_names[i]] <- convert_effect_sizes(fred_data[, es_value_columns[i]], fred_data[, es_type_columns[i]])
    }

  }

  fred_data
}

#' Align effect direction
#'
#' Ensure that all original effects are coded as positive, and that replication effects are coded in the same direction (so that they *would* be positive if successful.
#'
#' @param fred_data FReD dataset
#' @param es_original Character. Name of original effect size column.
#' @param es_replication Character. Name of replication effect size column.
#' @return Augmented FReD dataset with aligned effect directions.

align_effect_direction <- function(fred_data, es_original = "es_original", es_replication = "es_replication") {
  orig_direction <- sign(fred_data[, es_original])
  fred_data[, es_original] <- abs(fred_data[, es_original])
  fred_data[, es_replication] <- fred_data[, es_replication] * orig_direction
  fred_data
}

#' Add sampling variances, confidence intervals and p-values
#'
#' Adds sampling variances, confidence intervals (asymmetric, using z-transformation) and p-values for common-metric effect sizes (r) to the FReD dataset.
#'
#' @param fred_data FReD dataset
#' @param es_value_columns Character vector of column names with correlation values
#' @param N_columns Character vector of column names with sample sizes
#' @param se_columns Character vector of target columns for standard errors
#' @return FReD dataset with additional columns for standard errors

add_uncertainty <- function(fred_data, es_value_columns = c("es_original", "es_replication"),
                            N_columns = c("n_original", "n_replication"),
                            vi_columns = c("vi_original", "vi_replication"),
                            ci_lower_columns = c("ci.lower_original", "ci.lower_replication"),
                            ci_upper_columns = c("ci.upper_original", "ci.upper_replication"),
                            p_values = c("p_value_original", "p_value_replication")) {
  if (!all.equal(length(es_value_columns), length(N_columns), length(vi_columns), length(ci_lower_columns), length(ci_upper_columns))) {
    stop("Length of all column character vectors must be equal")
  }
  for (i in seq_along(es_value_columns)) {
    if (!es_value_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_value_columns[i], " not found in FReD dataset")
    }
    if (!N_columns[i] %in% colnames(fred_data)) {
      stop("Column ", N_columns[i], " not found in FReD dataset")
    }

    fred_data[, vi_columns[i]] <- metafor::escalc(measure = "COR", ri = fred_data[, es_value_columns[i]], ni = fred_data[, N_columns[i]])$vi

    # By default, the CIr function from psychometric only accepts a single r and n, so we need to vectorize it
    get_cis <- Vectorize(function(r, n) {
      if (is.na(n) | n <= 3) return(list(NA, NA) %>% matrix() %>% as.numeric())
      psychometric::CIr(r =  r, n = n)
    })

    ci <- get_cis(r =  fred_data[, es_value_columns[i]], n = fred_data[, N_columns[i]])

    fred_data[, ci_lower_columns[i]] <- ci[1,]
    fred_data[, ci_upper_columns[i]] <- ci[2,]

    fred_data[, p_values[i]] <- p_from_r(fred_data[, es_value_columns[i]], fred_data[, N_columns[i]])

    }

  fred_data
}

#' Code replication outcomes
#'
#' Different frameworks have been proposed to code replication outcomes. Here we code:
#' - signal vs no-signal (i.e., significant vs all others)
#' - consistent vs inconsistent (i.e., replication confidence interval overlaps original point estimate)
#' Based on https://etiennelebel.com/documents/lebeletal%282018,ampss%29a-unified-framework-to-quantify-the-credibility-of-scientific-findings.pdf
#' - and success vs failure (significant *in right direction* vs all others)
#'
#' @param fred_data FReD dataset
#' @param es_original Character. Name of original effect size column.
#' @param es_replication Character. Name of replication effect size column.
#' @param p_original Character. Significance of original effect size.
#' @param p_replication Character. Significance of replication effect size.
#' @return Augmented FReD dataset with replication outcome columns, including `signal`
#' @importFrom rlang sym
#' @importFrom dplyr mutate case_when

code_replication_outcomes <- function(fred_data,
                           es_original = "es_original",
                           p_original = "p_value_original",
                           p_replication = "p_value_replication",
                           ci_lower_replication = "ci.lower_replication",
                           ci_upper_replication = "ci.upper_replication",
                           es_replication = "es_replication") {

  # Convert column names to symbols for dplyr evaluation
  es_original_sym <- rlang::sym(es_original)
  p_original_sym <- rlang::sym(p_original)
  p_replication_sym <- rlang::sym(p_replication)
  ci_lower_replication_sym <- rlang::sym(ci_lower_replication)
  ci_upper_replication_sym <- rlang::sym(ci_upper_replication)
  es_replication_sym <- rlang::sym(es_replication)

  fred_data <- fred_data %>%
    dplyr::mutate(
      signal = ifelse(!!p_replication_sym < 0.05, "signal", "no signal"),
      os_ns = ifelse(!!p_original_sym >= 0.05, "OS not significant", NA_character_),
      consistent = ifelse(!!ci_lower_replication_sym <= !!es_original_sym & !!ci_upper_replication_sym >= !!es_original_sym, "consistent", NA_character_),
      inconsistent = case_when(
        is.na(consistent) & signal == "signal" & !!ci_lower_replication_sym > !!es_original_sym ~ "inconsistent, larger",
        is.na(consistent) & signal == "signal" & !!ci_upper_replication_sym < !!es_original_sym ~ "inconsistent, smaller",
        is.na(consistent) & signal == "signal" & sign(!!es_replication_sym) != sign(!!es_original_sym) ~ "inconsistent, opposite",
        is.na(consistent) & signal != "signal" & !!ci_upper_replication_sym < !!es_original_sym ~ "inconsistent",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate(
      consistency = dplyr::coalesce(os_ns, consistent, inconsistent),
      result = dplyr::case_when(
        !!p_original_sym >= 0.05 ~ "OS not significant",
        !!p_replication_sym < 0.05 & sign(!!es_original_sym) == sign(!!es_replication_sym) ~ "successful replication",
        !!p_replication_sym >= 0.05 | sign(!!es_original_sym) != sign(!!es_replication_sym) ~ "failed replication",
        TRUE ~ NA_character_
      ),
      result2 = paste(signal, consistency, sep = " - ")
    ) %>%
    dplyr::select(-consistent, -inconsistent, -os_ns)

  return(fred_data)
}

#' Add power
#'
#' Estimates the power of the replication study, given the original effect size, the sample size of the replication study, and the usual focus on a two-tailed test.
#'
#' @param fred_data FReD dataset
#' @param es_original Character. Name of original effect size column.
#' @param sample_replication Character. Name of replication sample size column.
#' @param power_column Character. Name of target column for power.
#' @return Augmented FReD dataset with power column.

add_replication_power <- function(fred_data, es_original = "es_original", N_replication = "n_replication", power_column = "power_r") {
  # NA where N_replication is missing
  fred_data[, power_column] <- NA
  # Return 0 where sample_replication < 4, as pwr.r.test does not work for n < 4
  # This will underestimate power for small samples, but they should be visible as low power rather than missing
  fred_data[fred_data[, N_replication] %>% {!is.na(.) & . < 4}, power_column] <- 0
  # Return power where sample_replication >= 4
  vector_power <- Vectorize(function(n, r) {
    pwr::pwr.r.test(n = n, r = r, sig.level = 0.05, alternative = "two.sided")$power
  })
  fred_data[fred_data[, N_replication] %>% {!is.na(.) & . >= 4}, power_column] <- vector_power(n = fred_data[fred_data[, N_replication] %>% {!is.na(.) & . >= 4}, N_replication], r = fred_data[fred_data[, N_replication] %>% {!is.na(.) & . >= 4}, es_original])
  fred_data
}

#' Calculate p-values from correlation coefficients and sample size
#'
#' This function calculates the p-value associated with a given correlation
#' coefficient and sample size, assuming the null hypothesis that the true
#' population correlation is zero.
#'
#' @param r Correlation coefficient.
#' @param N Sample size.
#'
#' @return Numeric p-value for the two-tailed test of the correlation.
#' @examples
#' p_from_r(r = c(0.5, 0.3), N = c(30, 25))

p_from_r <- function(r, N) {
  # Ensure arguments are numeric
  if (!is.numeric(r) || !is.numeric(N)) {
    stop("Both 'r' and 'N' must be numeric")
  }
  if (length(r) != length(N)) {
    stop("Length of 'r' and 'N' must be the same")
  }

  # Calculate the t-value
  t_value <- r * sqrt((N - 2) / (1 - r^2))

  # Calculate the p-value based on the t-distribution
  p_value <- 2 * pt(-abs(t_value), df = N - 2)

  return(p_value)
}

#' Add confidence intervals
#'
#' Adds standard errors for common-metric effect sizes (r) to the FReD dataset, using metafor::escalc
#'
#' @param fred_data FReD dataset
#' @param es_value_columns Character vector of column names with correlation values
#' @param N_columns Character vector of column names with sample sizes
#' @param se_columns Character vector of target columns for standard errors
#' @return FReD dataset with additional columns for standard errors

add_sampling_variances <- function(fred_data, es_value_columns = c("es_original", "es_replication"),
                                   N_columns = c("n_original", "n_replication"), vi_columns = c("vi_original", "vi_replication")) {
  if (!all.equal(length(es_value_columns), length(N_columns))) {
    stop("Length of es_value_columns, N_columns and vi_columns must be equal")
  }
  for (i in seq_along(es_value_columns)) {
    if (!es_value_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_value_columns[i], " not found in FReD dataset")
    }
    if (!N_columns[i] %in% colnames(fred_data)) {
      stop("Column ", N_columns[i], " not found in FReD dataset")
    }
    fred_data[, vi_columns[i]] <- metafor::escalc(measure = "COR", ri = fred_data[, es_value_columns[i]], ni = fred_data[, N_columns[i]], data = fred_data)$vi
  }
  fred_data
}

#' Augment Data for Z-Curve Analysis
#'
#' This function calculates and appends the standard error and z-score
#' for z-curve analysis based on original effect size and sample size.
#'
#' @param fred_data A dataframe containing `es_original`
#'   for the effect size of the original study, and `n_original` for the
#'   sample size of the original study.
#' @return `fred_data` but with two additional columns:
#'   `se` for the standard error, and `z` for the z-score.

# Example data for testing - target_p come from cor.test
z_test_data <- data.frame(
   es_original = c(0.522607 , -0.4926866 , -0.1175698),
   n_original = c(32, 32, 150),
   target_p = c(0.002151, 0.004173, 0.1519)
 )

augment_for_zcurve <- function(fred_data, method = c("rtoz", "r/se")) {

  # Ensure fred_data has required columns
  if (!all(c("es_original", "n_original") %in% names(fred_data))) {
    stop("fred_data must contain es_original and n_original columns")
  }

  if (method[1] == "rtoz") {
    # Fisher's z transformation
    z <- 0.5 * (log(1 + fred_data$es_original) - log(1 - fred_data$es_original))
    fred_data$se <- 1 / sqrt(fred_data$n_original - 3)
      fred_data$z <-  z / fred_data$se

  } else if (method[1] == "r/se") {
    # Method used in current Shiny app
    fred_data$se <- sqrt((1-abs(fred_data$es_original)^2)/(fred_data$n_original-2))
    fred_data$z <- fred_data$es_original / fred_data$se
  }
  return(fred_data)
}

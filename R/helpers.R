#' Compute Confidence Intervals for Correlation Coefficient
#'
#' Computes the confidence intervals for correlation coefficients using the Fisher r-to-z transformation.
#'
#' @param r Numeric vector. The correlation coefficients.
#' @param n Integer vector. The sample sizes.
#' @param conf.level Numeric. The confidence level for the interval. Default is 0.95.
#'
#' @return A data frame with columns `r`, `n`, `lower`, and `upper` containing the confidence intervals.
#' @noRd
#' @examples
#' r <- c(0.5, 0.3)
#' n <- c(30, 40)
#' conf.level <- 0.95
#' compute_ci_r(r, n, conf.level)
#'
compute_ci_r <- function(r, n, conf.level = 0.95) {
  # Ensure r and n are the same length
  if (length(r) != length(n)) {
    stop("The length of 'r' and 'n' must be the same")
  }

  # Handle NA values directly to avoid warnings
  valid_indices <- !(is.na(r) | is.na(n) | n <= 3)

  # Initialize vectors to NA
  z_lower <- z_upper <- r_lower <- r_upper <- rep(NA, length(r))

  if (any(valid_indices)) {
    # Fisher r-to-z transformation
    z <- 0.5 * log((1 + r[valid_indices]) / (1 - r[valid_indices]))

    # Standard error of z
    se <- 1 / sqrt(n[valid_indices] - 3)

    # Z critical value for the confidence level
    alpha <- 1 - conf.level
    z_crit <- qnorm(1 - alpha / 2)

    # Confidence interval for z
    z_lower[valid_indices] <- z - z_crit * se
    z_upper[valid_indices] <- z + z_crit * se

    # Inverse Fisher z-to-r transformation
    r_lower[valid_indices] <- (exp(2 * z_lower[valid_indices]) - 1) / (exp(2 * z_lower[valid_indices]) + 1)
    r_upper[valid_indices] <- (exp(2 * z_upper[valid_indices]) - 1) / (exp(2 * z_upper[valid_indices]) + 1)
  }

  # Return the confidence intervals in a data frame
  data.frame(r = r, n = n, lower = r_lower, upper = r_upper)
}

#' Compute Statistical Power for Correlation Coefficient
#'
#' Computes the statistical power for testing a correlation coefficient using a two-sided hypothesis given r, n, and alpha.
#'
#' @param r Numeric vector. The correlation coefficients.
#' @param n Integer vector. The sample sizes.
#' @param alpha Numeric. The significance level for the test. Default is 0.05.
#'
#' @return A numeric vector of the computed statistical power.
#'
#' @source Derived from the `pwr.r.test` function in the `pwr` package, copyright (c) 2020 by Stephane Champely.
#'
#' @noRd
#' @examples
#' r <- c(0.3, 0.5)
#' n <- c(30, 50)
#' alpha <- 0.05
#' compute_power_r(r, n, alpha)
#'
  compute_power_r <- function(r, n, alpha = 0.05) {
    # Ensure r and n are the same length
    if (length(r) != length(n)) {
      stop("The length of 'r' and 'n' must be the same")
    }

    # Compute the power
    power <- numeric(length(r))
    for (i in seq_along(r)) {
      ttt <- qt(alpha, df = n[i] - 2, lower.tail = FALSE)
      rc <- sqrt(ttt^2 / (ttt^2 + n[i] - 2))
      zr <- atanh(abs(r[i])) + abs(r[i]) / (2 * (n[i] - 1))
      zrc <- atanh(rc)
      power[i] <- pnorm((zr - zrc) * sqrt(n[i] - 3))
    }

    return(power)
  }

#' Find Correlation Coefficient for Desired Power
#'
#' Calculates the correlation coefficient required to achieve a target power
#' in a two-tailed test of correlation, given the sample size and significance level.
#'
#' @param power_target Numeric. The desired statistical power (between 0 and 1).
#' @param n Integer. The sample size.
#' @param alpha Numeric. The significance level (default is 0.05).
#'
#' @return Numeric. The required correlation coefficient.
#'
#' @details This function incorporates a small sample correction, similar to that
#' used in `compute_power_r()` - however, the correction cannot be analytically reversed,
#' so that minor deviations between the two functions occur (usually not before the 4th decimal).
#'
#' @examples
#' find_r_for_power(0.8, 100)
#' find_r_for_power(0.9, 50, 0.01)
#'
#' @noRd

  find_r_for_power <- function(power_target, n, alpha = 0.05) {
    # Compute critical t value for the given alpha and sample size
    t_crit <- qt(alpha, df = n - 2, lower.tail = FALSE)

    # Compute the corresponding critical correlation coefficient (rc)
    r_crit <- sqrt(t_crit^2 / (t_crit^2 + n - 2))

    # Compute z-score for the target power
    z_power <- qnorm(power_target)

    # Compute the required Fisher's z for the correlation
    z_required <- z_power / sqrt(n - 3) + atanh(r_crit)

    # Initial estimate without correction
    r_initial <- tanh(z_required)

    # Apply inverse correction for small samples (approximate)
    r_corrected <- r_initial - r_initial / (2 * (n - 1))

    # One step of fixed-point iteration to improve accuracy
    z_corrected <- atanh(r_corrected) + r_corrected / (2 * (n - 1))
    r_final <- tanh(z_required - r_corrected / (2 * (n - 1)))

    return(r_final)
  }


#' Calculate Prediction Interval for a Correlation Coefficient
#'
#' This function calculates the prediction interval for a correlation coefficient
#' based on the Fisher's Z-transformation. It uses the approach proposed in the
#' supplementary materials of Patil et al. (2016).
#'
#' @param r Numeric. The correlation coefficient from the original study.
#' @param n_o Integer. The sample size of the original study.
#' @param n_r Integer. The sample size of the replication study. Defaults to the same as `n_o`.
#' @param confidence_level Numeric. The confidence level for the prediction interval. Defaults to 0.95.
#'
#' @return A named numeric vector with two elements:
#' \item{lower}{The lower bound of the prediction interval.}
#' \item{upper}{The upper bound of the prediction interval.}
#'
#' @references
#' Patil, P., Peng, R. D., & Leek, J. T. (2016). A statistical framework for enhancing
#' reproducibility in data analysis. *Perspectives on Psychological Science, 11*(5), 633-640.
#' \url{https://doi.org/10.1177/1745691616646366}
#'
#' @examples
#' # Example usage:
#' r <- 0.5   # Correlation coefficient
#' n_o <- 30  # Sample size of the original study
#' n_r <- 30  # Sample size of the replication study
#'
#' calculate_prediction_interval(r, n_o, n_r)


calculate_prediction_interval <- function(r, n_o, n_r = n_o, confidence_level = 0.95) {

  # Approach based on formula proposed in supplementary materials of Patil et al. (2016, https://doi.org/10.1177/1745691616646366)

  # Fisher's Z-transformation
  Z <- 0.5 * log((1 + r) / (1 - r))

  # Standard error for prediction interval
  SE_PI <- sqrt(1 / (n_o - 3) + 1 / (n_r - 3))

  # Critical value for the normal distribution
  alpha <- 1 - confidence_level
  z_alpha <- qnorm(1 - alpha / 2)

  # Prediction interval in Z-space
  Z_lower <- Z - z_alpha * SE_PI
  Z_upper <- Z + z_alpha * SE_PI

  # Inverse Z-transformation to get the prediction interval in r-space
  r_lower <- (exp(2 * Z_lower) - 1) / (exp(2 * Z_lower) + 1)
  r_upper <- (exp(2 * Z_upper) - 1) / (exp(2 * Z_upper) + 1)

  return(list(lower = r_lower, upper = r_upper))

}

#' Equivalence Test for a Correlation
#'
#' This function performs an equivalence test for a given correlation \( r \) against a
#' specified smallest effect size of interest (SESOI). The test assesses whether the
#' observed correlation \( r \) is statistically equivalent to a range defined by the SESOI.
#'
#' The logic behind the test is based on the Two One-Sided Tests (TOST) procedure.
#' The function tests two null hypotheses: (1) that the true correlation is less than or
#' equal to the lower bound of the SESOI, and (2) that the true correlation is greater than
#' or equal to the upper bound of the SESOI. If both null hypotheses can be rejected
#' (i.e., if the p-value is less than the significance level \code{alpha}), it is concluded
#' that the correlation is equivalent to the SESOI.
#'
#' @param r Numeric. The observed correlation.
#' @param n Integer. The sample size.
#' @param sesoi Numeric. The smallest effect size of interest (SESOI) for the correlation.
#'   The SESOI is symmetric around zero, so the function tests equivalence within the range
#'   \code{-sesoi} to \code{sesoi}.
#' @param alpha Numeric. The significance level for the test. Default is 0.05.
#'
#' @return Numeric. The p-value for the equivalence test. A p-value less than the significance
#'   level \code{alpha} indicates that the observed correlation is equivalent to the SESOI.
#'   When the return value is printed, a full sentence detailing the result is provided.
#'
#' @examples
#' r <- 0.3
#' n <- 100
#' sesoi <- 0.1
#' alpha <- 0.05
#' test_equivalence_r(r, n, sesoi, alpha)
#'
#' @export

# ToDo: format r values properly, without leading 0

test_equivalence_r <- function(r, n, sesoi, alpha = 0.05) {

  # Fisher z-transformation of the correlation
  z_r <- atanh(r)

  # Standard error for the z-transformed correlation
  SE_z <- 1 / sqrt(n - 3)

  # Fisher z-transformation for the SESOI bounds
  z_low <- atanh(-sesoi)
  z_high <- atanh(sesoi)

  # Test statistic for the lower bound
  t_low <- (z_r - z_low) / SE_z
  p_low <- 1 - pnorm(t_low)

  # Test statistic for the upper bound
  t_high <- (z_high - z_r) / SE_z
  p_high <- 1 - pnorm(t_high)

  # The p-value for the equivalence test
  p_equivalence <- max(p_low, p_high)

  # Format the p-value for display
  formatted_p <- format.pval(p_equivalence, digits = 3, eps = .001)

  # Determine if the result is significant
  if (p_equivalence < alpha) {
    result_sentence <- paste0("The correlation of ", round(r, 3),
                             " is statistically equivalent to the SESOI (± ", sesoi,
                             ") with a p-value of ", formatted_p, ".")
  } else {
    result_sentence <- paste("The correlation of ", round(r, 3),
                             " is not statistically equivalent to the SESOI (± ", sesoi,
                             ") (p = ", formatted_p, ").")
  }

  # Attach attributes to the p-value
  attr(p_equivalence, "result_sentence") <- result_sentence
  class(p_equivalence) <- "fred_equivalence_test_result"

  return(p_equivalence)
}

#' Print Method for Equivalence Test Result
#'
#' This function is a custom print method for objects of class `equivalence_test_result`.
#' It prints a formatted sentence describing the results of the equivalence test.
#'
#' @param x An object of class `equivalence_test_result`.
#' @param ... Additional arguments (not used).
#'
#' @export

print.fred_equivalence_test_result <- function(x, ...) {
  cat(attr(x, "result_sentence"), "\n")
  invisible(x)
}

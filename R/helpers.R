#' Compute Confidence Intervals for Correlation Coefficient
#'
#' Computes the confidence intervals for correlation coefficients using the Fisher r-to-z transformation.
#'
#' @param r Numeric vector. The correlation coefficients.
#' @param n Integer vector. The sample sizes.
#' @param conf.level Numeric. The confidence level for the interval. Default is 0.95.
#'
#' @return A data frame with columns `r`, `n`, `lower`, and `upper` containing the confidence intervals.
#'
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

  # Fisher r-to-z transformation
  z <- 0.5 * log((1 + r) / (1 - r))

  # Standard error of z
  se <- ifelse(n <= 3, NA, 1 / sqrt(n - 3))

  # Z critical value for the confidence level
  alpha <- 1 - conf.level
  z_crit <- qnorm(1 - alpha / 2)

  # Confidence interval for z
  z_lower <- z - z_crit * se
  z_upper <- z + z_crit * se

  # Inverse Fisher z-to-r transformation
  r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
  r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)

  # Return the confidence intervals in a data frame
  data.frame(r = r, n = n, lower = r_lower, upper = r_upper)
}

# Example usage
r <- c(0.5, 0.3)
n <- c(30, 40)
conf.level <- 0.95
compute_ci_r(r, n, conf.level)

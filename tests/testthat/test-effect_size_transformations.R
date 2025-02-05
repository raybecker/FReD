# Test statistic transformations
library(testthat)

test_that("convert_effect_sizes correctly converts t(df) to r", {
  es_values <- c("t(10) = 2.5", "t(30) = -1.8", "t(5) = 0")
  es_types <- c("test statistic", "test statistic", "test statistic")

  expected_r <- c(
    2.5 / sqrt(2.5^2 + 10),  # Matches output from effectsize::t_to_r(2.5, 10)
    -1.8 / sqrt((-1.8)^2 + 30),
    0 / sqrt(0^2 + 5)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts F(df1=1, df2) to r", {
  es_values <- c("F(1, 20) = 4.5", "F(1, 50) = 2.0")
  es_types <- c("test statistic", "test statistic")

  expected_r <- c(
    sqrt(4.5) / sqrt((sqrt(4.5))^2 + 20), # Matches output from effectsize::F_to_r(4.5, 1, 20)
    sqrt(2.0) / sqrt((sqrt(2.0))^2 + 50)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes returns NA for F(df1 > 1, df2) and F < 1", {
  es_values <- c("F(2, 15) = 3.2", "F(3, 40) = 5.1", "F < 1")
  es_types <- c("test statistic", "test statistic", "test statistic")

  result <- convert_effect_sizes(es_values, es_types)
  expect_true(all(is.na(result)))  # Should return NA for non-convertible F-tests
})

test_that("convert_effect_sizes returns NA for invalid test statistic formats", {
  es_values <- c("t(df) = 2.5", "F(10) = 4.5", "f(1,20) 4.5", "random text")
  es_types <- c("test statistic", "test statistic", "test statistic", "test statistic")

  result <- convert_effect_sizes(es_values, es_types)
  expect_true(all(is.na(result)))  # All should fail conversion
})

test_that("convert_effect_sizes converts z-scores with N", {
  es_values <- c(" z = 2.81, N = 34", "z = 3.1, N = 68")
  es_types <- c("test statistic", "test statistic")

  expected_r <- c(
    2.81 / sqrt(2.81^2 + 34), # Matches output from effectsize::z_to_r(2.81, 34)
    3.1 / sqrt(3.1^2 + 68)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

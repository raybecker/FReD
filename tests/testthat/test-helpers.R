test_that("power functions work", {
  expect_equal(.4, compute_power_r(find_r_for_power(.4, 100, .05), 100, .05), tolerance = 1e-5)
  # As small sample correction cannot be analytically reversed, we need to use a larger tolerance here - but 3 identical decimal digits are still ok.
  expect_equal(.4, compute_power_r(find_r_for_power(.4, 10, .05), 10, .05), tolerance = 1e-3)
})

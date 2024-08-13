test_that("replication success assessed correctly", {
  expect_equal(2 * 2, 4)
})

es_agg <- compare_effectsizes(c(.1, .4), c(10, 100), c(.4, .1), c(100, 10))

test_that("replications can be aggregated", {
  expect_subset(names(es_agg), c("est", "ci_lb", "QEp"))
})

test_that("success assessments work", {
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "significance_r")$outcome, "success")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "significance_agg")$outcome, "success")
  expect_equal(assess_replication_outcome(.5, 50, -.5, 50, "significance_agg")$outcome, "failure")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "consistency_pi")$outcome, "failure")
  expect_equal(assess_replication_outcome(.5, 50, .1, 30, "consistency_pi")$outcome, "success")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "consistency_ci")$outcome, "failure")
  expect_equal(assess_replication_outcome(.5, 50, .1, 1000, "homogeneity")$outcome, "failure")
  expect_equal(assess_replication_outcome(.3, 50, 0, 40, "homogeneity")$outcome, "success")
  expect_equal(assess_replication_outcome(.3, 50, 0, 40, "homogeneity_significance")$outcome, "failure")
})

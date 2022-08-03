test_that("adding irs works", {
  interventions <- example_site$interventions
  interventions$irs_cov[1:10] <- 0.5

  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p1 <- add_irs(
    p = p0,
    interventions = interventions)

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_timesteps, round(-(3 * 365 / 12) + 1 + (interventions$year - p0$baseline_year) * 365))
  expect_equal(p1$spraying_coverages, interventions$irs_cov)
  expect_equal(p1$spraying_ls_theta, matrix(interventions$ls_theta, ncol = 1))
  expect_equal(p1$spraying_ls_gamma, matrix(interventions$ls_gamma, ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(interventions$ks_theta, ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(interventions$ks_theta, ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(interventions$ms_theta, ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(interventions$ms_theta, ncol = 1))
})

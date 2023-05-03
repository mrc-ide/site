test_that("LSM works", {
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  interventions$lsm_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()

  p0$baseline_year <- 2000
  p1 <- add_lsm(
    p = p0,
    interventions = interventions)

  expect_equal(p1$larval_source_management, TRUE)
  expect_equal(p1$lsm_coverages, matrix(c(rep(0.5, 10), rep(0, 13)), ncol = 1))
  expect_equal(p1$lsm_timesteps, 1 + (interventions$year - p1$baseline_year) * 365)
})


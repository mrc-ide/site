test_that("stephensi works", {
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  interventions$stephensi_scaler[1:10] <- 2
  p0 <- malariasimulation::get_parameters()

  p0$baseline_year <- 2000
  p1 <- add_stephensi_invasion(
    p = p0,
    interventions = interventions)

  expect_equal(p1$rescale_carrying_capacity, TRUE)
  expect_equal(p1$rcc_scalers, matrix(c(rep(2, 10), rep(1, 13)), ncol = 1))
  expect_equal(p1$rcc_timesteps, 1 + (interventions$year - p1$baseline_year) * 365)
})

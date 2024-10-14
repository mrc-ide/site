test_that("Carrying capacity works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  interventions$lsm_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000

  p1 <- p0 |>
    adjust_carrying_capacity(
      interventions = interventions
    )

  expect_true(p1$carrying_capacity)
  expect_equal(p1$carrying_capacity_scalers, matrix(1 - interventions$lsm_cov, ncol = 1))
  expect_equal(p1$carrying_capacity_timesteps, 1 + (interventions$year - p1$baseline_year) * 365)
})

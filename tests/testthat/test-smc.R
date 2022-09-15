test_that("adding smc works", {
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  interventions$smc_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()

  p0$baseline_year <- 2000
  p1 <- add_smc(
    p = p0,
    interventions = interventions)

  month <- 365 / 12

  peak <- malariasimulation::peak_season_offset(p1)
  rounds <- interventions$smc_n_rounds
  year_start_times <-  1 + (interventions$year - p1$baseline_year) * 365
  peak_season_times <- peak + year_start_times
  # Assume middle of rounds occurs at peak season:
  round_relative_time <- as.vector(
    unlist(
      sapply(rounds, function (x) {seq(-x * month / 2, x * month / 2, length.out = x)})
    )
  )
  timesteps <- round(round_relative_time) + rep(peak_season_times, rounds)
  index <- timesteps < 0
  timesteps <- timesteps[!index]

  expect_equal(p1$smc, TRUE)
  expect_equal(p1$smc_coverages, rep(interventions$smc_cov, rounds)[!index])
  expect_equal(p1$smc_drug, 3)
  expect_equal(p1$smc_min_age, rep(interventions$smc_min_age, rounds)[!index])
  expect_equal(p1$smc_max_age, rep(interventions$smc_max_age, rounds)[!index])
  expect_equal(p1$smc_timesteps, timesteps)

  interventions$smc_drug <- "n"
  expect_error(add_smc(p = p1, interventions = interventions), "Not currently set up for non SP AQ SMC drug")
})

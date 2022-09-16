test_that("adding irs works", {
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  interventions$irs_cov[1:10] <- 0.5

  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p0 <- add_seasonality(p0, example_site$seasonality)
  p0$g <- p0$g + 5
  p1 <- add_irs(
    p = p0,
    interventions = interventions)

  month <- 365 / 12
  peak <- malariasimulation::peak_season_offset(p1)
  year_start_times <-  1 + (example_site$interventions$year - p1$baseline_year) * 365
  peak_season_times <- peak + year_start_times
  # Assume IRS occurs 3 months before seasonal peak
  timesteps <- round(peak_season_times - 3 * month)
  index <- timesteps < 0
  timesteps <- timesteps[!index]

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_timesteps, timesteps)
  expect_equal(p1$spraying_coverages, interventions$irs_cov[!index])
  expect_equal(p1$spraying_ls_theta, matrix(interventions$ls_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ls_gamma, matrix(interventions$ls_gamma[!index], ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(interventions$ks_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(interventions$ks_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(interventions$ms_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(interventions$ms_theta[!index], ncol = 1))

  # With negative timestep early relative to peak
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  interventions$irs_cov[1:10] <- 0.5

  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p0 <- add_seasonality(p0, example_site$seasonality)
  p1 <- add_irs(
    p = p0,
    interventions = interventions)

  month <- 365 / 12
  peak <- malariasimulation::peak_season_offset(p1)
  year_start_times <-  1 + (example_site$interventions$year - p1$baseline_year) * 365
  peak_season_times <- peak + year_start_times
  # Assume IRS occurs 3 months before seasonal peak
  timesteps <- round(peak_season_times - 3 * month)
  index <- timesteps < 0
  timesteps <- timesteps[!index]

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_timesteps, timesteps)
  expect_equal(p1$spraying_coverages, interventions$irs_cov[!index])
  expect_equal(p1$spraying_ls_theta, matrix(interventions$ls_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ls_gamma, matrix(interventions$ls_gamma[!index], ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(interventions$ks_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(interventions$ks_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(interventions$ms_theta[!index], ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(interventions$ms_theta[!index], ncol = 1))

})

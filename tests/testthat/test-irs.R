test_that("adding irs works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  interventions$irs_cov[1:10] <- 0.5

  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p0 <- add_seasonality(p0, single_site$seasonality$seasonality_parameters)
  p0$g <- p0$g + 5
  p1 <- add_irs(
    p = p0,
    interventions = interventions
  )

  month <- 365 / 12
  peak <- malariasimulation::peak_season_offset(p1)
  year_start_times <-  1 + (single_site$interventions$year - p1$baseline_year) * 365
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
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  interventions$irs_cov[1:10] <- 0.5

  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p0 <- add_seasonality(p0, single_site$seasonality$seasonality_parameters)
  p1 <- add_irs(
    p = p0,
    interventions = interventions
  )

  month <- 365 / 12
  peak <- malariasimulation::peak_season_offset(p1)
  year_start_times <-  1 + (single_site$interventions$year - p1$baseline_year) * 365
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

test_that("adding irs multiple rounds works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  interventions$irs_spray_rounds[nrow(interventions)] <- 2
  interventions$irs_cov[1:23] <- 0.5

  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p0 <- add_seasonality(p0, single_site$seasonality$seasonality_parameters)
  p0$g <- p0$g + 5
  p1 <- add_irs(
    p = p0,
    interventions = interventions
  )

  month <- 365 / 12
  peak <- malariasimulation::peak_season_offset(p1)
  year_start_times <-  1 + (single_site$interventions$year - p1$baseline_year) * 365
  peak_season_times <- peak + year_start_times
  # Assume IRS occurs 3 months before seasonal peak
  timesteps <- round(peak_season_times - 3 * month)
  timesteps <- round(c(timesteps, peak_season_times[length(peak_season_times)] + 3 * month))
  index <- timesteps < 0
  timesteps <- timesteps[!index]

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_timesteps, timesteps)
  expect_equal(p1$spraying_coverages, rep(interventions$irs_cov, interventions$irs_spray_rounds)[!index])
  expect_equal(p1$spraying_ls_theta, matrix(rep(interventions$ls_theta, interventions$irs_spray_rounds)[!index], ncol = 1))
  expect_equal(p1$spraying_ls_gamma, matrix(rep(interventions$ls_gamma, interventions$irs_spray_rounds)[!index], ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(rep(interventions$ks_theta, interventions$irs_spray_rounds)[!index], ncol = 1))
  expect_equal(p1$spraying_ks_theta, matrix(rep(interventions$ks_theta, interventions$irs_spray_rounds)[!index], ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(rep(interventions$ms_theta, interventions$irs_spray_rounds)[!index], ncol = 1))
  expect_equal(p1$spraying_ms_theta, matrix(rep(interventions$ms_theta, interventions$irs_spray_rounds)[!index], ncol = 1))
})

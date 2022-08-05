test_that("adding rtss works", {
  interventions <- example_site$interventions
  interventions$rtss_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p1 <- add_rtss(
    p = p0,
    interventions = interventions)

  month <- 365 / 12
  expect_equal(p1$rtss_epi_age, round(6 * month))
  expect_equal(p1$rtss_epi_booster_coverage, 0.8)
  expect_equal(p1$rtss_epi_boosters, round(18 * month))
  expect_equal(p1$rtss_epi_timesteps, 1 + (365 * (interventions$year - p0$baseline_year)))
  expect_equal(p1$rtss_epi_coverages, interventions$rtss_cov)
  expect_equal(p1$rtss_epi_min_wait, 0)
  expect_equal(p1$rtss_epi_seasonal_boosters, FALSE)
})

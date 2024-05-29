test_that("adding rtss works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  interventions$rtss_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p1 <- add_rtss(
    p = p0,
    interventions = interventions
  )

  month <- 365 / 12
  expect_equal(p1$pev_epi_age, round(6 * month))
  expect_equal(p1$pev_epi_booster_coverage, matrix(rep(0.8, length(p1$pev_epi_timesteps))))
  expect_equal(p1$pev_epi_booster_spacing, round(12 * month))
  expect_equal(p1$pev_epi_timesteps, 1 + (365 * (interventions$year - p0$baseline_year)))
  expect_equal(p1$pev_epi_coverages, interventions$rtss_cov)
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
})

test_that("adding r21 works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  interventions$r21_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p1 <- add_r21(
    p = p0,
    interventions = interventions
  )

  month <- 365 / 12
  expect_equal(p1$pev_epi_age, round(6 * month))
  expect_equal(p1$pev_epi_booster_coverage, matrix(rep(0.8, length(p1$pev_epi_timesteps))))
  expect_equal(p1$pev_epi_booster_spacing, round(12 * month))
  expect_equal(p1$pev_epi_timesteps, 1 + (365 * (interventions$year - p0$baseline_year)))
  expect_equal(p1$pev_epi_coverages, interventions$r21_cov)
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
})

test_that("adding r21 and rtss fails as expected", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  interventions$rtss_cov[1:10] <- 0.5
  interventions$r21_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  expect_warning(
    p1 <- add_interventions(
      p = p0,
      interventions = interventions,
      species = "pf"
    )
  )
})

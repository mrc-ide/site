test_that("adding (static) demography works", {
  example_site <- single_site(example_site, 1)
  demography <- example_site$demography
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p1 <- add_demography(p = p0, demography = demography)

  expect_equal(p1$deathrate_agegroups, round(unique(demography$age_upper) * 365))
  expect_equal(p1$deathrate_timesteps, 365 * (unique(demography$year) - p0$baseline_year))
  expect_equal(p1$deathrates,
               matrix(demography$mortality_rate / 365,
                      nrow = length(unique(demography$year)),
                      byrow = TRUE))
})

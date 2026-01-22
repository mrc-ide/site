example_demography <- data.frame(
  name = "place",
  year = rep(2000:2002, each = 2),
  age_lower = rep(c(0, 10), 3),
  age_upper = rep(c(10, 100), 3),
  adjusted_mortality_rates = rep(c(0.01, 0.001), 3)
)
p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("adding (static) demography works", {
  p1 <- add_demography(p = p0, demography = example_demography)

  expect_equal(
    p1$deathrate_agegroups,
    round(unique(example_demography$age_upper) * 365)
  )
  expect_equal(
    p1$deathrate_timesteps,
    c(
      0,
      calendar_to_timestep(
        year = unique(example_demography$year),
        start_year = p0$start_year
      )
    )
  )
  rates <- matrix(
    example_demography$adjusted_mortality_rates / 365,
    nrow = length(unique(example_demography$year)),
    byrow = TRUE
  )
  expect_equal(
    p1$deathrates,
    rbind(rates[1, ], rates)
  )
})

test_that("calculate_total_timesteps works correctly", {
  # Basic functionality
  expect_equal(calculate_total_timesteps(2000, 2000, 0), 365)
  expect_equal(calculate_total_timesteps(2000, 2001, 0), 730)
  expect_equal(calculate_total_timesteps(2000, 2020, 20), 14965) # 41 years * 365
  expect_equal(calculate_total_timesteps(2000, 2005, 10), 5840) # 16 years * 365

  # Edge cases
  expect_equal(calculate_total_timesteps(2000, 2000, 0), 365) # Single year, no burnin
  expect_equal(calculate_total_timesteps(1980, 1980, 50), 18615) # Single year, long burnin

  # Input validation - should error
  expect_error(calculate_total_timesteps("2000", 2001, 0), "must be numeric")
  expect_error(calculate_total_timesteps(2000, "2001", 0), "must be numeric")
  expect_error(calculate_total_timesteps(2000, 2001, "0"), "must be numeric")
  expect_error(
    calculate_total_timesteps(c(2000, 2001), 2002, 0),
    "must be scalars"
  )
  expect_error(
    calculate_total_timesteps(2000, c(2001, 2002), 0),
    "must be scalars"
  )
  expect_error(
    calculate_total_timesteps(2000, 2001, c(0, 1)),
    "must be scalars"
  )
  expect_error(
    calculate_total_timesteps(2001, 2000, 0),
    "end_year must be >= start_year"
  )
  expect_error(calculate_total_timesteps(2000, 2001, -1), "burnin must be >= 0")
  expect_error(calculate_total_timesteps(NA, 2001, 0), "must be numeric")
  expect_error(calculate_total_timesteps(2000, Inf, 0), "must be finite")
})

test_that("calendar_to_timestep works correctly", {
  # Basic functionality
  expect_equal(calendar_to_timestep(2000, 1, 2000), 1) # Day 1 of start year
  expect_equal(calendar_to_timestep(2000, 365, 2000), 365) # Last day of start year
  expect_equal(calendar_to_timestep(2001, 1, 2000), 366) # First day of next year
  expect_equal(calendar_to_timestep(2000, 1, 1980), 7301) # 20 years later

  # First day inferred
  expect_equal(calendar_to_timestep(year = 2000, start_year = 2000), 1) # Day 1 of start year
  expect_equal(
    calendar_to_timestep(year = 2000:2001, start_year = 2000),
    c(1, 366)
  ) # Day 1 of start year

  # Vectorized input
  result <- calendar_to_timestep(c(2000, 2001), c(1, 100), 2000)
  expect_equal(result, c(1, 465))

  result <- calendar_to_timestep(c(2000, 2001, 2005), c(1, 100, 200), 1980)
  expect_equal(result, c(7301, 7765, 9325))

  # Edge cases
  expect_equal(calendar_to_timestep(2000, 1, 2000), 1) # Same year, first day
  expect_equal(calendar_to_timestep(2000, 365, 2000), 365) # Same year, last day

  # Input validation - should error
  expect_error(calendar_to_timestep("2000", 1, 2000), "must be numeric")
  expect_error(calendar_to_timestep(2000, "1", 2000), "must be numeric")
  expect_error(calendar_to_timestep(2000, 1, "2000"), "must be numeric")
  expect_error(
    calendar_to_timestep(c(2000, 2001), 2000, 1),
    "year and day_of_year must"
  )
  expect_error(calendar_to_timestep(c(2000, 2001), 1, 2000), "same length")
  expect_error(calendar_to_timestep(2000, c(1, 2), 2000), "same length")
  expect_error(calendar_to_timestep(2000, 0, 2000), "between 1 and 365")
  expect_error(calendar_to_timestep(2000, 366, 2000), "between 1 and 365")
  expect_error(calendar_to_timestep(1999, 1, 2000), "must be >= start_year")
  expect_error(calendar_to_timestep(NA, 1, 2000), "must be numeric")
  expect_error(calendar_to_timestep(2000, NA, 2000), "must be numeric")
  expect_error(calendar_to_timestep(Inf, 1, 2000), "must be finite")
})

test_that("functions work together correctly", {
  # Integration test: total timesteps should accommodate converted dates
  start_year <- 2000
  end_year <- 2020
  burnin <- 20
  model_start_year <- start_year - burnin

  total_steps <- calculate_total_timesteps(start_year, end_year, burnin)

  # First intervention timestep
  first_intervention <- calendar_to_timestep(start_year, 1, model_start_year)
  expect_equal(first_intervention, burnin * 365 + 1)

  # Last possible timestep
  last_timestep <- calendar_to_timestep(end_year, 365, model_start_year)
  expect_equal(last_timestep, total_steps)

  # Timesteps should be within valid range
  expect_true(first_intervention <= total_steps)
  expect_true(last_timestep <= total_steps)
})

test_that("edge cases and boundary conditions", {
  # Zero burnin
  expect_equal(calculate_total_timesteps(2000, 2000, 0), 365)
  expect_equal(calendar_to_timestep(2000, 1, 2000), 1)

  # Large numbers
  expect_equal(calculate_total_timesteps(2000, 2000, 100), 36865) # 101 years
  expect_equal(calendar_to_timestep(2100, 1, 2000), 36501) # 100 years later

  # Boundary day values
  expect_equal(calendar_to_timestep(2000, 1, 2000), 1)
  expect_equal(calendar_to_timestep(2000, 365, 2000), 365)
})

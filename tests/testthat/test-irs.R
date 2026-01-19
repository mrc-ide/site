example_irs <- list(
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    irs_cov = 0.1,
    peak_season = 100,
    insecticide = c("ddt", "actellic", "sumishield"),
    round = 1,
    spray_day_of_year = 70
  )
)
p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("adding irs works", {
  p1 <- add_irs(
    p = p0,
    irs = example_irs,
    irs_adjust = 1
  )

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_coverages, example_irs$implementation$irs_cov)
  expect_equal(
    p1$spraying_timesteps,
    calendar_to_timestep(
      year = example_irs$implementation$year,
      day_of_year = example_irs$implementation$spray_day_of_year,
      start_year = p0$start_year
    )
  )
})

test_that("IRS insecticide is checked", {
  wrong_insecticide_example_irs <- example_irs
  wrong_insecticide_example_irs$implementation$insecticide[
    1
  ] <- "unsupported_insecticide"
  expect_error(
    p1 <- add_irs(
      p = p0,
      irs = wrong_insecticide_example_irs,
      irs_adjust = 1
    ),
    "Unsupported insecticide.*found.*unsupported_insecticide"
  )
})

test_that("IRS adjustment works", {
  p1 <- add_irs(
    p = p0,
    irs = example_irs,
    irs_adjust = 0.5
  )

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_coverages, example_irs$implementation$irs_cov * 0.5)
  expect_equal(
    p1$spraying_timesteps,
    calendar_to_timestep(
      year = example_irs$implementation$year,
      day_of_year = example_irs$implementation$spray_day_of_year,
      start_year = p0$start_year
    )
  )
})

test_that("IRS adjustment errors if out of bounds", {
  expect_error(
    p1 <- add_irs(
      p = p0,
      irs = example_irs,
      irs_adjust = -1
    ),
    "irs_adjust must be between 0 and 1"
  )
  expect_error(
    p1 <- add_irs(
      p = p0,
      irs = example_irs,
      irs_adjust = 2
    ),
    "irs_adjust must be between 0 and 1"
  )
})

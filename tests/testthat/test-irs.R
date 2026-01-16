example_irs <- list(
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    irs_cov = 0.1,
    peak_season = 100,
    insecticide = c("ddt", "actellic", "sumishield"),
    round = 1,
    spray_day = (2000:2002 - 2000) * 365 + 70
  )
)
p0 <- malariasimulation::get_parameters()

test_that("adding irs works", {
  p1 <- add_irs(
    p = p0,
    irs = example_irs,
    irs_adjust = 1
  )

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_timesteps, example_irs$implementation$spray_day)
  expect_equal(p1$spraying_coverages, example_irs$implementation$irs_cov)
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

test_that("Negative spray days are appropriately droppped", {
  negative_example_irs <- example_irs
  negative_example_irs$implementation$spray_day[1] <- -1
  p0 <- malariasimulation::get_parameters()
  p1 <- add_irs(
    p = p0,
    irs = negative_example_irs,
    irs_adjust = 1
  )

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_timesteps, example_irs$implementation$spray_day[-1])
  expect_equal(p1$spraying_coverages, example_irs$implementation$irs_cov[-1])
})

test_that("IRS adjustment works", {
  p1 <- add_irs(
    p = p0,
    irs = example_irs,
    irs_adjust = 0.5
  )

  expect_equal(p1$spraying, TRUE)
  expect_equal(p1$spraying_timesteps, example_irs$implementation$spray_day)
  expect_equal(p1$spraying_coverages, example_irs$implementation$irs_cov * 0.5)
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

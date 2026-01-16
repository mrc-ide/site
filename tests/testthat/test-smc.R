example_smc <- list(
  drug = "sp_aq",
  implementation = data.frame(
    name = "place",
    year = rep(2000:2002, each = 2),
    smc_cov = 0.1,
    peak_season = 45,
    smc_min_age = 91,
    smc_max_age = 1825,
    round = rep(1:2, 3),
    round_day = (rep(2000:2002, each = 2) - 2000) * 365 + rep(c(30, 60), 3)
  )
)

test_that("Adding SMC correctly modifies the parameter list", {
  p0 <- malariasimulation::get_parameters()
  p1 <- add_smc(
    p = p0,
    smc = example_smc
  )
  expect_equal(p1$smc, TRUE)
  expect_equal(
    p1$smc_coverages,
    example_smc$implementation$smc_cov
  )
  expect_equal(p1$smc_drug, 3)
  expect_equal(
    p1$smc_min_age,
    example_smc$implementation$smc_min_age
  )
  expect_equal(
    p1$smc_max_age,
    example_smc$implementation$smc_max_age
  )
  expect_equal(
    p1$smc_timesteps,
    example_smc$implementation$round_day
  )
})

test_that("SPAQ drug check is informative", {
  wrong_drug_example_smc <- example_smc
  wrong_drug_example_smc$drug <- "n"
  expect_error(
    add_smc(p = p0, smc = wrong_drug_example_smc),
    "SMC drug must be sp_aq"
  )
})

test_that("Negative round days are appropriately droppped", {
  negative_days_example_smc <- example_smc
  negative_days_example_smc$implementation$round_day[1] <- -1
  p0 <- malariasimulation::get_parameters()
  p1 <- add_smc(
    p = p0,
    smc = negative_days_example_smc
  )
  expect_equal(p1$smc, TRUE)
  expect_equal(
    p1$smc_coverages,
    example_smc$implementation$smc_cov[-1]
  )
  expect_equal(p1$smc_drug, 3)
  expect_equal(
    p1$smc_min_age,
    example_smc$implementation$smc_min_age[-1]
  )
  expect_equal(
    p1$smc_max_age,
    example_smc$implementation$smc_max_age[-1]
  )
  expect_equal(
    p1$smc_timesteps,
    example_smc$implementation$round_day[-1]
  )
})

example_smc <- create_example_smc()

p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("Adding SMC correctly modifies the parameter list", {
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
    calendar_to_timestep(
      year = example_smc$implementation$year,
      day_of_year = example_smc$implementation$round_day_of_year,
      start_year = p0$start_year
    )
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

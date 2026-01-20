example_pmc <- list(
  drug = "sp",
  age = c(60, 90, 270),
  implementation = data.frame(
    name = "place",
    year = 2000:2003,
    pmc_coverage_timesteps = ((2000:2003) - 2000) * 365 + 1,
    pmc_cov = 0.1
  )
)

p0 <- malariasimulation::get_parameters() |>
  add_drugs()
p0$start_year <- 2000

test_that("Adding PMC correctly modifies the parameter list", {
  p1 <- add_pmc(
    p = p0,
    pmc = example_pmc
  )

  expect_true(p1$pmc)
  expect_equal(p1$pmc_drug, 1)
  expect_equal(p1$pmc_ages, example_pmc$age)
  expect_equal(p1$pmc_coverages, example_pmc$implementation$pmc_cov)
  expect_equal(
    p1$pmc_timesteps,
    calendar_to_timestep(
      year = example_pmc$implementation$year,
      start_year = p0$start_year
    )
  )
})

test_that("SP drug check is informative", {
  wrong_drug_example_pmc <- example_pmc
  wrong_drug_example_pmc$drug <- "n"
  expect_error(
    add_pmc(p = p0, pmc = wrong_drug_example_pmc),
    "PMC drug must be sp"
  )
})

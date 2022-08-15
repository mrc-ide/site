test_that("PMC works", {
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  interventions$pmc_cov[1:10] <- 0.5
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000

  p1 <- p0 |>
    add_drugs() |>
    add_pmc(interventions = interventions)

  expect_true(p1$pmc)
  expect_equal(p1$pmc_drug, 1)
  expect_equal(p1$pmc_ages, c(2, 3, 9) * 30)
  expect_equal(p1$pmc_coverages, interventions$pmc_cov)
  expect_equal(p1$pmc_timesteps, 1 + (interventions$year - p1$baseline_year) * 365)

  interventions$pmc_drug <- "n"
  expect_error(add_pmc(p = p1, interventions = interventions), "Not currently set up for non SP PMC drug")
})

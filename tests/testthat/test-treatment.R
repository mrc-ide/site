test_that("adding treatment works", {
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  # Drug types
  p0 <- add_drugs(p0)

  p1 <- add_treatment(p = p0,
                      interventions = interventions)

  expect_equal(p1$clinical_treatment_drugs, list(4, 5))

  expect_equal(p1$clinical_treatment_coverages[[1]], interventions$tx_cov * (1 - interventions$prop_act))
  expect_equal(p1$clinical_treatment_coverages[[2]], interventions$tx_cov * interventions$prop_act)


  expect_equal(p1$clinical_treatment_timesteps,
               list(
                 1 + 365 * (interventions$year - p0$baseline_year),
                 1 + 365 * (interventions$year - p0$baseline_year))
  )
})


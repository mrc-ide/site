test_that("multiplication works", {
  example_site <- single_site(example_site, 1)

  p0 <- list()
  interventions <- example_site$interventions
  p1 <- add_time(p = p0, interventions = interventions)
  expect_equal(p1$baseline_year, min(interventions$year))
  expect_equal(p1$timesteps, 365 * (1 + max(interventions$year) - min(interventions$year)))
})

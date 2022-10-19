test_that("burnin works", {
  bi <- 10
  example_site <- single_site(example_site, 1)

  interventions <- burnin_interventions(example_site$interventions, bi)
  expect_equal(nrow(interventions), nrow(example_site$interventions) + bi)
  expect_equal(sum(is.na(interventions)), 0)

  demography <- burnin_demography(example_site$demography, bi)
  expect_equal(nrow(demography), nrow(example_site$demography) + bi * sum(example_site$demography$year == min(example_site$demography$year)))
  expect_equal(sum(is.na(demography)), 0)

  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    burnin = bi
  )
  expect_identical(p$burnin, bi)
})

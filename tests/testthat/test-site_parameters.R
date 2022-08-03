test_that("site parameters wrapper works", {
  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality
  )
  expect_type(p, "list")

  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    eir = 10
  )
  expect_type(p, "list")
})

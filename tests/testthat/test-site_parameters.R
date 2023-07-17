test_that("site parameters wrapper works", {
  example_site <- single_site(example_site, 1)
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


test_that("setting vivax works", {
  example_site <- single_site(example_site, 1)
  example_site$interventions$rtss_cov <- 0.1
  example_site$interventions$pmc_cov <- 0.1
  example_site$interventions$smc_cov <- 0.1

  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    species = "pv"
  )
  expect_false(p$pev)
  expect_false(p$smc)
  expect_false(p$pmc)
})

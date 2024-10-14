test_that("site parameters wrapper works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  p <- site_parameters(
    interventions = single_site$interventions,
    demography = single_site$demography,
    vectors = single_site$vectors$vector_species,
    seasonality = single_site$seasonality$seasonality_parameters
  )
  expect_type(p, "list")

  p <- site_parameters(
    interventions = single_site$interventions,
    demography = single_site$demography,
    vectors = single_site$vectors$vector_species,
    seasonality = single_site$seasonality$seasonality_parameters,
    eir = 10
  )
  expect_type(p, "list")
})


test_that("setting vivax works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  single_site$interventions$rtss_cov <- 0.1
  single_site$interventions$pmc_cov <- 0.1
  single_site$interventions$smc_cov <- 0.1

  p <- site_parameters(
    interventions = single_site$interventions,
    demography = single_site$demography,
    vectors = single_site$vectors$vector_species,
    seasonality = single_site$seasonality$seasonality_parameters,
    species = "pv"
  )
  expect_false(p$pev)
  expect_false(p$smc)
  expect_false(p$pmc)
})

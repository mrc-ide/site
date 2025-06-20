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

test_that("setting parameter draw works for falciparum", {

  single_site <- subset_site(example_site, example_site$eir[1,])

  p <- site_parameters(
    interventions = single_site$interventions,
    demography = single_site$demography,
    vectors = single_site$vectors$vector_species,
    seasonality = single_site$seasonality$seasonality_parameters,
    species = "pf",
    eir = 10,
    draw = 1000
  )

  parameter_names_pf <- names(malariasimulation::parameter_draws_pf[[1000]])

  expect_identical(p[parameter_names_pf], parameter_draws_pf[[1000]][parameter_names_pf])

})

test_that("parameter draw functionality switched off for vivax", {

  single_site <- subset_site(example_site, example_site$eir[1,])

  p <- site_parameters(
    interventions = single_site$interventions,
    demography = single_site$demography,
    vectors = single_site$vectors$vector_species,
    seasonality = single_site$seasonality$seasonality_parameters,
    species = "pv",
    eir = 10,
    draw = 10
  )

  parameter_names_pf <- names(parameter_draws_pf)

  expect_identical(get_parameters()[parameter_names_pf], p[parameter_names_pf])

})

test_that("set_equilibrium not called when eir argument left at default", {

  single_site <- subset_site(example_site, example_site$eir[1,])

  p <- site_parameters(
    interventions = single_site$interventions,
    demography = single_site$demography,
    vectors = single_site$vectors$vector_species,
    seasonality = single_site$seasonality$seasonality_parameters,
    species = "pf")

  expect_null(p$eq_params)
  expect_null(p$init_EIR)
  expect_identical(p$init_foim, 0)

})

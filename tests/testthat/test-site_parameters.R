example_site <- create_example_site()
usage_timestep <- calendar_to_timestep(
  year = example_site$interventions$itns$use$year,
  day_of_year = example_site$interventions$itns$use$usage_day_of_year,
  start_year = 2000
)
implementation_timestep <- calendar_to_timestep(
  year = example_site$interventions$itns$implementation$year,
  day_of_year = example_site$interventions$itns$implementation$distribution_day_of_year,
  start_year = 2000
)
example_site$interventions$itns$implementation$itn_input_dist <-
  netz::usage_to_model_distribution(
    usage = example_itns_complete$use$itn_use,
    usage_timesteps = usage_timestep,
    distribution_timesteps = implementation_timestep,
    distribution_lower = example_itns_complete$implementation$distribution_lower,
    distribution_upper = example_itns_complete$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = example_itns_complete$retention_half_life
  )

start_year <- 2000
end_year <- 2002

test_that("site parameters wrapper works", {
  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    start_year = start_year,
    end_year = end_year
  )
  expect_type(p, "list")

  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    eir = 10,
    start_year = start_year,
    end_year = end_year
  )
  expect_type(p, "list")
})

test_that("setting vivax works", {
  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    parasite = "vivax",
    start_year = start_year,
    end_year = end_year
  )
  expect_false(p$pev)
  expect_false(p$smc)
  expect_false(p$pmc)
})

test_that("setting parameter draw works for falciparum", {
  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    parasite = "falciparum",
    eir = 10,
    draw = 1000,
    start_year = start_year,
    end_year = end_year
  )

  parameter_names_pf <- names(malariasimulation::parameter_draws_pf[[1000]])

  expect_identical(
    p[parameter_names_pf],
    malariasimulation::parameter_draws_pf[[1000]][parameter_names_pf]
  )
})

test_that("set_equilibrium not called when eir argument left at default", {
  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    start_year = start_year,
    end_year = end_year
  )

  expect_null(p$eq_params)
  expect_null(p$init_EIR)
  expect_identical(p$init_foim, 0)
})

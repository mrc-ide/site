example_site <- create_example_site()
usage_timestep <- calendar_to_timestep(
year = example_site$interventions$itn$use$year,
day_of_year = example_site$interventions$itn$use$usage_day_of_year,
start_year = 2000
)
implementation_timestep <- calendar_to_timestep(
  year = example_site$interventions$itn$implementation$year,
  day_of_year = example_site$interventions$itn$implementation$distribution_day_of_year,
  start_year = 2000
)
example_site$interventions$itn$implementation$itn_input_dist <-
  netz::usage_to_model_distribution(
    usage = example_site$interventions$itn$use$itn_use,
    usage_timesteps = usage_timestep,
    distribution_timesteps = implementation_timestep,
    distribution_lower = example_site$interventions$itn$implementation$distribution_lower,
    distribution_upper = example_site$interventions$itn$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = example_site$interventions$itn$retention_half_life
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

test_that("default age outputs are set for falciparum", {
  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    start_year = start_year,
    end_year = end_year
  )

  # Check default age group outputs (0, 5, 15, 100 years in days)
  default_ages <- c(0, 5, 15, 100) * 365
  expected_min <- default_ages[-length(default_ages)]
  expected_max <- default_ages[-1] - 1

  expect_equal(p$age_group_rendering_min_ages, expected_min)
  expect_equal(p$age_group_rendering_max_ages, expected_max)

  # Check clinical incidence outputs
  expect_equal(p$clinical_incidence_rendering_min_ages, expected_min)
  expect_equal(p$clinical_incidence_rendering_max_ages, expected_max)

  # Check severe incidence outputs (falciparum only)
  expect_equal(p$severe_incidence_rendering_min_ages, expected_min)
  expect_equal(p$severe_incidence_rendering_max_ages, expected_max)

  # Check prevalence outputs (2-10 years)
  prevalence_ages <- c(2, 10) * 365
  expect_equal(
    p$prevalence_rendering_min_ages,
    prevalence_ages[-length(prevalence_ages)]
  )
  expect_equal(p$prevalence_rendering_max_ages, prevalence_ages[-1] - 1)
})

test_that("custom age outputs can be specified", {
  custom_ages <- c(0, 10, 20) * 365
  custom_prev <- c(5, 15) * 365

  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    age_group = custom_ages,
    clinical_incidence = custom_ages,
    severe_incidence = custom_ages,
    prevalence = custom_prev,
    start_year = start_year,
    end_year = end_year
  )

  expected_min <- custom_ages[-length(custom_ages)]
  expected_max <- custom_ages[-1] - 1

  expect_equal(p$age_group_rendering_min_ages, expected_min)
  expect_equal(p$age_group_rendering_max_ages, expected_max)
  expect_equal(p$clinical_incidence_rendering_min_ages, expected_min)
  expect_equal(p$clinical_incidence_rendering_max_ages, expected_max)

  expect_equal(
    p$prevalence_rendering_min_ages,
    custom_prev[-length(custom_prev)]
  )
  expect_equal(p$prevalence_rendering_max_ages, custom_prev[-1] - 1)
})

test_that("vivax does not get severe incidence outputs", {
  p <- site_parameters(
    interventions = example_site$interventions,
    demography = example_site$demography,
    vectors = example_site$vectors,
    seasonality = example_site$seasonality,
    parasite = "vivax",
    start_year = start_year,
    end_year = end_year
  )

  # Vivax should not have severe incidence parameters
  expect_equal(p$severe_incidence_rendering_min_ages, numeric(0))
  expect_equal(p$severe_incidence_rendering_max_ages, numeric(0))

  # But should have other outputs
  expect_type(p$age_group_rendering_min_ages, "double")
  expect_type(p$clinical_incidence_rendering_min_ages, "double")
  expect_type(p$prevalence_rendering_min_ages, "double")
})

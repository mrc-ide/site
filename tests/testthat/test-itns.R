example_itns <- create_example_itn()

d <- calendar_to_timestep(
  example_itns$implementation$year,
  example_itns$implementation$distribution_day_of_year,
  start_year = 2000
)
usage_timestep <- calendar_to_timestep(
  example_itns$use$year,
  example_itns$use$usage_day_of_year,
  start_year = 2000
)

example_resistance <- create_example_resistance()

example_itns_complete <- create_example_itn()

usage_timestep <- calendar_to_timestep(
  year = example_itns_complete$use$year,
  day_of_year = example_itns_complete$use$usage_day_of_year,
  start_year = 2000
)
implementation_timestep <- calendar_to_timestep(
  year = example_itns_complete$implementation$year,
  day_of_year = example_itns_complete$implementation$distribution_day_of_year,
  start_year = 2000
)
example_itns_complete$implementation$itn_input_dist <-
  netz::usage_to_model_distribution(
    usage = example_itns_complete$use$itn_use,
    usage_timesteps = usage_timestep,
    distribution_timesteps = implementation_timestep,
    distribution_lower = example_itns_complete$implementation$distribution_lower,
    distribution_upper = example_itns_complete$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = example_itns_complete$retention_half_life
  )

p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("Adding itns works", {
  p1 <- add_itns(
    p = p0,
    itn = example_itns_complete,
    resistance = example_resistance
  )

  expect_equal(p1$bednets, TRUE)
  expect_equal(
    p1$bednet_timesteps,
    calendar_to_timestep(
      year = example_itns_complete$implementation$year,
      day_of_year = example_itns_complete$implementation$distribution_day_of_year,
      start_year = p0$start_year
    )
  )
  expect_equal(
    p1$bednet_coverages,
    example_itns_complete$implementation$itn_input_dist
  )
  expect_equal(
    p1$bednet_logistic_half_life,
    example_itns_complete$retention_half_life
  )
  expect_equal(p1$bednet_logistic_k, 20)
})

test_that("Missing itn_input_dist gives informative error", {
  expect_error(
    p1 <- add_itns(
      p = p0,
      itn = example_itns,
      resistance = example_resistance
    ),
    "Missing required column.*itn_input_dist"
  )
})

test_that("Net type is checked", {
  wrong_net_example_itn <- example_itns_complete
  wrong_net_example_itn$implementation$net_type[1] <- "unsupported_net"
  expect_error(
    p1 <- add_itns(
      p = p0,
      itn = wrong_net_example_itn,
      resistance = example_resistance
    ),
    "Unsupported net type.*found.*unsupported_net"
  )
})

test_that("Introduced NAs get flagged", {
  na_example_resistance <- example_resistance
  na_example_resistance$pyrethroid_resistance[1] <- 100
  expect_error(
    p1 <- add_itns(
      p = p0,
      itn = example_itns_complete,
      resistance = na_example_resistance
    ),
    "NA values found.*bioassay"
  )
})

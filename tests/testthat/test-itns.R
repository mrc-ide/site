example_itns <- list(
  retention_half_life = 600,
  use = data.frame(
    name = "place",
    year = 2000:2002,
    itn_use = c(0.1, 0.2, 0.5),
    usage_day_of_year = 1
  ),
  implementation = data.frame(
    name = "place",
    year = rep(2000:2002, each = 5),
    net_type = rep(
      c("pyrethroid_only", "pyrethroid_pbo", "pyrethroid_pyrrole"),
      each = 5
    ),
    distribution_type = rep(c("mass", rep("routine", 4)), 3),
    distribution_lower = 0,
    distribution_upper = rep(c(1, rep(0.01, 4)), 3),
    distribution_day_of_year = rep(c(1, 2, 90, 180, 270), 3)
  )
)

implementation_time_step <- calendar_to_timestep(
  example_itns$implementation$year,
  example_itns$implementation$distribution_day_of_year,
  start_year = 2000
)
usage_timestep <- calendar_to_timestep(
  example_itns$use$year,
  example_itns$use$usage_day_of_year,
  start_year = 2000
)

index <- implementation_time_step <= max(usage_timestep)
example_itns$implementation <- example_itns$implementation[index, ]
implementation_time_step <- implementation_time_step[index]

example_resistance <- data.frame(
  name = "place",
  year = 2000:2002,
  pyrethroid_resistance = c(0.2345, 0.3283, 0.224)
)

example_itns_complete <- example_itns
example_itns_complete$implementation$itn_input_dist <-
  netz::usage_to_model_distribution(
    usage = example_itns$use$itn_use,
    usage_timesteps = usage_timestep,
    distribution_timesteps = implementation_time_step,
    distribution_lower = example_itns$implementation$distribution_lower,
    distribution_upper = example_itns$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = example_itns$retention_half_life
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

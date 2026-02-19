example_itns <- create_example_itn()
example_resistance <- create_example_resistance()

example_itns_complete <- create_example_itn()

example_itns_complete$implementation$itn_input_dist <-
  site_usage_to_model_distribution(
    usage = example_itns_complete$use$itn_use,
    usage_year = example_itns_complete$use$year,
    usage_day_of_year = example_itns_complete$use$usage_day_of_year,
    distribution_year = example_itns_complete$implementation$year,
    distribution_day_of_year = example_itns_complete$implementation$distribution_day_of_year,
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

test_that("Missing join keys between itn and resistance is flagged", {
  resistance_no_join <- example_resistance
  names(resistance_no_join) <- c("site", "time", "pyrethroid_resistance")
  expect_error(
    p1 <- add_itns(
      p = p0,
      itn = example_itns_complete,
      resistance = resistance_no_join
    ),
    "No shared columns"
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

test_that("site_usage_to_model_distribution matches netz directly", {
  itn <- create_example_itn()
  ref_start <- min(c(itn$use$year, itn$implementation$year))

  usage_ts <- calendar_to_timestep(
    itn$use$year, itn$use$usage_day_of_year, start_year = ref_start
  )
  dist_ts <- calendar_to_timestep(
    itn$implementation$year,
    itn$implementation$distribution_day_of_year,
    start_year = ref_start
  )

  expected <- netz::usage_to_model_distribution(
    usage = itn$use$itn_use,
    usage_timesteps = usage_ts,
    distribution_timesteps = dist_ts,
    distribution_lower = itn$implementation$distribution_lower,
    distribution_upper = itn$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = itn$retention_half_life
  )

  result <- site_usage_to_model_distribution(
    usage = itn$use$itn_use,
    usage_year = itn$use$year,
    usage_day_of_year = itn$use$usage_day_of_year,
    distribution_year = itn$implementation$year,
    distribution_day_of_year = itn$implementation$distribution_day_of_year,
    distribution_lower = itn$implementation$distribution_lower,
    distribution_upper = itn$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = itn$retention_half_life
  )

  expect_equal(result, expected)
})

test_that("site_usage_to_model_distribution defaults work", {

  itn <- create_example_itn()

  # Should work with defaults for distribution_lower and distribution_upper
  result <- site_usage_to_model_distribution(
    usage = itn$use$itn_use,
    usage_year = itn$use$year,
    usage_day_of_year = itn$use$usage_day_of_year,
    distribution_year = itn$implementation$year,
    distribution_day_of_year = itn$implementation$distribution_day_of_year,
    net_loss_function = netz::net_loss_map,
    half_life = itn$retention_half_life
  )

  expect_length(result, nrow(itn$implementation))
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
})

test_that("site_model_distribution_to_usage matches netz directly", {
  itn <- create_example_itn()
  ref_start <- min(c(itn$use$year, itn$implementation$year))

  usage_ts <- calendar_to_timestep(
    itn$use$year, itn$use$usage_day_of_year, start_year = ref_start
  )
  dist_ts <- calendar_to_timestep(
    itn$implementation$year,
    itn$implementation$distribution_day_of_year,
    start_year = ref_start
  )

  input_dist <- netz::usage_to_model_distribution(
    usage = itn$use$itn_use,
    usage_timesteps = usage_ts,
    distribution_timesteps = dist_ts,
    distribution_lower = itn$implementation$distribution_lower,
    distribution_upper = itn$implementation$distribution_upper,
    net_loss_function = netz::net_loss_map,
    half_life = itn$retention_half_life
  )

  expected <- netz::model_distribution_to_usage(
    usage_timesteps = usage_ts,
    distribution = input_dist,
    distribution_timesteps = dist_ts,
    net_loss_function = netz::net_loss_map,
    half_life = itn$retention_half_life
  )

  result <- site_model_distribution_to_usage(
    distribution = input_dist,
    usage_year = itn$use$year,
    usage_day_of_year = itn$use$usage_day_of_year,
    distribution_year = itn$implementation$year,
    distribution_day_of_year = itn$implementation$distribution_day_of_year,
    net_loss_function = netz::net_loss_map,
    half_life = itn$retention_half_life
  )

  expect_equal(result, expected)
})

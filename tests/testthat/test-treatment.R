p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("adding drugs works for falciparum", {
  # Drug types
  p1 <- add_drugs(p0)
  expect_equal(
    p1$drug_efficacy,
    c(0.90, 0.95, 0.90, 1.00, 1.00)
  )
  expect_equal(
    p1$drug_rel_c,
    c(0.32000, 0.05094, 0.32000, 0.32000, 0.05094)
  )
  expect_equal(
    p1$drug_prophylaxis_shape,
    c(2.516, 11.300, 4.300, 2.516, 11.300)
  )
  expect_equal(
    p1$drug_prophylaxis_scale,
    c(46.68, 10.6, 38.1, 46.68, 10.6)
  )
})

test_that("adding drugs works for vivax", {
  # Drug types
  p0$parasite <- "vivax"
  p1 <- add_drugs(p0)
  expect_equal(
    p1$drug_efficacy,
    c(0.90, 0.95, 0.90, 1.00, 1.00)
  )
  expect_equal(
    p1$drug_rel_c,
    c(0.32000, 0.05094, 0.32000, 0.32000, 0.05094)
  )
  expect_equal(
    p1$drug_prophylaxis_shape,
    c(2.516, 11.300, 4.300, 2.516, 11.300)
  )
  expect_equal(
    p1$drug_prophylaxis_scale,
    c(46.68, 10.6, 38.1, 46.68, 10.6)
  )
  expect_equal(
    p1$drug_hypnozoite_efficacy,
    rep(0, 5)
  )
  expect_equal(
    p1$drug_hypnozoite_prophylaxis_shape,
    rep(0, 5)
  )
  expect_equal(
    p1$drug_hypnozoite_prophylaxis_scale,
    rep(0, 5)
  )
})

example_treatment <- list(
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    tx_cov = 0.1,
    prop_act = 0.2
  )
)

test_that("Setting treatment works", {
  p1 <- add_drugs(p0) |>
    add_treatment(treatment = example_treatment)

  expect_equal(
    p1$clinical_treatment_coverages[[1]],
    example_treatment$implementation$tx_cov *
      (1 - example_treatment$implementation$prop_act)
  )
  expect_equal(
    p1$clinical_treatment_coverages[[2]],
    example_treatment$implementation$tx_cov *
      example_treatment$implementation$prop_act
  )
  expect_equal(
    p1$clinical_treatment_timesteps[[1]],
    calendar_to_timestep(
      year = example_treatment$implementation$year,
      start_year = p1$start_year
    )
  )
  expect_equal(
    p1$clinical_treatment_timesteps[[2]],
    calendar_to_timestep(
      year = example_treatment$implementation$year,
      start_year = p1$start_year
    )
  )
})

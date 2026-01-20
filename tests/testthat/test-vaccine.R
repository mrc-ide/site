## rtss -----------------------------------------------------------------------------------

example_rtss_age_based_no_boost <- list(
  delivery = "age-based",
  primary_schedule = c(180, 210, 240),
  booster_spacing = 365,
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0.8,
    rtss_booster1_cov = 0,
    r21_primary_cov = 0,
    r21_booster1_cov = 0,
    hybrid_booster_day_of_year = NA
  )
)

p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("adding rtss, age based no booster works", {
  p1 <- add_vaccine(
    p = p0,
    vaccine = example_rtss_age_based_no_boost
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_rtss_age_based_no_boost$implementation$rtss_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_rtss_age_based_no_boost$primary_schedule[1]
  )
  expect_equal(
    p1$pev_epi_booster_coverage,
    matrix(
      example_rtss_age_based_no_boost$implementation$rtss_booster1_cov /
        example_rtss_age_based_no_boost$implementation$rtss_primary_cov,
      ncol = 1
    )
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    example_rtss_age_based_no_boost$booster_spacing
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_rtss_age_based_no_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_rtss_age_based_no_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::rtss_profile,
      malariasimulation::rtss_booster_profile
    )
  )
})

example_rtss_age_based_single_boost <- list(
  delivery = "age-based",
  primary_schedule = c(180, 210, 240),
  booster_spacing = 365,
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0.8,
    rtss_booster1_cov = 0.4,
    r21_primary_cov = 0,
    r21_booster1_cov = 0,
    hybrid_booster_day_of_year = NA
  )
)

test_that("adding rtss, age based single booster works", {
  p1 <- add_vaccine(
    p = p0,
    vaccine = example_rtss_age_based_single_boost
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_rtss_age_based_single_boost$implementation$rtss_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_rtss_age_based_single_boost$primary_schedule[1]
  )
  expect_equal(
    p1$pev_epi_booster_coverage,
    matrix(
      example_rtss_age_based_single_boost$implementation$rtss_booster1_cov /
        example_rtss_age_based_single_boost$implementation$rtss_primary_cov,
      ncol = 1
    )
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    example_rtss_age_based_single_boost$booster_spacing
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_rtss_age_based_single_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_rtss_age_based_single_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::rtss_profile,
      malariasimulation::rtss_booster_profile
    )
  )
})

example_rtss_age_based_multiple_boost <- list(
  delivery = "age-based",
  primary_schedule = c(180, 210, 240),
  booster_spacing = c(365, 365 * 2),
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0.8,
    rtss_booster1_cov = 0.4,
    rtss_booster2_cov = 0.3,
    r21_primary_cov = 0,
    r21_booster1_cov = 0,
    hybrid_booster_day_of_year = NA
  )
)

test_that("adding rtss, age based multiple booster works", {
  p1 <- add_vaccine(
    p = p0,
    vaccine = example_rtss_age_based_multiple_boost
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_rtss_age_based_multiple_boost$implementation$rtss_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_rtss_age_based_multiple_boost$primary_schedule[1]
  )

  expected_booster_cov <- matrix(
    c(
      example_rtss_age_based_multiple_boost$implementation$rtss_booster1_cov /
        example_rtss_age_based_multiple_boost$implementation$rtss_primary_cov,
      example_rtss_age_based_multiple_boost$implementation$rtss_booster2_cov /
        example_rtss_age_based_multiple_boost$implementation$rtss_primary_cov
    ),
    ncol = 2
  )
  colnames(expected_booster_cov) <- c(
    "vaccine_booster1_cov",
    "vaccine_booster2_cov"
  )

  expect_equal(
    p1$pev_epi_booster_coverage,
    expected_booster_cov
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    example_rtss_age_based_multiple_boost$booster_spacing
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_rtss_age_based_multiple_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_rtss_age_based_multiple_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::rtss_profile,
      malariasimulation::rtss_booster_profile,
      malariasimulation::rtss_booster_profile
    )
  )
})

example_rtss_hybrid_single_boost <- list(
  delivery = "hybrid",
  primary_schedule = c(180, 210, 240),
  booster_spacing = NA,
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0.8,
    rtss_booster1_cov = 0.4,
    r21_primary_cov = 0,
    r21_booster1_cov = 0
  )
)

test_that("adding rtss, hybird single booster works", {
  p1 <- add_vaccine(
    p = p0,
    vaccine = example_rtss_hybrid_single_boost
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_rtss_hybrid_single_boost$implementation$rtss_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_rtss_hybrid_single_boost$primary_schedule[1]
  )

  expect_equal(
    p1$pev_epi_booster_coverage,
    matrix(
      example_rtss_hybrid_single_boost$implementation$rtss_booster1_cov /
        example_rtss_hybrid_single_boost$implementation$rtss_primary_cov,
      ncol = 1
    )
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    (example_rtss_hybrid_single_boost$implementation$peak_season[1] - 90) %% 365
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_rtss_hybrid_single_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_rtss_hybrid_single_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 182)
  expect_equal(p1$pev_epi_seasonal_boosters, TRUE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::rtss_profile,
      malariasimulation::rtss_booster_profile
    )
  )
})

example_rtss_hybrid_multiple_boost <- list(
  delivery = "hybrid",
  primary_schedule = c(180, 210, 240),
  booster_spacing = c(NA, 365),
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0.8,
    rtss_booster1_cov = 0.4,
    rtss_booster2_cov = 0.3,
    r21_primary_cov = 0,
    r21_booster1_cov = 0
  )
)

test_that("adding rtss, hybird multiple booster works", {
  p1 <- add_vaccine(
    p = p0,
    vaccine = example_rtss_hybrid_multiple_boost
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_rtss_hybrid_multiple_boost$implementation$rtss_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_rtss_hybrid_multiple_boost$primary_schedule[1]
  )
  expected_booster_cov <- matrix(
    c(
      example_rtss_age_based_multiple_boost$implementation$rtss_booster1_cov /
        example_rtss_age_based_multiple_boost$implementation$rtss_primary_cov,
      example_rtss_age_based_multiple_boost$implementation$rtss_booster2_cov /
        example_rtss_age_based_multiple_boost$implementation$rtss_primary_cov
    ),
    ncol = 2
  )
  colnames(expected_booster_cov) <- c(
    "vaccine_booster1_cov",
    "vaccine_booster2_cov"
  )
  expect_equal(
    p1$pev_epi_booster_coverage,
    expected_booster_cov
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    c(
      (example_rtss_hybrid_multiple_boost$implementation$peak_season[1] - 90) %%
        365,
      365
    )
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_rtss_hybrid_multiple_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_rtss_hybrid_multiple_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 182)
  expect_equal(p1$pev_epi_seasonal_boosters, TRUE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::rtss_profile,
      malariasimulation::rtss_booster_profile,
      malariasimulation::rtss_booster_profile
    )
  )
})


## R21 -----------------------------------------------------------------------------------

example_r21_age_based_no_boost <- list(
  delivery = "age-based",
  primary_schedule = c(180, 210, 240),
  booster_spacing = 365,
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0,
    rtss_booster1_cov = 0,
    r21_primary_cov = 0.8,
    r21_booster1_cov = 0,
    hybrid_booster_day_of_year = NA
  )
)

p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("adding r21, age based no booster works", {
  p1 <- add_vaccine(
    p = p0,
    vaccine = example_r21_age_based_no_boost
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_r21_age_based_no_boost$implementation$r21_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_r21_age_based_no_boost$primary_schedule[1]
  )
  expect_equal(
    p1$pev_epi_booster_coverage,
    matrix(
      example_r21_age_based_no_boost$implementation$r21_booster1_cov /
        example_r21_age_based_no_boost$implementation$r21_primary_cov,
      ncol = 1
    )
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    example_r21_age_based_no_boost$booster_spacing
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_r21_age_based_no_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_r21_age_based_no_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::r21_profile,
      malariasimulation::r21_booster_profile
    )
  )
})

example_r21_age_based_single_boost <- list(
  delivery = "age-based",
  primary_schedule = c(180, 210, 240),
  booster_spacing = 365,
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0,
    rtss_booster1_cov = 0,
    r21_primary_cov = 0.8,
    r21_booster1_cov = 0.4,
    hybrid_booster_day_of_year = NA
  )
)

test_that("adding r21, age based single booster works", {
  p1 <- add_vaccine(
    p = p0,
    vaccine = example_r21_age_based_single_boost
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_r21_age_based_single_boost$implementation$r21_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_r21_age_based_single_boost$primary_schedule[1]
  )
  expect_equal(
    p1$pev_epi_booster_coverage,
    matrix(
      example_r21_age_based_single_boost$implementation$r21_booster1_cov /
        example_r21_age_based_single_boost$implementation$r21_primary_cov,
      ncol = 1
    )
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    example_r21_age_based_single_boost$booster_spacing
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_r21_age_based_single_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_r21_age_based_single_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::r21_profile,
      malariasimulation::r21_booster_profile
    )
  )
})

example_r21_age_based_multiple_boost <- list(
  delivery = "age-based",
  primary_schedule = c(180, 210, 240),
  booster_spacing = c(365, 365 * 2),
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0,
    rtss_booster1_cov = 0,
    r21_primary_cov = 0.8,
    r21_booster1_cov = 0.4,
    r21_booster2_cov = 0.3,
    hybrid_booster_day_of_year = NA
  )
)

## mixed -----------------------------------------------------------------------------------

example_rtss_r21_age_based_single_boost <- list(
  delivery = "age-based",
  primary_schedule = c(180, 210, 240),
  booster_spacing = 365,
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    peak_season = 100,
    rtss_primary_cov = 0.8,
    rtss_booster1_cov = 0.4,
    r21_primary_cov = 0.3,
    r21_booster1_cov = 0.2,
    hybrid_booster_day_of_year = NA
  )
)

test_that("adding rtss and r21 togther warns and reverts to r21", {
  expect_warning(
    p1 <- add_vaccine(
      p = p0,
      vaccine = example_rtss_r21_age_based_single_boost
    ),
    "Cannot currently"
  )

  expect_equal(
    p1$pev_epi_coverages,
    example_rtss_r21_age_based_single_boost$implementation$r21_primary_cov
  )
  expect_equal(
    p1$pev_epi_age,
    example_rtss_r21_age_based_single_boost$primary_schedule[1]
  )
  expect_equal(
    p1$pev_epi_booster_coverage,
    matrix(
      example_rtss_r21_age_based_single_boost$implementation$r21_booster1_cov /
        example_rtss_r21_age_based_single_boost$implementation$r21_primary_cov,
      ncol = 1
    )
  )
  expect_equal(
    p1$pev_epi_booster_spacing,
    example_rtss_r21_age_based_single_boost$booster_spacing
  )
  expect_equal(
    p1$pev_epi_timesteps,
    calendar_to_timestep(
      year = example_rtss_r21_age_based_single_boost$implementation$year,
      day_of_year = rep(
        1,
        nrow(example_rtss_r21_age_based_single_boost$implementation)
      ),
      start_year = p0$start_year
    )
  )
  expect_equal(p1$pev_epi_min_wait, 0)
  expect_equal(p1$pev_epi_seasonal_boosters, FALSE)
  expect_equal(
    p1$pev_profiles,
    list(
      malariasimulation::r21_profile,
      malariasimulation::r21_booster_profile
    )
  )
})

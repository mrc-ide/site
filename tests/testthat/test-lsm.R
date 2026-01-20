example_lsm <- list(
  implementation = data.frame(
    name = "place",
    year = 2000:2002,
    lsm_cov = 0.2
  )
)
p0 <- malariasimulation::get_parameters()
p0$start_year <- 2000

test_that("Carrying capacity works", {
  p1 <- p0 |>
    adjust_carrying_capacity(
      lsm = example_lsm
    )

  expect_true(p1$carrying_capacity)
  expect_equal(
    p1$carrying_capacity_scalers,
    matrix(1 - example_lsm$implementation$lsm_cov, ncol = 1)
  )
  expect_equal(
    p1$carrying_capacity_timesteps,
    calendar_to_timestep(
      year = example_lsm$implementation$year,
      start_year = p1$start_year
    )
  )
})

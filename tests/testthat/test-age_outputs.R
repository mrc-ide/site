test_that("age outputs work", {
  min_ages <- c(0, 10) * 365
  max_ages <- (c(min_ages[-1], 100 * 365)) - 1

  p0 <- malariasimulation::get_parameters()
  p1 <- set_age_outputs(p = p0,
                        min_ages = min_ages)

  expect_equal(p1$clinical_incidence_rendering_min_ages, min_ages)
  expect_equal(p1$clinical_incidence_rendering_max_ages, max_ages)

  expect_equal(p1$clinical_incidence_rendering_min_ages, min_ages)
  expect_equal(p1$clinical_incidence_rendering_max_ages, max_ages)

  expect_equal(p1$age_group_rendering_min_ages, min_ages)
  expect_equal(p1$age_group_rendering_max_ages, max_ages)

  expect_equal(p1$prevalence_rendering_min_ages, c(1, 2) * 365)
  expect_equal(p1$prevalence_rendering_max_ages, c(100, 10) * 365 - 1)
})

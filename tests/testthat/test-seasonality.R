example_seasonality_parameters <- create_example_seasonality_parameters()
p0 <- malariasimulation::get_parameters()

test_that("adding seasonality works", {
  p1 <- add_seasonality(p = p0, seasonality = example_seasonality_parameters)

  expect_equal(p1$model_seasonality, TRUE)
  expect_equal(p1$g0, example_seasonality_parameters$g0)
  expect_equal(
    p1$g,
    c(
      example_seasonality_parameters$g1,
      example_seasonality_parameters$g2,
      example_seasonality_parameters$g3
    )
  )
  expect_equal(
    p1$h,
    c(
      example_seasonality_parameters$h1,
      example_seasonality_parameters$h2,
      example_seasonality_parameters$h3
    )
  )
})

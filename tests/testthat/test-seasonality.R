test_that("adding seasonality works", {
  example_site <- single_site(example_site, 1)
  seasonality <- example_site$seasonality
  p0 <- malariasimulation::get_parameters()
  p1 <- add_seasonality(p = p0, seasonality = seasonality)

  expect_equal(p1$model_seasonality, TRUE)
  expect_equal(p1$g0, seasonality$g0)
  expect_equal(p1$g, c(seasonality$g1, seasonality$g2, seasonality$g3))
  expect_equal(p1$h, c(seasonality$h1, seasonality$h2, seasonality$h3))
})

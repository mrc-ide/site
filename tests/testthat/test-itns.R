test_that("adding itns works", {
  example_site <- single_site(example_site, 1)
  interventions <- example_site$interventions
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p1 <- add_itns(
    p = p0,
    interventions = interventions)

  expect_equal(p1$bednets, TRUE)
  expect_equal(p1$bednet_timesteps, 1 + (interventions$year - p0$baseline_year) * 365)
  expect_equal(p1$bednet_coverages, interventions$itn_input_dist)
  expect_equal(p1$bednet_retention, 365 * 5)
  expect_equal(p1$bednet_dn0,
               matrix(rep(interventions$dn0, length(p1$species)), ncol = length(p1$species)))
  expect_equal(p1$bednet_rn, matrix(rep(interventions$rn0, length(p1$species)), ncol = length(p1$species)))
  expect_equal(p1$bednet_rnm, matrix(rep(interventions$rnm, length(p1$species)), ncol = length(p1$species)))
  expect_equal(p1$bednet_gamman, interventions$gamman * 365)
})

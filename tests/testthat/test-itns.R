test_that("adding itns works", {
  single_site <- subset_site(example_site, example_site$eir[1,])
  interventions <- single_site$interventions
  p0 <- malariasimulation::get_parameters()
  p0$baseline_year <- 2000
  p1 <- add_itns(
    p = p0,
    interventions = interventions
  )

  expect_equal(p1$bednets, TRUE)
  expect_equal(p1$bednet_timesteps, interventions$itn_distribution_day + (interventions$year - p0$baseline_year) * 365)
  expect_equal(p1$bednet_coverages, interventions$itn_input_dist)
  expect_equal(p1$bednet_retention, unique(interventions$mean_retention))
  expect_equal(p1$bednet_dn0,
               matrix(rep(interventions$dn0, length(p1$species)), ncol = length(p1$species)))
  expect_equal(p1$bednet_rn, matrix(rep(interventions$rn0, length(p1$species)), ncol = length(p1$species)))
  expect_equal(p1$bednet_rnm, matrix(rep(interventions$rnm, length(p1$species)), ncol = length(p1$species)))
  expect_equal(p1$bednet_gamman, interventions$gamman * 365)


  interventions$mean_retention[1:2] <- 1:2
  expect_error(
    add_itns(
      p = p0,
      interventions = interventions
    ),
    "Time-varying net rentetion is not currently supported"
  )
})

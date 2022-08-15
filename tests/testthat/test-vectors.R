test_that("adding vectors works", {
  example_site <- single_site(example_site, 1)
  vectors <- example_site$vectors
  p0 <- malariasimulation::get_parameters()
  p1 <- add_vectors(p = p0, vectors = vectors)

  expect_equal(p1$species, vectors$species)
  expect_equal(p1$species_proportions, vectors$prop)
  expect_equal(p1$blood_meal_rates, vectors$blood_meal_rates)
  expect_equal(p1$foraging_time, vectors$foraging_time)
  expect_equal(p1$Q0, vectors$Q0)
  expect_equal(p1$phi_bednets, vectors$phi_bednets)
  expect_equal(p1$phi_indoors, vectors$phi_indoors)
  expect_equal(p1$mum, vectors$mum)
})

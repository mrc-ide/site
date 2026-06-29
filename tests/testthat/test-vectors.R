example_vector_species <- create_example_vector_species()
p0 <- malariasimulation::get_parameters()

test_that("adding vectors works", {
  p1 <- add_vectors(p = p0, vectors = example_vector_species)

  expect_equal(p1$species, example_vector_species$species)
  expect_equal(p1$species_proportions, example_vector_species$prop)
  expect_equal(p1$blood_meal_rates, example_vector_species$blood_meal_rates)
  expect_equal(p1$foraging_time, example_vector_species$foraging_time)
  expect_equal(p1$Q0, example_vector_species$Q0)
  expect_equal(p1$phi_bednets, example_vector_species$phi_bednets)
  expect_equal(p1$phi_indoors, example_vector_species$phi_indoors)
  expect_equal(p1$mum, example_vector_species$mum)
})

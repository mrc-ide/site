# ---- Theme and palettes ------------------------------------------------------

test_that("theme_site returns a ggplot2 theme", {
  th <- theme_site()
  expect_s3_class(th, "theme")
  expect_equal(th$aspect.ratio, 1 / 3)
})

test_that("theme_site respects custom aspect ratio", {
  th <- theme_site(aspect.ratio = 1)
  expect_equal(th$aspect.ratio, 1)
})

test_that("site_age_palette returns correct number of colours", {
  cols <- site_age_palette(5)
  expect_length(cols, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("site_vector_palette returns correct number of colours", {

  cols <- site_vector_palette(3)
  expect_length(cols, 3)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("site_vector_palette returns NAs when n exceeds available colours", {
  cols <- site_vector_palette(10)
  expect_length(cols, 10)
  expect_true(any(is.na(cols)))
})

# ---- Default intervention config --------------------------------------------

test_that("default_intervention_colours returns named character vector", {
  cols <- default_intervention_colours()
  expect_type(cols, "character")
  expect_true(all(nzchar(names(cols))))
  expect_true("itn_use" %in% names(cols))
  expect_true("smc" %in% names(cols))
})

test_that("default_intervention_labels returns named character vector", {
  labs <- default_intervention_labels()
  expect_type(labs, "character")
  expect_true(all(nzchar(names(labs))))
  expect_identical(names(labs), names(default_intervention_colours()))
})

# ---- Internal helpers --------------------------------------------------------

test_that("year_shading_data creates continuous shading rectangles", {
  shading <- year_shading_data(2000:2004)
  expect_s3_class(shading, "data.frame")
  expect_named(shading, c("xmin", "xmax", "ymin", "ymax"))
  expect_equal(shading$xmin, c(2000, 2002, 2004))
  expect_equal(shading$xmax, c(2001, 2003, 2005))
})

test_that("year_shading_data creates discrete shading rectangles", {
  shading <- year_shading_data(2000:2003, offset = "discrete")
  expect_equal(shading$xmin, c(1999.5, 2001.5))
  expect_equal(shading$xmax, c(2000.5, 2002.5))
})

test_that("year_shading_data respects ymin and ymax", {
  shading <- year_shading_data(2000:2001, ymin = 0, ymax = 100)
  expect_equal(shading$ymin, 0)
  expect_equal(shading$ymax, 100)
})

test_that("year_shading_layer returns a ggplot2 layer", {
  shading <- year_shading_data(2000:2002)
  layer <- year_shading_layer(shading)
  expect_s3_class(layer, "LayerInstance")
})

test_that("build_legend_data filters to present elements", {
  cols <- c(a = "red", b = "blue", c = "green")
  labs <- c(a = "A label", b = "B label", c = "C label")
  result <- build_legend_data(c("a", "c"), cols, labs)
  expect_equal(nrow(result), 2)
  expect_equal(result$element, c("a", "c"))
  expect_equal(result$colour, c("red", "green"))
  expect_equal(result$label, c("A label", "C label"))
})

test_that("build_legend_data handles missing elements gracefully", {
  cols <- c(a = "red")
  labs <- c(a = "A label")
  result <- build_legend_data(c("a", "missing"), cols, labs)
  expect_equal(nrow(result), 1)
  expect_equal(result$element, "a")
})

# ---- Plot output class checks -----------------------------------------------

test_that("plot_site_prevalence returns a ggplot", {
  prevalence <- data.frame(
    year = 2000:2004,
    pfpr = c(0.1, 0.12, 0.15, 0.13, 0.11),
    pvpr = c(0.01, 0.02, 0.01, 0.015, 0.01)
  )
  p <- plot_site_prevalence(prevalence, title = "Test prevalence")
  expect_s3_class(p, "ggplot")
})

test_that("plot_vector_species returns a ggplot", {
  vs <- data.frame(
    species = c("gambiae", "funestus"),
    prop = c(0.7, 0.3),
    Q0 = c(0.92, 0.94),
    phi_indoors = c(0.9, 0.87),
    phi_bednets = c(0.85, 0.78)
  )
  p <- plot_vector_species(vs, title = "Test vectors")
  expect_s3_class(p, "ggplot")
})

test_that("plot_pyrethroid_resistance returns a ggplot", {
  res <- data.frame(
    year = 2000:2004,
    pyrethroid_resistance = c(0.1, 0.2, 0.3, 0.4, 0.5)
  )
  p <- plot_pyrethroid_resistance(res, title = "Test resistance")
  expect_s3_class(p, "ggplot")
})

test_that("plot_age_distribution_stacked returns a ggplot", {
  pop <- data.frame(
    year = rep(2000:2002, each = 5),
    age_lower = rep(c(0, 5, 15, 30, 50), 3),
    pop = rep(c(100, 200, 300, 250, 150), 3),
    par = rep(c(80, 160, 240, 200, 120), 3)
  )
  p <- plot_age_distribution_stacked(pop, title = "Test age")
  expect_s3_class(p, "ggplot")
})

# ---- Segment preparation ----------------------------------------------------

test_that("prepare_segment_intervention formats data correctly", {
  data <- data.frame(
    year = c(2000, 2001),
    spray_day = c(100, 200),
    cov = c(0.5, 0.8)
  )
  result <- prepare_segment_intervention(data, "spray_day", "cov", "irs")
  expect_named(result, c("t", "value", "element"))
  expect_equal(result$element, c("irs", "irs"))
  expect_equal(result$value, c(0.5, 0.8))
  expect_equal(result$t, c(2000 + 100 / 365, 2001 + 200 / 365))
})

# ---- Rainfall / continuous preparation --------------------------------------

test_that("prepare_rainfall_bars normalises rainfall to the maximum", {
  monthly <- data.frame(
    year = rep(2000:2001, each = 2),
    t = rep(c(15, 200), 2),
    rainfall = c(50, 100, 25, 75)
  )
  result <- prepare_rainfall_bars(monthly)
  expect_named(result, c("t", "value", "element"))
  expect_equal(max(result$value), 1)
  expect_true(all(result$element == "rainfall_bars"))
})

test_that("prepare_rainfall_profile expands over years and normalises", {
  fourier <- data.frame(t = c(1, 183, 365), profile = c(10, 50, 10))
  result <- prepare_rainfall_profile(fourier, years = 2000:2002, max_rainfall = 100)
  expect_named(result, c("t", "value", "element"))
  expect_equal(nrow(result), 3 * 3)
  expect_equal(max(result$value), 0.5)
  expect_true(all(result$element == "rainfall_profile"))
})

test_that("prepare_continuous_interventions pivots and recodes vaccine columns", {
  interventions <- create_example_interventions()
  result <- prepare_continuous_interventions(interventions, years = 2000:2002)
  expect_named(result, c("t", "element", "value"))
  expect_true(all(c("tx_cov", "pmc_cov", "lsm_cov") %in% result$element))
  # rtss_primary_cov is recoded to the legend element name rtss_cov
  expect_true("rtss_cov" %in% result$element)
  expect_false("rtss_primary_cov" %in% result$element)
  expect_false(any(is.na(result$value)))
})

test_that("prepare_itn errors when itn_input_dist is missing", {
  itn <- create_example_itn()
  expect_error(
    prepare_itn(
      itn$implementation,
      itn$use,
      itn$retention_half_life,
      years = 2000:2002
    ),
    "itn_input_dist"
  )
})

# ---- Small utility helpers ---------------------------------------------------

test_that("label_fixed_width right-pads labels to a fixed width", {
  lab <- label_fixed_width(function(x) as.character(x), width = 6)
  out <- lab(c(1, 22))
  expect_true(all(nchar(out) == 6))
  expect_equal(out, c("     1", "    22"))
})

test_that("known_vector_species returns the major species", {
  species <- known_vector_species()
  expect_type(species, "character")
  expect_true(all(c("gambiae", "funestus", "arabiensis") %in% species))
})

test_that("species_colour_lookup uses fixed colours and generates the rest", {
  cols <- species_colour_lookup(c("Gambiae", "Funestus", "Arabiensis", "Dirus"))
  expect_equal(unname(cols["Gambiae"]), "#2A9D8F")
  expect_equal(unname(cols["Funestus"]), "#E76F51")
  expect_equal(unname(cols["Arabiensis"]), "#1B2A6C")
  # An unfixed-but-known species still resolves to a colour
  expect_match(cols["Dirus"], "^#[0-9A-Fa-f]{6}$")
})

# ---- Composite plot smoke tests ---------------------------------------------

# A complete single-site fixture: the skeleton example site augmented with the
# extra components the map / timeline / diagnostic plots consume (spatial shapes,
# prevalence, population, rainfall seasonality, and modelled ITN distribution).
example_full_site <- function() {
  site <- create_example_site()

  # prepare_itn() requires an itn_input_dist column on the implementation table.
  site$interventions$itn$implementation$itn_input_dist <- 0.3

  site$seasonality$monthly_rainfall <- data.frame(
    year = rep(2000:2002, each = 12),
    t = rep(seq(15, 350, length.out = 12), 3),
    rainfall = rep(c(10, 20, 40, 80, 120, 150, 140, 90, 60, 30, 15, 10), 3)
  )
  site$seasonality$fourier_prediction <- data.frame(
    t = 1:365,
    profile = 60 + 40 * sin(2 * pi * (1:365) / 365)
  )

  site$prevalence <- data.frame(
    year = 2000:2002,
    pfpr = c(0.10, 0.12, 0.15),
    pvpr = c(0.01, 0.02, 0.01)
  )

  site$population <- list(
    population_by_age = data.frame(
      year = rep(2000:2002, each = 5),
      age_lower = rep(c(0, 5, 15, 30, 50), 3),
      pop = rep(c(100, 200, 300, 250, 150), 3),
      par = rep(c(80, 160, 240, 200, 120), 3)
    )
  )

  # Two-level set of simple square polygons: admin-1 units and a country outline.
  admin1 <- sf::st_sf(
    name_1 = c("A", "B"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
      ))),
      sf::st_polygon(list(rbind(
        c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)
      )))
    )
  )
  country <- sf::st_sf(
    iso3c = "XXX",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(2, 0), c(2, 1), c(0, 1), c(0, 0)
    ))))
  )
  site$shape <- list(admin1, country)
  site$sites <- data.frame(iso3c = "XXX", name_1 = "A")

  site
}

test_that("plot_site_map returns a ggplot", {
  p <- plot_site_map(example_full_site(), title = "Map")
  expect_s3_class(p, "ggplot")
})

test_that("plot_site_interventions returns a ggplot", {
  p <- plot_site_interventions(example_full_site(), title = "Interventions")
  expect_s3_class(p, "ggplot")
})

test_that("plot_interventions honours show_legend = FALSE", {
  site <- example_full_site()
  p <- plot_interventions(
    site$interventions,
    site$seasonality,
    show_legend = FALSE
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_site_diagnostic returns a patchwork object", {
  p <- plot_site_diagnostic(example_full_site())
  expect_s3_class(p, "patchwork")
})

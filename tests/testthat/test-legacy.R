# A legacy (pre-update) interventions object: a single flat data frame mixing
# all intervention types into one table.
legacy_interventions <- function() {
  data.frame(
    year = 2000:2002,
    tx_cov = 0.1,
    itn_input_dist = 0.5,
    irs_cov = 0,
    rtss_cov = 0,
    pmc_cov = 0,
    smc_cov = 0,
    lsm_cov = 0
  )
}

test_that("is_legacy_site_structure detects flat data frames", {
  # Legacy: each element is a flat data frame
  expect_true(is_legacy_site_structure(interventions = legacy_interventions()))
  expect_true(is_legacy_site_structure(vectors = data.frame(species = "gambiae")))
  expect_true(is_legacy_site_structure(seasonality = data.frame(g0 = 1)))

  # Current: nested lists are not data frames
  site <- create_example_site()
  expect_false(is_legacy_site_structure(
    interventions = site$interventions,
    vectors = site$vectors,
    seasonality = site$seasonality
  ))

  # Nothing supplied
  expect_false(is_legacy_site_structure())
})

test_that("check_site_structure aborts on a legacy structure with a link", {
  expect_error(
    check_site_structure(interventions = legacy_interventions()),
    "older site-file structure"
  )

  err <- expect_error(check_site_structure(interventions = legacy_interventions()))
  msg <- conditionMessage(err)
  expect_true(grepl("Upcoming-changes", msg))
})

test_that("check_site_structure passes a current structure silently", {
  site <- create_example_site()
  expect_no_error(check_site_structure(
    interventions = site$interventions,
    vectors = site$vectors,
    seasonality = site$seasonality
  ))
})

test_that("site_parameters aborts when fed a legacy interventions object", {
  expect_error(
    site_parameters(
      interventions = legacy_interventions(),
      demography = NULL,
      vectors = NULL,
      seasonality = NULL,
      start_year = 2000,
      end_year = 2001
    ),
    "older site-file structure"
  )
})

test_that("subset_site aborts when fed a legacy site file", {
  legacy_site <- list(
    interventions = legacy_interventions(),
    vectors = data.frame(species = "gambiae"),
    seasonality = data.frame(g0 = 1)
  )
  expect_error(
    subset_site(legacy_site, site_filter = data.frame(name = "place")),
    "older site-file structure"
  )
})

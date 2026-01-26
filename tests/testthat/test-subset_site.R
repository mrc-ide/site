test_that("subset_site filters data frames recursively", {
  # Create two example sites with different names
  site1 <- create_example_site()
  site2 <- create_example_site()

  # Helper function to rename "place" to a new name in all data frames
  rename_site <- function(site_data, old_name, new_name) {
    if (is.data.frame(site_data)) {
      if ("name" %in% names(site_data)) {
        site_data$name[site_data$name == old_name] <- new_name
      }
      return(site_data)
    }
    if (is.list(site_data)) {
      return(lapply(site_data, rename_site, old_name, new_name))
    }
    return(site_data)
  }

  # Rename site2 from "place" to "place2"
  site2 <- rename_site(site2, "place", "place2")

  # Combine the two sites
  combine_sites <- function(s1, s2) {
    if (is.data.frame(s1) && is.data.frame(s2)) {
      return(rbind(s1, s2))
    }
    if (is.list(s1) && is.list(s2)) {
      combined <- list()
      for (name in union(names(s1), names(s2))) {
        if (name %in% names(s1) && name %in% names(s2)) {
          combined[[name]] <- combine_sites(s1[[name]], s2[[name]])
        } else if (name %in% names(s1)) {
          combined[[name]] <- s1[[name]]
        } else {
          combined[[name]] <- s2[[name]]
        }
      }
      return(combined)
    }
    return(s1)
  }

  site <- combine_sites(site1, site2)

  # Add metadata
  site$country <- "TestCountry"
  site$version <- "1.0"
  site$admin_level <- "name"

  # Filter to only "place" (site1)
  site_filter <- data.frame(name = "place")

  # Apply subset
  result <- subset_site(site, site_filter)

  # Check metadata is preserved
  expect_equal(result$country, "TestCountry")
  expect_equal(result$version, "1.0")
  expect_equal(result$admin_level, "name")
  expect_equal(result$sites, site_filter)

  # Check demography is filtered (should only have "place", not "place2")
  expect_true(all(result$demography$name == "place"))
  expect_false(any(result$demography$name == "place2"))

  # Check vectors are filtered
  expect_true(all(result$vectors$vector_species$name == "place"))
  expect_false(any(result$vectors$vector_species$name == "place2"))

  expect_true(all(result$vectors$pyrethroid_resistance$name == "place"))
  expect_false(any(result$vectors$pyrethroid_resistance$name == "place2"))

  # Check seasonality is filtered
  expect_true(all(result$seasonality$seasonality_parameters$name == "place"))
  expect_false(any(result$seasonality$seasonality_parameters$name == "place2"))

  # Check nested interventions are filtered
  expect_true(
    all(result$interventions$treatment$implementation$name == "place")
  )
  expect_false(
    any(result$interventions$treatment$implementation$name == "place2")
  )

  expect_true(all(result$interventions$itns$use$name == "place"))
  expect_false(any(result$interventions$itns$use$name == "place2"))

  expect_true(all(result$interventions$itns$implementation$name == "place"))
  expect_false(any(result$interventions$itns$implementation$name == "place2"))

  expect_true(all(result$interventions$irs$implementation$name == "place"))
  expect_false(any(result$interventions$irs$implementation$name == "place2"))

  expect_true(all(result$interventions$smc$implementation$name == "place"))
  expect_false(any(result$interventions$smc$implementation$name == "place2"))

  expect_true(all(result$interventions$pmc$implementation$name == "place"))
  expect_false(any(result$interventions$pmc$implementation$name == "place2"))

  expect_true(
    all(result$interventions$vaccine$implementation$name == "place")
  )
  expect_false(
    any(result$interventions$vaccine$implementation$name == "place2")
  )

  expect_true(all(result$interventions$lsm$implementation$name == "place"))
  expect_false(any(result$interventions$lsm$implementation$name == "place2"))
})

test_that("subset_site handles multiple site selection", {
  # Create three sites
  site1 <- create_example_site()
  site2 <- create_example_site()
  site3 <- create_example_site()

  # Helper to rename sites
  rename_site <- function(site_data, old_name, new_name) {
    if (is.data.frame(site_data)) {
      if ("name" %in% names(site_data)) {
        site_data$name[site_data$name == old_name] <- new_name
      }
      return(site_data)
    }
    if (is.list(site_data)) {
      return(lapply(site_data, rename_site, old_name, new_name))
    }
    return(site_data)
  }

  site2 <- rename_site(site2, "place", "place2")
  site3 <- rename_site(site3, "place", "place3")

  # Combine function
  combine_sites <- function(s1, s2) {
    if (is.data.frame(s1) && is.data.frame(s2)) {
      return(rbind(s1, s2))
    }
    if (is.list(s1) && is.list(s2)) {
      combined <- list()
      for (name in union(names(s1), names(s2))) {
        if (name %in% names(s1) && name %in% names(s2)) {
          combined[[name]] <- combine_sites(s1[[name]], s2[[name]])
        } else if (name %in% names(s1)) {
          combined[[name]] <- s1[[name]]
        } else {
          combined[[name]] <- s2[[name]]
        }
      }
      return(combined)
    }
    return(s1)
  }

  site <- combine_sites(combine_sites(site1, site2), site3)
  site$country <- "TestCountry"
  site$version <- "1.0"
  site$admin_level <- "name"

  # Filter to place and place3 (exclude place2)
  site_filter <- data.frame(name = c("place", "place3"))

  result <- subset_site(site, site_filter)

  # Check we have both place and place3, but not place2
  expect_true(all(result$demography$name %in% c("place", "place3")))
  expect_false(any(result$demography$name == "place2"))

  expect_true(
    all(result$vectors$vector_species$name %in% c("place", "place3"))
  )
  expect_false(any(result$vectors$vector_species$name == "place2"))
})

test_that("subset_site handles data frames without matching columns", {
  site <- create_example_site()
  site$country <- "TestCountry"
  site$version <- "1.0"
  site$admin_level <- "name"

  # Add a data frame without a "name" column
  site$other_data <- data.frame(
    id = 1:5,
    value = letters[1:5]
  )

  site_filter <- data.frame(name = "place")

  result <- subset_site(site, site_filter)

  # Data frame without matching columns should be kept as-is
  expect_equal(result$other_data, site$other_data)
})

test_that("match_by_names filters by matching columns", {
  x <- data.frame(
    name = c("place", "place2", "place3"),
    year = c(2000, 2001, 2002),
    value = c(10, 20, 30)
  )

  y <- data.frame(
    name = c("place", "place3"),
    other_col = c("a", "b")
  )

  result <- match_by_names(x, y)

  expect_equal(nrow(result), 2)
  expect_equal(result$name, c("place", "place3"))
  expect_equal(result$value, c(10, 30))
})

test_that("match_by_names returns x unchanged if no matching columns", {
  x <- data.frame(
    id = 1:3,
    value = c(10, 20, 30)
  )

  y <- data.frame(
    name = c("place", "place2")
  )

  result <- match_by_names(x, y)

  expect_equal(result, x)
})

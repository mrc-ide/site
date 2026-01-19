#' Add RTS,S
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_rtss <- function(p, vaccine) {
  if (vaccine$delivery == "age-based") {
    n_boosters <- length(vaccine$booster_ages)
    booster_spacing <- eg$interventions$vaccine$booster_spacing
    min_wait = 0
    seasonal_boosters <- FALSE
  }

  if (vaccine$delivery == "hybrid") {
    n_boosters <- 1
    booster_spacing <- vaccine$implementation$hybrid_booster_day_of_year
    min_wait = 182
    seasonal_boosters <- TRUE
  }

  # Booster coverage is input realative to primary series coverage
  booster_coverage <- as.matrix(vaccine$implementation[, paste0(
    "rtss_booster",
    1:n_boosters,
    "_cov"
  )]) |>
    sweep(1, vaccine$implementation$rtss_primary_cov, "/")

  booster_profile <- rep(
    malariasimulation::rtss_booster_profile,
    n_boosters
  )

  p <- malariasimulation::set_pev_epi(
    parameters = p,
    profile = malariasimulation::rtss_profile,
    coverages = vaccine$implementation$rtss_primary_cov,
    timesteps = vaccine$implementation$vaccine_coverage_timesteps,
    age = vaccine$primary_schedule[1],
    min_wait = min_wait,
    booster_spacing = booster_spacing,
    booster_coverage = booster_coverage,
    booster_profile = booster_profile,
    seasonal_boosters = seasonal_boosters
  )

  return(p)
}

#' Add R21
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_r21 <- function(p, interventions) {
  month <- 365 / 12
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365
  if ("n_doses" %in% colnames(interventions)) {
    booster_cov <- rep(0, length(timesteps))
    booster_cov[interventions$n_doses == 4] <- 0.8
  } else {
    booster_cov <- rep(0.8, length(timesteps))
  }

  p <- malariasimulation::set_pev_epi(
    parameters = p,
    profile = malariasimulation::r21_profile,
    coverages = interventions$r21_cov,
    timesteps = timesteps,
    # TODO: Check R21 timings/ages
    age = round(6 * month),
    min_wait = 0,
    booster_spacing = 12 * month, # The booster is administered 12 months following the third dose.
    booster_coverage = matrix(booster_cov),
    booster_profile = list(malariasimulation::r21_booster_profile)
  )

  return(p)
}

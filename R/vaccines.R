#' Add RTS,S
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_rtss <- function(p, vaccine) {
  # Format boosters
  booster_cols <- intersect(
    paste0("rtss_booster", 1:100, "_cov"),
    names(vaccine$implementation)
  )
  # Take absolute booster coverages and convert so they are relative to primary coverage
  rtss_booster_coverage <- as.matrix(vaccine$implementation[, booster_cols]) |>
    sweep(1, vaccine$implementation$rtss_primary_cov, "/")
  if (ncol(rtss_booster_coverage) != length(vaccine$schedule) + 3) {
    stop(
      "Number of RTS,S booster coverage columns and booster ages do not align"
    )
  }
  booster_spacing <- setdiff(vaccine$schedule[3:length(vaccine$schedule)])
  booster_profile <- rep(
    malariasimulation::rtss_booster_profile,
    length(booster_spacing)
  )

  p <- malariasimulation::set_pev_epi(
    parameters = p,
    profile = malariasimulation::rtss_profile,
    coverages = vaccine$implementation$rtss_primary_cov,
    timesteps = vaccine$implementation$vaccine_coverage_timesteps,
    age = vaccine$schedule[1],
    min_wait = 0,
    booster_spacing = booster_spacing,
    booster_coverage = rtss_booster_coverage,
    booster_profile = booster_profile
  )

  return(p)
}

#' Add RTS,S
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

#' Add vaccine
#'
#' @inheritParams add_interventions
#' @param vaccine vaccine intervention data including coverage, delivery mode, and booster schedules
#'
#' @return modified parameter list
add_vaccine <- function(p, vaccine) {
  rtss <- sum(vaccine$implementation$rtss_primary_cov, na.rm = TRUE) > 0
  r21 <- sum(vaccine$implementation$r21_primary_cov, na.rm = TRUE) > 0
  if (rtss && r21) {
    warning(
      "Cannot currently model two vaccine types,
            defaulting to R21 vaccine type implemented at
            the maximum yearly coverage inputs across rtss
            and r21"
    )
    rtss <- FALSE
  }

  if (rtss) {
    profile <- malariasimulation::rtss_profile
    booster_profile <- malariasimulation::rtss_booster_profile
    names(vaccine$implementation) <- stringr::str_replace_all(
      string = names(vaccine$implementation),
      pattern = "rtss_",
      replacement = "vaccine_"
    )
  }
  if (r21) {
    profile <- malariasimulation::r21_profile
    booster_profile <- malariasimulation::r21_booster_profile
    names(vaccine$implementation) <- stringr::str_replace_all(
      string = names(vaccine$implementation),
      pattern = "r21_",
      replacement = "vaccine_"
    )
  }

  if (vaccine$delivery == "age-based") {
    min_wait = 0
    seasonal_boosters <- FALSE
  }
  if (vaccine$delivery == "hybrid") {
    min_wait = 182
    seasonal_boosters <- TRUE
    vaccine$booster_spacing[1] <- (vaccine$implementation$peak_season[1] -
      90) %%
      365
  }

  n_boosters <- length(vaccine$booster_spacing)
  booster_spacing <- vaccine$booster_spacing
  # Ensure all required booster coverage columns exist (default to 0 if missing)
  for (i in seq_len(n_boosters)) {
    col_name <- paste0("vaccine_booster", i, "_cov")
    if (!col_name %in% names(vaccine$implementation)) {
      vaccine$implementation[[col_name]] <- 0
    }
  }
  # Booster coverage is input realative to primary series coverage
  booster_coverage <- as.matrix(vaccine$implementation[, paste0(
    "vaccine_booster",
    1:n_boosters,
    "_cov"
  )])
  for (j in 1:ncol(booster_coverage)) {
    booster_coverage[, j] <- booster_coverage[, j] /
      vaccine$implementation$vaccine_primary_cov
    booster_coverage[vaccine$implementation$vaccine_primary_cov == 0, j] <- 0
  }

  booster_profile_list <- lapply(1:n_boosters, function(x) {
    booster_profile
  })

  timesteps <- calendar_to_timestep(
    vaccine$implementation$year,
    day_of_year = vaccine$implementation$day_of_year,
    start_year = p$start_year
  )

  p <- malariasimulation::set_pev_epi(
    parameters = p,
    profile = profile,
    coverages = vaccine$implementation$vaccine_primary_cov,
    timesteps = timesteps,
    age = vaccine$primary_schedule[1],
    min_wait = min_wait,
    booster_spacing = booster_spacing,
    booster_coverage = booster_coverage,
    booster_profile = booster_profile_list,
    seasonal_boosters = seasonal_boosters
  )

  return(p)
}

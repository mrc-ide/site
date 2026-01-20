#' Add vaccine
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_vaccine <- function(p, vaccine) {
  rtss <- sum(vaccine$implementation$rtss_primary_cov, na.rm = T) > 0
  r21 <- sum(vaccine$implementation$r21_primary_cov, na.rm = T) > 0
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
  # Booster coverage is input realative to primary series coverage
  booster_coverage <- as.matrix(vaccine$implementation[, paste0(
    "vaccine_booster",
    1:n_boosters,
    "_cov"
  )]) |>
    sweep(1, vaccine$implementation$vaccine_primary_cov, "/")

  booster_profile_list <- lapply(1:n_boosters, function(x) {
    booster_profile
  })

  timesteps <- calendar_to_timestep(
    vaccine$implementation$year,
    day_of_year = rep(1, nrow(vaccine$implementation)),
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

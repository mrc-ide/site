#' Adjust carrying capacity
#'
#' @inheritParams add_interventions
#' @param lsm larval source management intervention data including coverage over time
#'
#' @return modified parameter list
adjust_carrying_capacity <- function(p, lsm) {
  n_species <- length(p$species)
  lsm_impact <- rep(1 - lsm$implementation$lsm_cov, each = n_species)

  carrying_capacity_scaler <- matrix(
    data = lsm_impact,
    ncol = n_species,
    byrow = TRUE
  )

  timesteps <- calendar_to_timestep(
    year = lsm$implementation$year,
    start_year = p$start_year
  )

  p <- malariasimulation::set_carrying_capacity(
    parameters = p,
    carrying_capacity = carrying_capacity_scaler,
    timesteps = timesteps
  )

  return(p)
}

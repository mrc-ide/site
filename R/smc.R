#' Add SMC
#'
#' @param p parameter list
#' @param smc interventions smc object
#'
#' @return modified parameter list
add_smc <- function(p, smc) {
  if (smc$drug != "sp_aq") {
    stop("SMC drug must be sp_aq")
  }

  timesteps <- calendar_to_timestep(
    year = smc$implementation$year,
    day_of_year = smc$implementation$round_day_of_year,
    start_year = p$start_year
  )

  p <- malariasimulation::set_smc(
    parameters = p,
    drug = 3,
    timesteps = timesteps,
    coverages = smc$implementation$smc_cov,
    min_age = smc$implementation$smc_min_age,
    max_age = smc$implementation$smc_max_age
  )

  return(p)
}

#' Add PMC
#'
#' @param p parameter list
#' @param pmc interventions pmc object
#'
#' @return modified parameter list
add_pmc <- function(p, pmc) {
  if (pmc$drug != "sp") {
    stop("PMC drug must be sp")
  }

  timesteps <- calendar_to_timestep(
    year = pmc$implementation$year,
    day_of_year = pmc$implementation$day_of_year,
    start_year = p$start_year
  )

  p <- malariasimulation::set_pmc(
    parameters = p,
    drug = 1,
    timesteps = timesteps,
    coverages = pmc$implementation$pmc_cov,
    ages = pmc$age
  )

  return(p)
}

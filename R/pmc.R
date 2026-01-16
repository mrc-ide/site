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

  # Drop negative timesteps
  pmc$implementation <- pmc$implementation[
    pmc$implementation$pmc_coverage_timesteps > 0,
  ]

  p <- malariasimulation::set_pmc(
    parameters = p,
    drug = 1,
    timesteps = pmc$implementation$pmc_coverage_timesteps,
    coverages = pmc$implementation$pmc_cov,
    ages = pmc$age
  )

  return(p)
}

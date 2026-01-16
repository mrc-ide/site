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

  # Drop negative timesteps
  smc$implementation <- smc$implementation[smc$implementation$round_day > 0, ]

  p <- malariasimulation::set_smc(
    parameters = p,
    drug = 3,
    timesteps = smc$implementation$round_day,
    coverages = smc$implementation$smc_cov,
    min_age = smc$implementation$smc_min_age,
    max_age = smc$implementation$smc_max_age
  )

  return(p)
}

#' Add Drugs
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_drugs <- function(p) {
  # Shape (alpha) 2.516 and scale (beta) 46.68 From Gina's
  # fit to Cisse et al data
  SP_params <- c(0.9, 0.32, 2.516, 46.68)

  # For treatment of clinical disease, we already include efficacy within
  # The effective coverage, so any treatment has assumed 100% efficacy
  SP_full_efficacy <- SP_params
  SP_full_efficacy[1] <- 1
  AL_full_efficacy <- malariasimulation::AL_params
  AL_full_efficacy[1] <- 1

  if (p$parasite == "falciparum") {
    p <- malariasimulation::set_drugs(
      parameters = p,
      list(
        SP_params,
        malariasimulation::AL_params,
        malariasimulation::SP_AQ_params,
        SP_full_efficacy,
        AL_full_efficacy
      )
    )
  }
  if (p$parasite == "vivax") {
    p <- malariasimulation::set_drugs(
      parameters = p,
      list(
        c(SP_params, 0, 0, 0),
        c(malariasimulation::AL_params, 0, 0, 0),
        c(malariasimulation::SP_AQ_params, 0, 0, 0),
        c(SP_full_efficacy, 0, 0, 0),
        c(AL_full_efficacy, 0, 0, 0)
      )
    )
  }
  return(p)
}

#' Add treatment
#'
#' @inheritParams add_interventions
#' @param treatment treatment intervention data including coverage and drug type proportions
#'
#' @return modified parameter list
add_treatment <- function(p, treatment) {
  # Assuming treatent changes happen on January 1st
  timesteps <- calendar_to_timestep(
    year = treatment$implementation$year,
    start_year = p$start_year
  )

  non_act_coverage <- treatment$implementation$tx_cov *
    (1 - treatment$implementation$prop_act)
  act_coverage <- treatment$implementation$tx_cov *
    treatment$implementation$prop_act

  # Non ACT (SP)
  p <- malariasimulation::set_clinical_treatment(
    parameters = p,
    drug = 4,
    timesteps = timesteps,
    coverages = non_act_coverage
  )
  # ACT (AL)
  p <- malariasimulation::set_clinical_treatment(
    parameters = p,
    drug = 5,
    timesteps = timesteps,
    coverages = act_coverage
  )

  return(p)
}

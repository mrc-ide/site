#' Add all interventions
#'
#' @param p parameter list
#' @param interventions site intervention inputs
#'
#' @return modified parameter list
add_interventions <- function(p, interventions, resistance, irs_adjust) {
  pf <- p$parasite == "falciparum"
  # Drug types
  p <- add_drugs(p)
  # Treatment
  if (sum(interventions$tx_cov) > 0) {
    p <- add_treatment(p, interventions)
  }
  # ITNs
  if (sum(interventions$itn_input_dist, na.rm = TRUE) > 0) {
    p <- add_itns(
      p = p,
      itn = interventions$itn,
      resistance = resistance
    )
  }
  # IRS
  if (sum(interventions$irs_cov, na.rm = TRUE) > 0) {
    p <- add_irs(
      p = p,
      irs = interventions$irs,
      irs_adjust = irs_adjust
    )
  }
  # SMC
  if (sum(interventions$smc$implementation$smc_cov, na.rm = TRUE) > 0 & pf) {
    p <- add_smc(
      p = p,
      smc = interventions$smc
    )
  }

  if (
    sum(interventions$rtss_cov, na.rm = TRUE) > 0 &
      sum(interventions$r21_cov, na.rm = TRUE) > 0
  ) {
    warning(
      "Cannot currently model two vaccine types,
            defaulting to R21 vaccine type implemented at
            the maximum yearly coverage inputs across rtss
            and r21"
    )
    interventions$r21_cov <- pmax(interventions$r21_cov, interventions$rtss_cov)
    interventions$rtss_cov <- 0
  }
  # RTSS
  if (sum(interventions$rtss_cov, na.rm = TRUE) > 0 & pf) {
    p <- add_rtss(
      p = p,
      interventions = interventions
    )
  }
  # R21
  if (sum(interventions$r21_cov, na.rm = TRUE) > 0 & pf) {
    p <- add_r21(
      p = p,
      interventions = interventions
    )
  }
  # PMC
  if (sum(interventions$pmc_cov, na.rm = TRUE) > 0 & pf) {
    p <- add_pmc(
      p = p,
      interventions = interventions
    )
  }
  # Interventions that modify the carrying capacity
  # The combined carrying capacity scaling must be estimated for all
  # interventions that modify it, before updating. Currently, only LSM is
  # implemented here. This could included An. stephensi in the future.
  if (sum(interventions$lsm_cov, na.rm = TRUE) > 0) {
    p <- adjust_carrying_capacity(
      p = p,
      interventions = interventions
    )
  }

  return(p)
}

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
#'
#' @return modified parameter list
add_treatment <- function(p, interventions) {
  # Assuming treatent changes happen on January 1st
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365

  # Non ACT (SP)
  p <- malariasimulation::set_clinical_treatment(
    parameters = p,
    drug = 4,
    timesteps = timesteps,
    coverages = interventions$tx_cov * (1 - interventions$prop_act)
  )
  # ACT (AL)
  p <- malariasimulation::set_clinical_treatment(
    parameters = p,
    drug = 5,
    timesteps = timesteps,
    coverages = interventions$tx_cov * interventions$prop_act
  )

  return(p)
}

#' Adjust carrying capacity
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
adjust_carrying_capacity <- function(p, interventions) {
  lsm_impact <- rep(1 - interventions$lsm_cov, each = length(p$species))
  carrying_capacity_scaler <- matrix(
    data = lsm_impact,
    ncol = length(p$species),
    byrow = TRUE
  )
  month <- 365 / 12
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365

  p <- malariasimulation::set_carrying_capacity(
    parameters = p,
    carrying_capacity = carrying_capacity_scaler,
    timesteps = timesteps
  )

  return(p)
}

#' Add all interventions
#'
#' @param p parameter list
#' @param interventions site intervention inputs
#' @param species Can be falciparum: "pf" or vivax: "pv", for vivax SMC, RTSS
#'  and PMC are not implemented
#'
#' @return modified parameter list
add_interventions <- function(p, interventions, species){

  # Drug types
  p <- add_drugs(p)
  # Treatment
  if(sum(interventions$tx_cov) > 0){
    p <- add_treatment(p, interventions)
  }
  # ITNs
  if(sum(interventions$itn_input_dist, na.rm = TRUE) > 0){
    p <- add_itns(
      p = p,
      interventions = interventions)
  }
  # IRS
  if(sum(interventions$irs_cov, na.rm = TRUE) > 0){
    p <- add_irs(
      p = p,
      interventions = interventions)
  }
  # SMC
  if(sum(interventions$smc_cov, na.rm = TRUE) > 0 &
     species ==  "pf"){
    p <- add_smc(p = p,
                 interventions = interventions)
  }
  # RTSS
  if(sum(interventions$rtss_cov, na.rm = TRUE) > 0 &
     species ==  "pf"){
    p <- add_rtss(p = p,
                  interventions = interventions)
  }
  # PMC
  if(sum(interventions$pmc_cov, na.rm = TRUE) > 0 &
     species ==  "pf"){
    p <- add_pmc(p = p,
                 interventions = interventions)
  }

  return(p)
}

#' Add Drugs
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_drugs <- function(p){

  # Shape (alpha) 2.516 and scale (beta) 46.68 From Gina's
  # fit to Cisse et al data
  SP_params <- c(0.9, 0.32, 2.516, 46.68)

  # For treatment of clinical disease, we already include efficacy within
  # The effective coverage, so any treatment has assumed 100% efficacy
  SP_full_efficacy <- SP_params
  SP_full_efficacy[1] <- 1
  AL_full_efficacy <- malariasimulation::AL_params
  AL_full_efficacy[1] <- 1

  p <- malariasimulation::set_drugs(
    parameters = p,
    list(tetst = SP_params,
         malariasimulation::AL_params,
         malariasimulation::SP_AQ_params,
         SP_full_efficacy,
         AL_full_efficacy))


  return(p)
}

#' Add treatment
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_treatment <- function(p, interventions){
  # Assuming treatent changes happen on January 1st
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365

  # Non ACT (SP)
  p <- malariasimulation::set_clinical_treatment(parameters = p,
                                                 drug = 4,
                                                 timesteps = timesteps,
                                                 coverages = interventions$tx_cov * (1 - interventions$prop_act))
  # ACT (AL)
  p <- malariasimulation::set_clinical_treatment(parameters = p,
                                                 drug = 5,
                                                 timesteps = timesteps,
                                                 coverages = interventions$tx_cov * interventions$prop_act)


  return(p)
}

#' Add ITNs
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_itns <- function(p, interventions){
  # Assuming net distribution happens on January 1st
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365
  # Net retention half life does not vary over time (Should match what is used when fitting input dist)
  retention <- 365 * 5
  # Net input coverage
  coverages <- interventions$itn_input_dist
  coverages[is.na(coverages)] <- 0
  # Net efficacy parameters
  n_species <- length(p$species)
  dn0 <- matrix(rep(interventions$dn0, n_species), ncol = n_species)
  rn <- matrix(rep(interventions$rn0, n_species), ncol = n_species)
  rnm <- matrix(rep(interventions$rnm, n_species), ncol = n_species)
  gamman <- interventions$gamman

  p <- malariasimulation::set_bednets(
    parameters = p,
    timesteps = timesteps,
    coverages = coverages,
    retention = retention,
    dn0 = dn0,
    rn = rn,
    rnm = rnm,
    gamman = gamman
  )

  return(p)
}

#' Add IRS
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_irs <- function(p, interventions){
  month <- 365 / 12
  peak <- malariasimulation::peak_season_offset(p)
  year_start_times <-  1 + (interventions$year - p$baseline_year) * 365
  peak_season_times <- peak + year_start_times
  # Assume IRS occurs 3 months before seasonal peak
  irs_spray_times <- round(peak_season_times - 3 * month)

  n_species <- length(p$species)

  p <- malariasimulation::set_spraying(
    parameters = p,
    timesteps = irs_spray_times,
    coverages = interventions$irs_cov,
    ls_theta = matrix(rep(interventions$ls_theta, n_species), ncol = n_species),
    ls_gamma = matrix(rep(interventions$ls_gamma, n_species), ncol = n_species),
    ks_theta = matrix(rep(interventions$ks_theta, n_species), ncol = n_species),
    ks_gamma = matrix(rep(interventions$ks_gamma, n_species), ncol = n_species),
    ms_theta = matrix(rep(interventions$ms_theta, n_species), ncol = n_species),
    ms_gamma = matrix(rep(interventions$ms_gamma, n_species), ncol = n_species)
  )

  return(p)
}

#' Add SMC
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_smc <- function(p, interventions){
  month <- 365 / 12

  if(!all(interventions$smc_drug == "sp_aq")){
    stop("Not currently set up for non SP AQ SMC drug")
  }

  peak <- malariasimulation::peak_season_offset(p)
  # Note: min age and max age are not currently time-varying
  rounds <- interventions$smc_n_rounds
  year_start_times <-  1 + (interventions$year - p$baseline_year) * 365
  peak_season_times <- peak + year_start_times
  # Assume middle of rounds occurs at peak season:
  round_relative_time <- as.vector(
    unlist(
      sapply(rounds, function (x) {seq(-x * month / 2, x * month / 2, length.out = x)})
    )
  )
  timesteps <- round(round_relative_time) + rep(peak_season_times, rounds)
  coverages <- rep(interventions$smc_cov, rounds)
  min_age <- rep(interventions$smc_min_age, rounds)
  max_age <- rep(interventions$smc_max_age, rounds)

  p <- malariasimulation::set_smc(
    parameters = p,
    drug = 3,
    timesteps = timesteps,
    coverages = coverages,
    min_age = min_age,
    max_age = max_age)

  return(p)
}

#' Add RTS,S
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_rtss <- function(p, interventions){
  month <- 365 / 12
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365

  p <- malariasimulation::set_rtss_epi(
    parameters = p,
    timesteps = timesteps,
    coverages = interventions$rtss_cov,
    age = round(6 * month),
    min_wait = 0,
    boosters = round(18 * month),
    booster_coverage = 0.8,
    seasonal_boosters = FALSE
  )

  return(p)
}

#' Add PMC
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_pmc <- function(p, interventions){
  month <- 365 / 12
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365

  if(!all(interventions$pmc_drug == "sp")){
    stop("Not currently set up for non SP PMC drug")
  }

  p <- malariasimulation::set_pmc(
    parameters = p,
    drug = 1,
    timesteps = timesteps,
    coverages = interventions$pmc_cov,
    ages = c(2, 3, 9) * 30
  )

  return(p)
}


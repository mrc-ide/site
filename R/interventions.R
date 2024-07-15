#' Add all interventions
#'
#' @param p parameter list
#' @param interventions site intervention inputs
#' @param species Can be falciparum: "pf" or vivax: "pv", for vivax SMC, RTSS,
#'  R21 and PMC are not implemented
#'
#' @return modified parameter list
add_interventions <- function(p, interventions, species){

  pf <- species ==  "pf"
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
      interventions = interventions
    )
  }
  # IRS
  if(sum(interventions$irs_cov, na.rm = TRUE) > 0){
    p <- add_irs(
      p = p,
      interventions = interventions
    )
  }
  # SMC
  if(sum(interventions$smc_cov, na.rm = TRUE) > 0 & pf){
    p <- add_smc(
      p = p,
      interventions = interventions
    )
  }

  if(
    sum(interventions$rtss_cov, na.rm = TRUE) > 0 &
    sum(interventions$r21_cov, na.rm = TRUE) > 0
  ) {
    warning("Cannot currently model two vaccine types,
            defaulting to R21 vaccine type implemented at
            the maximum yearly coverage inputs across rtss
            and r21")
    interventions$r21_cov <- pmax(interventions$r21_cov, interventions$rtss_cov)
    interventions$rtss_cov <- 0
  }
  # RTSS
  if(sum(interventions$rtss_cov, na.rm = TRUE) > 0 & pf){
    p <- add_rtss(
      p = p,
      interventions = interventions
    )
  }
  # R21
  if(sum(interventions$r21_cov, na.rm = TRUE) > 0 & pf){
    p <- add_r21(
      p = p,
      interventions = interventions
    )
  }
  # PMC
  if(sum(interventions$pmc_cov, na.rm = TRUE) > 0 & pf){
    p <- add_pmc(
      p = p,
      interventions = interventions
    )
  }
  # Interventions that modify the carrying capacity
  # The combined carrying capacity scaling must be estimated for all
  # interventions that modify it, before updating. Currently, only LSM is
  # implemented here. This could included An. stephensi in the future.
  if(sum(interventions$lsm_cov, na.rm = TRUE) > 0){
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
    list(
      SP_params,
      malariasimulation::AL_params,
      malariasimulation::SP_AQ_params,
      SP_full_efficacy,
      AL_full_efficacy
    )
  )
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

#' Add ITNs
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_itns <- function(p, interventions){

  # If not specified, assume distribution happens January 1st
  if(!"itn_distribution_day" %in% colnames(interventions)){
    interventions$itn_distribution_day <- 1
  }
  timesteps <- interventions$itn_distribution_day + (interventions$year - p$baseline_year) * 365
  # Net retention half life does not vary over time (Should match what is used when fitting input dist)
  retention <- unique(interventions$mean_retention)
  if(length(retention) > 1){
    stop("Time-varying net rentetion is not currently supported")
  }
  # Net input coverage
  coverages <- interventions$itn_input_dist
  coverages[is.na(coverages)] <- 0
  # Net efficacy parameters
  n_species <- length(p$species)
  dn0 <- matrix(rep(interventions$dn0, n_species), ncol = n_species)
  rn <- matrix(rep(interventions$rn0, n_species), ncol = n_species)
  rnm <- matrix(rep(interventions$rnm, n_species), ncol = n_species)
  gamman <- interventions$gamman * 365

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

  if(any(interventions$irs_spray_rounds > 2)){
    stop("Maximum of 2 IRS spray rounds per year supported")
  }

  peak_season_times <- peak + year_start_times
  peak_season_times <- rep(peak_season_times, interventions$irs_spray_rounds)

  # Assume multiple spray rounds are 6 months apart (First is 3 months prior to peak)
  offset <- sapply(interventions$irs_spray_rounds, function(x){
    c(3, -3)[1:x]
  }) |>
    unlist()
  irs_spray_times <- round(peak_season_times - offset * month)

  coverages <- rep(interventions$irs_cov, interventions$irs_spray_rounds)
  ls_theta <- rep(interventions$ls_theta, interventions$irs_spray_rounds)
  ls_gamma <- rep(interventions$ls_gamma, interventions$irs_spray_rounds)
  ks_theta <- rep(interventions$ks_theta, interventions$irs_spray_rounds)
  ks_gamma <- rep(interventions$ks_gamma, interventions$irs_spray_rounds)
  ms_theta <- rep(interventions$ms_theta, interventions$irs_spray_rounds)
  ms_gamma <- rep(interventions$ms_gamma, interventions$irs_spray_rounds)

  index <- irs_spray_times <= 0
  if(sum(index) > 0){
    irs_spray_times <- irs_spray_times[!index]
    coverages <- coverages[!index]
    ls_theta <- ls_theta[!index]
    ls_gamma <- ls_gamma[!index]
    ks_theta <- ks_theta[!index]
    ks_gamma <- ks_gamma[!index]
    ms_theta <- ms_theta[!index]
    ms_gamma <- ms_gamma[!index]
  }

  n_species <- length(p$species)

  p <- malariasimulation::set_spraying(
    parameters = p,
    timesteps = irs_spray_times,
    coverages = coverages,
    ls_theta = matrix(rep(ls_theta, n_species), ncol = n_species),
    ls_gamma = matrix(rep(ls_gamma, n_species), ncol = n_species),
    ks_theta = matrix(rep(ks_theta, n_species), ncol = n_species),
    ks_gamma = matrix(rep(ks_gamma, n_species), ncol = n_species),
    ms_theta = matrix(rep(ms_theta, n_species), ncol = n_species),
    ms_gamma = matrix(rep(ms_gamma, n_species), ncol = n_species)
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

  index <- timesteps <= 0
  if(sum(index) > 0){
    timesteps <- timesteps[!index]
    coverages <- coverages[!index]
    min_age <- min_age[!index]
    max_age <- max_age[!index]
  }

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

  p <- malariasimulation::set_pev_epi(
    parameters = p,
    profile = malariasimulation::rtss_profile,
    coverages = interventions$rtss_cov,
    timesteps = timesteps,
    age = round(6 * month),
    min_wait = 0,
    booster_spacing = 12 * month, # The booster is administered 12 months following the third dose.
    booster_coverage = matrix(rep(0.8, length(timesteps))),
    booster_profile = list(malariasimulation::rtss_booster_profile) # We will model implementation of the RTSS booster.
  )

  return(p)
}

#' Add RTS,S
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
add_r21 <- function(p, interventions){
  month <- 365 / 12
  timesteps <- 1 + (interventions$year - p$baseline_year) * 365

  r21_profile <- malariasimulation::create_pev_profile(
    vmax = 0.87,
    alpha = 0.91,
    beta = 471,
    cs = c(9.3199677, 0.8387902),
    rho = c(0.8071676, 0.6010363),
    ds = c(3.7996007, 0.1618982),
    dl =c(6.2820200, 0.4549185)
  )
  r21_booster_profile <- malariasimulation::create_pev_profile(
    vmax = 0.87,
    alpha = 0.91,
    beta = 471,
    cs = c(9.2372858, 0.7188541),
    rho = c(0.07140337, 0.54175154),
    ds = c(3.7996007, 0.1618982),
    dl =c(6.2820200, 0.4549185)
  )

  p <- malariasimulation::set_pev_epi(
    parameters = p,
    profile = r21_profile,
    coverages = interventions$r21_cov,
    timesteps = timesteps,
    # TODO: Check R21 timings/ages
    age = round(6 * month),
    min_wait = 0,
    booster_spacing = 12 * month, # The booster is administered 12 months following the third dose.
    booster_coverage = matrix(rep(0.8, length(timesteps))),
    booster_profile = list(r21_booster_profile) # We will model implementation of the RTSS booster.
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

#' Adjust carrying capacity
#'
#' @inheritParams add_interventions
#'
#' @return modified parameter list
adjust_carrying_capacity <- function(p, interventions){
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

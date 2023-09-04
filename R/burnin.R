#' Expand interventions retrospectively for burn in period
#'
#' Assumes interventions for burn in period are = 0 coverage.
#'
#' @param interventions Site intervention inputs
#' @param burnin Burn in period (years)
#'
#' @return Intervention inputs with burn in
burnin_interventions <- function(interventions, burnin){
  start_year <- min(interventions$year) - burnin
  interventions <- interventions |>
    tidyr::complete(year = start_year:(max(interventions$year))) |>
    tidyr::replace_na(
      replace = list(
        itn_use = 0,
        itn_input_dist = 0,
        tx_cov = 0,
        irs_cov = 0,
        smc_cov = 0,
        rtss_cov = 0,
        pmc_cov = 0,
        lsm_cov = 0,
        stephensi_scaler = 1
      )
    ) |>
    tidyr::fill(dplyr::everything(), .direction = "up")
  return(interventions)
}

#' Expand demography retrospectively for burn in period
#'
#' Assumes demography for burn in period is = to those in year 1.
#'
#' @param demography Site demography inputs
#' @param burnin Burn in period (years)
#'
#' @return Demography inputs with burn in
burnin_demography <- function(demography, burnin){
  current_start_year <- min(as.integer(rownames(demography)))
  new_start_year <-  current_start_year - burnin
  burnin_years <- new_start_year:(current_start_year - 1)
  burnin_demog <- demography[rep(1, burnin), ]
  rownames(burnin_demog) <- burnin_years
  demography <- rbind(burnin_demog, demography)
  return(demography)
}

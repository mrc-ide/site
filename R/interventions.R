#' Add all interventions
#'
#' @param p parameter list
#' @param interventions site intervention inputs
#'
#' @return modified parameter list
add_interventions <- function(p, interventions, resistance, irs_adjust) {
  # Drug types
  p <- add_drugs(p)
  # Treatment
  if (sum(interventions$treatment$implementation$tx_cov) > 0) {
    p <- add_treatment(
      p = p,
      treatment = interventions$treatment
    )
  }
  # ITNs
  if (sum(interventions$itn$use$itn_use, na.rm = TRUE) > 0) {
    p <- add_itns(
      p = p,
      itn = interventions$itn,
      resistance = resistance
    )
  }
  # IRS
  if (sum(interventions$irs$implementation$irs_cov, na.rm = TRUE) > 0) {
    p <- add_irs(
      p = p,
      irs = interventions$irs,
      irs_adjust = irs_adjust
    )
  }
  # SMC
  if (
    sum(interventions$smc$implementation$smc_cov, na.rm = TRUE) > 0 &&
      p$parasite == "falciparum"
  ) {
    p <- add_smc(
      p = p,
      smc = interventions$smc
    )
  }
  # Vaccine
  if (
    (sum(interventions$vaccine$implementation$rtss_primary_cov, na.rm = TRUE) >
      0 ||
      sum(interventions$vaccine$implementation$r21_primary_cov, na.rm = TRUE) >
        0) &&
      p$parasite == "falciparum"
  ) {
    p <- add_vaccine(
      p = p,
      vaccine = interventions$vaccine
    )
  }
  # PMC
  if (
    sum(interventions$pmc$implementation$pmc_cov, na.rm = TRUE) > 0 &&
      p$parasite == "falciparum"
  ) {
    p <- add_pmc(
      p = p,
      pmc = interventions$pmc
    )
  }
  # Interventions that modify the carrying capacity
  # The combined carrying capacity scaling must be estimated for all
  # interventions that modify it, before updating. Currently, only LSM is
  # implemented here. This could included An. stephensi in the future.
  if (sum(interventions$lsm$implementation$lsm_cov, na.rm = TRUE) > 0) {
    p <- adjust_carrying_capacity(
      p = p,
      lsm = interventions$lsm
    )
  }

  return(p)
}

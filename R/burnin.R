#' Expand interventions retrospectively for burn in period
#'
#' Assumes interventions for burn in period are = to those in year 1.
#'
#' @param interventions Site intervention inputs
#' @param burnin Burn in period (years)
#'
#' @return Intervention inputs with burn in
burnin_interventions <- function(interventions, burnin){
  start_year <- min(interventions$year) - burnin
  interventions <- interventions |>
    tidyr::complete(year = start_year:(max(interventions$year))) |>
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
  start_year <- min(demography$year) - burnin
  demog_start <- demography[demography$year == min(demography$year), ]
  burnin_years <- start_year:(min(demography$year) - 1)
  demog_burnin <- dplyr::bind_rows(
    lapply(burnin_years, function(x, demog_start){
      demog_start |>
        dplyr::mutate(year = x)
    }, demog_start = demog_start)
  )
  demography <- dplyr::bind_rows(demog_burnin, demography)
  return(demography)
}

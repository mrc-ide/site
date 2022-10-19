#' Create site parameters
#'
#' @param interventions Site intervention inputs
#' @param demography Site demography inputs
#' @param vectors Site vectors inputs
#' @param seasonality Site seasonality inputs
#' @param min_ages Lower age bands for incidence and N age outputs
#' @param species Can be falciparum: "pf" or vivax: "pv", for vivax SMC, RTSS
#'  and PMC are not implemented
#' @param eir Site baseline EIR
#' @param overrides List of malariasimulation default parameter overrides
#' @param burnin Number of burn in years
#'
#' @return A malariasimulation parameter list
#' @export
site_parameters <- function(interventions, demography, vectors, seasonality,
                            min_ages = c(0, 5, 15) * 365, species = "pf",
                            eir = NULL, overrides = list(), burnin = 0){

  p <- malariasimulation::get_parameters(overrides = overrides)
  p$individual_mosquitoes <- FALSE

  p$burnin <- 0
  if(burnin > 0){
    p$burnin <- burnin
    interventions <- burnin_interventions(interventions, burnin)
    demography <- burnin_demography(demography, burnin)
  }

  p <- p |>
    add_time(interventions) |>
    add_seasonality(seasonality = seasonality) |>
    add_vectors(vectors = vectors) |>
    add_demography(demography = demography) |>
    add_interventions(interventions = interventions,
                      species = species) |>
    set_age_outputs(min_ages = min_ages)

  if(!is.null(eir)){
    p <- malariasimulation::set_equilibrium(p, init_EIR = eir)
  }

  return(p)
}

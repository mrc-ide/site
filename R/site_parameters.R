#' Create site parameters
#'
#' @param interventions Site intervention inputs
#' @param demography Site demography inputs
#' @param vectors Site vectors inputs
#' @param seasonality Site seasonality inputs
#' @param eir Site baseline EIR
#' @param overrides List of malariasimulation default parameter overrides
#'
#' @return A malariasimulation parameter list
#' @export
site_parameters <- function(interventions, demography, vectors, seasonality,
                            eir = NULL, overrides = list()){

  p <- malariasimulation::get_parameters(overrides = overrides)
  p$individual_mosquitoes <- FALSE

  p <- p |>
    add_time(interventions) |>
    add_seasonality(seasonality = seasonality) |>
    add_vectors(vectors = vectors) |>
    add_demography(demography = demography) |>
    add_interventions(interventions = interventions)

  if(!is.null(eir)){
    p <- malariasimulation::set_equilibrium(p, init_EIR = eir)
  }

  return(p)
}

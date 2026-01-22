#' Create site parameters
#'
#' @param interventions Site intervention inputs
#' @param demography Site demography inputs
#' @param vectors Site vectors inputs
#' @param seasonality Site seasonality inputs
#' @param min_ages Lower age bands for incidence and N age outputs
#' @param parasite Can be "falciparum" or "vivax" for vivax SMC, RTSS
#'  and PMC are not implemented
#' @param eir Site baseline EIR
#' @param draw malariasimulation parameter draw. Default NULL is best-fit parameter set
#' @param overrides List of malariasimulation default parameter overrides
#' @param start_year
#'
#' @return A malariasimulation parameter list
#' @export
site_parameters <- function(
  interventions,
  demography,
  vectors,
  seasonality,
  min_ages = c(0, 5, 15) * 365,
  parasite = "falciparum",
  eir = NULL,
  draw = NULL,
  overrides = list(),
  start_year,
  end_year,
  irs_adjust
) {
  # Baseline parameters
  p <- malariasimulation::get_parameters(
    overrides = overrides,
    parasite = parasite
  )
  # Parameter draw
  if (!is.null(draw)) {
    p <- malariasimulation::set_parameter_draw(p, draw)
  }
  # Additional "helper" parameters
  p$start_year <- start_year
  p$end_year <- end_year
  p$timesteps <- calculate_total_timesteps(start_year, end_year)

  # Site inputs
  p <- p |>
    add_seasonality(seasonality = seasonality) |>
    add_vectors(vectors = vectors) |>
    add_demography(demography = demography) |>
    add_interventions(interventions = interventions) |>
    set_age_outputs(min_ages = min_ages)

  # Transmission level
  if (!is.null(eir)) {
    p <- malariasimulation::set_equilibrium(p, init_EIR = eir)
  }

  return(p)
}

#' Create site parameters
#'
#' @param interventions Site intervention inputs
#' @param demography Site demography inputs
#' @param vectors Site vectors inputs
#' @param seasonality Site seasonality inputs
#' @param age_group Age breaks for population size outputs in days; default = c(0, 5, 15, 100) * 365
#' @param clinical_incidence Age breaks for clinical incidence outputs in days; default = c(0, 5, 15, 100) * 365
#' @param severe_incidence Age breaks for severe incidence outputs in days (pf only); default = c(0, 5, 15, 100) * 365
#' @param prevalence Age breaks for prevalence outputs in days; default = c(2, 10) * 365
#' @param parasite Can be "falciparum" or "vivax". For vivax, SMC, RTSS
#'  and PMC are not implemented
#' @param eir Site baseline EIR
#' @param draw malariasimulation parameter draw. Default NULL is best-fit parameter set
#' @param overrides List of malariasimulation default parameter overrides
#' @param start_year The year the simulation starts. This should be inclusive of any burn-in
#' (which is recommended). For example, if the site file data started in year 2000 and I wanted to
#' specify a 20 year burn-in I would set start_year to 1980. Interventions prior to the site file start year are assumed to be 0 coverage, and
#' demography set to the same profile as the first site file entry.
#' @param end_year The end year (inclusive) of the simulation
#' @param irs_adjust A scalar adjustment to IRS coverage inputs to account for observed discrepancies
#' between IRS impact in trial versus real-world implementation.
#'
#' @return A malariasimulation parameter list
#' @export
site_parameters <- function(
  interventions,
  demography,
  vectors,
  seasonality,
  age_group = c(0, 5, 15, 100) * 365,
  clinical_incidence = c(0, 5, 15, 100) * 365,
  severe_incidence = c(0, 5, 15, 100) * 365,
  prevalence = c(2, 10) * 365,
  parasite = "falciparum",
  eir = NULL,
  draw = NULL,
  overrides = list(),
  start_year,
  end_year,
  irs_adjust = 0.75
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
    add_seasonality(seasonality = seasonality$seasonality_parameters) |>
    add_vectors(vectors = vectors$vector_species) |>
    add_demography(demography = demography) |>
    add_interventions(
      interventions = interventions,
      resistance = vectors$pyrethroid_resistance,
      irs_adjust = irs_adjust
    )

  # Set epidemiological outputs
  if (p$parasite == "falciparum") {
    p <- malariasimulation::set_epi_outputs(
      parameters = p,
      age_group = age_group,
      clinical_incidence = clinical_incidence,
      severe_incidence = severe_incidence,
      prevalence = prevalence
    )
  } else if (p$parasite == "vivax") {
    p <- malariasimulation::set_epi_outputs(
      parameters = p,
      age_group = age_group,
      clinical_incidence = clinical_incidence,
      prevalence = prevalence
    )
  }

  # Transmission level
  if (!is.null(eir)) {
    p <- malariasimulation::set_equilibrium(p, init_EIR = eir)
  }

  return(p)
}

#' Set age outputs
#'
#' @param p parameter list
#' @param min_ages Minimum age bands for incidence and n_age outputs
#'
#' @return modified parameter list
set_age_outputs <- function(p, min_ages){
  # Max age upper bound is the maximimum of 100 years, or maximum of min_ages + 1 year
  max_upper_bound <- max(100 * 365, min_ages[length(min_ages)] + 365)
  ages <- c(min_ages, max_upper_bound)
  p <- p |>
    malariasimulation::set_epi_outputs(
      age_group = ages,
      clinical_incidence = ages,
      severe_incidence = ages
    )

  # PfPr: 2-10, PvPr: 1-100
  p$prevalence_rendering_min_ages = c(1, 2) * 365
  p$prevalence_rendering_max_ages = c(100, 10) * 365 - 1

  return(p)
}

#' Set age outputs
#'
#' @param p parameter list
#' @param min_ages Minimum age bands for incidence and n_age outputs
#'
#' @return modified parameter list
set_age_outputs <- function(p, min_ages){
  max_ages <- c(min_ages[-1], 100 * 365) - 1

  p$age_group_rendering_min_ages = min_ages
  p$age_group_rendering_max_ages = max_ages

  p$clinical_incidence_rendering_min_ages = min_ages
  p$clinical_incidence_rendering_max_ages = max_ages

  p$severe_incidence_rendering_min_ages = min_ages
  p$severe_incidence_rendering_max_ages = max_ages

  # PfPr: 2-10, PvPr: 1-100
  p$prevalence_rendering_min_ages = c(1, 2) * 365
  p$prevalence_rendering_max_ages = c(100, 10) * 365 - 1

  return(p)
}

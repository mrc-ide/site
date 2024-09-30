#' Add demography
#'
#' @param p parameter list
#' @param demography site demography
#'
#' @return modified parameter list
add_demography <- function(p, demography){

  # Age group upper
  ages <- round(unique(demography$age_upper) * 365)
  timesteps <- 365 * (unique(demography$year) - p$baseline_year)
  deathrates <- demography$adjusted_mortality_rates / 365
  deathrates_matrix <- matrix(deathrates, nrow = length(timesteps), byrow = TRUE)
  # Add parameters
  p <- malariasimulation::set_demography(
    parameters = p,
    agegroups = ages,
    timesteps = timesteps,
    deathrates = deathrates_matrix
  )

  return(p)
}

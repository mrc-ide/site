#' Add demography
#'
#' @param p parameter list
#' @param demography site demography
#'
#' @return modified parameter list
add_demography <- function(p, demography){

  # Age group upper
  ages <- round(unique(demography$age_upper) * 365)
  # Single demography currently
  timesteps <- 365 * (unique(demography$year) - p$baseline_year)
  # Take demography is first year as static demography
  deathrates <- demography$mortality_rate / 365
  # Create matrix of death rates
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

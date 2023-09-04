#' Add demography
#'
#' @param p parameter list
#' @param demography site demography
#'
#' @return modified parameter list
add_demography <- function(p, demography){

  # Age group upper
  ages <- as.integer(colnames(demography))
  # Single demography currently
  timesteps <- 365 * (as.integer(rownames(demography)) - p$baseline_year)
  # Add parameters
  p <- malariasimulation::set_demography(
    parameters = p,
    agegroups = ages,
    timesteps = timesteps,
    deathrates = demography
  )

  return(p)
}

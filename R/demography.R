#' Add demography
#'
#' @param p parameter list
#' @param demography site demography
#'
#' @return modified parameter list
add_demography <- function(p, demography) {
  ages <- round(unique(demography$age_upper) * 365)
  timesteps <- calendar_to_timestep(
    year = unique(demography$year),
    start_year = p$start_year
  )
  deathrates <- demography$adjusted_mortality_rates / 365
  deathrates_matrix <- matrix(
    deathrates,
    nrow = length(timesteps),
    byrow = TRUE
  )

  # Specify baseline demography
  timesteps <- c(0, timesteps)
  deathrates_matrix <- rbind(deathrates_matrix[1, ], deathrates_matrix)

  # Add parameters
  p <- malariasimulation::set_demography(
    parameters = p,
    agegroups = ages,
    timesteps = timesteps,
    deathrates = deathrates_matrix
  )

  return(p)
}

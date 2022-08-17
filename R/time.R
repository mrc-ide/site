add_time <- function(p, interventions){
  p$baseline_year <- min(interventions$year)
  p$timesteps <- ((diff(range(interventions$year)) + 1) * 365)
  return(p)
}

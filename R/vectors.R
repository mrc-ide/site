#' Add vectors
#'
#' @param p parameter list
#' @param vectors site vectors
#'
#' @return modified parameter list
add_vectors <- function(p, vectors){
  bionomics <- vectors[ , c("species", "blood_meal_rates", "foraging_time",
                          "Q0", "phi_bednets", "phi_indoors", "mum")]

  species <- list()
  for(s in 1:nrow(bionomics)){
    species[[s]] <- as.list(bionomics[s,])
  }

  p <- malariasimulation::set_species(
    parameters = p,
    species = species,
    proportions = vectors$prop)

  return(p)
}

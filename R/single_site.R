#' Extract a subset from a country site file
#'
#' @param site Country site file
#' @param site_filter Data.frame to filter site file elements by. Filtering will
#' be conducted on any matched columns for each element.
#'
#' @return Filtered site file
#' @export
subset_site <- function(site, site_filter){

  sub_site <- list()
  sub_site$country <- site$country
  sub_site$version <- site$version
  sub_site$admin_level <- site$admin_level
  sub_site$sites <- site_filter[ , sub_site$admin_level]
  sub_site$shape <- lapply(
    site$shape,
    match,
    y = site_filter
  )
  names(sub_site$shape) <- names(site$shape)
  sub_site$cases_deaths <- match(site$cases_deaths, site_filter)
  sub_site$prevalence <- match(site$prevalence, site_filter)
  sub_site$interventions <- match(site$interventions, sub_site$site)
  sub_site$population <- list(
    population_total = match(site$population$population_total, sub_site$site),
    population_by_age = match(site$population$population_by_age, sub_site$site)
  )
  sub_site$demography <- match(site$demography, sub_site$site)
  sub_site$vectors <-  list(
    vector_species = match(site$vectors$vector_species, sub_site$site),
    pyrethroid_resistance = match(site$vectors$pyrethroid_resistance, sub_site$site)
  )
  sub_site$seasonality <-  list(
    seasonality_parameters = match(site$seasonality$seasonality_parameters, sub_site$site),
    monthly_rainfall = match(site$seasonality$monthly_rainfall, sub_site$site),
    fourier_prediction = match(site$seasonality$fourier_prediction, sub_site$site)
  )
  sub_site$blood_disorders <- match(site$blood_disorders, sub_site$site)
  sub_site$accessibility <- match(site$accessibility, sub_site$site)
  sub_site$eir <- match(site$eir, site_filter)
  return(sub_site)
}

#' Matched join
#'
#' @param x Site file element
#' @param y Data.frame to match for
#'
#' @return Site file element filtered by y
match <- function(x, y){
  by_names <- names(y)[names(y) %in% names(x)]
  if(length(by_names) == 0){
    return(x)
  }
  y <- y[, by_names, drop = FALSE]
  y |>
    dplyr::left_join(x, by = by_names)
}

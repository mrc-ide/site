#' Extract a single site-input from a country site file
#'
#' @param site_file Country site file
#' @param index Index row from site_file$sites
#'
#' @return Single site
#' @export
single_site <- function(site_file, index){
  if(index < 1 | index > nrow(site_file$sites)){
    stop("Mis-specified site index")
  }
  index_site <- site_file$sites[index, ]

  to_mod <- c("sites", "interventions", "pyrethroid_resistance", "population",
              "vectors", "seasonality", "prevalence", "eir")

  site <- site_file
  for(level in to_mod){
    mc <- intersect(colnames(index_site), colnames(site[[level]]))
    site[[level]] <- dplyr::left_join(index_site, site[[level]], by = mc)
  }
  return(site)
}

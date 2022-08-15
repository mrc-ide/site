#' Extarcta a single site-input from a country site file
#'
#' @param site_file Country site file
#' @param index Index row from site_file$sites
#'
#' @return Single site
single_site <- function(site_file, index){
  if(index < 1 | index > nrow(site_file$sites)){
    stop("Mis-specified site index")
  }
  index_site <- site_file$sites[index, ]

  to_mod <- c("interventions", "pyrethroid_resistance", "population",
              "vectors", "seasonality", "eir")

  site <- site_file
  for(level in to_mod){
    site[[level]] <- merge(index_site, site[[level]])
  }
  return(site)
}

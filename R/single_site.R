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

  site <- list()
  for(level in names(site_file)){
    mc <- intersect(colnames(index_site), colnames(site_file[[level]]))
    if(length(mc) == 0){
      site[[level]] <- site_file[[level]]
    } else {
      site[[level]] <- dplyr::left_join(index_site, site_file[[level]], by = mc)
    }
  }
  return(site)
}

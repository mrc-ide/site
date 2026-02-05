#' Extract a subset from a country site file
#'
#' @param site Country site file
#' @param site_filter data frame to filter site file elements by. Filtering will
#' be conducted on any matched columns for each element.
#' @param skip Character vector of field names to preserve without filtering.
#' Default is c("country", "version", "admin_level", "sites").
#'
#' @return Filtered site file
#' @export
subset_site <- function(site, site_filter, skip = c("country", "version", "admin_level", "sites")){

  # Extract metadata fields that should be preserved
  sub_site <- list()
  for (name in skip) {
    if (name == "sites") {
      sub_site$sites <- site_filter[, site$admin_level, drop = FALSE]
    } else if (name %in% names(site)) {
      sub_site[[name]] <- site[[name]]
    }
  }

  # Recursively filter all other elements
  for (name in setdiff(names(site), skip)) {
    sub_site[[name]] <- filter_recursive(site[[name]], site_filter)
  }

  return(sub_site)
}

#' Recursively filter site file elements
#'
#' @param x Site file element (can be data.frame, list, or other)
#' @param site_filter data frame to filter by
#'
#' @return Filtered element
filter_recursive <- function(x, site_filter) {
  # If x is a data.frame, apply match_by_names
  if (is.data.frame(x)) {
    return(match_by_names(x, site_filter))
  }

  # If x is a list (but not a data.frame), recursively apply to each element
  if (is.list(x)) {
    result <- lapply(x, filter_recursive, site_filter = site_filter)
    # Preserve names
    names(result) <- names(x)
    return(result)
  }

  # Otherwise, return as-is (e.g., atomic vectors, NULL, etc.)
  return(x)
}

#' Matched join
#'
#' @param x Site file element
#' @param y data frame to match for
#'
#' @return Site file element filtered by y
match_by_names <- function(x, y){
  by_names <- names(y)[names(y) %in% names(x)]
  if(length(by_names) == 0){
    return(x)
  }
  y <- y[, by_names, drop = FALSE]
  y |>
    dplyr::left_join(x, by = by_names)
}

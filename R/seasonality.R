#' Add seasonality
#'
#' @param p parameter list
#' @param seasonality site seasonality
#'
#' @return modified parameter list
add_seasonality <- function(p, seasonality){
  # Switch seasonality on
  p$model_seasonality <- TRUE
  # rainfall fourier parameters
  p$g0 = seasonality$g0
  p$g = c(seasonality$g1, seasonality$g2, seasonality$g3)
  p$h = c(seasonality$h1, seasonality$h2, seasonality$h3)

  return(p)
}

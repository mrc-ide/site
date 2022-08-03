#' An example country site file
#'
#' A site file containing all of the input components for running a setting-specific
#' \href{https://mrc-ide.github.io/malariasimulation/}{malariasimulation} model run.
#'
#' @format A list with 10 variables:
#' \describe{
#'   \item{country}{The iso3c country code}
#'   \item{level}{The level of subnational disaggregation}
#'   \item{sites}{Unique sites}
#'   \item{epi}{Epidemiological site data}
#'   \item{interventions}{Intervention coverage and specification}
#'   \item{population}{Population and population at risk projections}
#'   \item{demography}{Demographic projections}
#'   \item{vectors}{Vector proportions}
#'   \item{seasonality}{Seasonal profile parameters}
#' }
"example_site"

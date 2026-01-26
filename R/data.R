#' IRS insecticide efficacy parameters
#'
#' A dataset containing efficacy parameters for different Indoor Residual Spraying (IRS)
#' insecticides. These parameters are used by malariasimulation to model the impact of
#' IRS interventions on mosquito populations.
#'
#' @format A data frame with 4 rows and 7 columns:
#' \describe{
#'   \item{insecticide}{Character. Name of the insecticide (ddt, actellic, bendiocarb, sumishield)}
#'   \item{ls_theta}{Numeric. Theta parameter for mosquito repelling effect}
#'   \item{ls_gamma}{Numeric. Gamma parameter for mosquito repelling effect}
#'   \item{ks_theta}{Numeric. Theta parameter for mosquito killing effect}
#'   \item{ks_gamma}{Numeric. Gamma parameter for mosquito killing effect}
#'   \item{ms_theta}{Numeric. Theta parameter for successful feeding reduction}
#'   \item{ms_gamma}{Numeric. Gamma parameter for successful feeding reduction}
#' }
#' @source Parameterized from field trial data for malariasimulation
"irs_efficacy"

#' ITN net efficacy parameters
#'
#' A dataset containing efficacy parameters for Insecticide Treated Nets (ITNs)
#' based on pyrethroid resistance levels and net type. These parameters are used
#' by malariasimulation to model the protective effect of bed nets.
#'
#' @format A data frame with 303 rows and 7 columns:
#' \describe{
#'   \item{pyrethroid_resistance}{Numeric. Pyrethroid resistance level (0 to 1)}
#'   \item{bioassay_mortality}{Numeric. Bioassay mortality rate}
#'   \item{dn0}{Numeric. Initial deterrence parameter}
#'   \item{rn0}{Numeric. Initial killing parameter}
#'   \item{gamman}{Numeric. Net half-life parameter (in days)}
#'   \item{rnm}{Numeric. Minimum killing rate parameter}
#'   \item{net_type}{Character. Type of net (e.g., pyrethroid_only, pyrethroid_pbo, pyrethroid_pyrrole)}
#' }
#' @source Parameterized from field trial data for malariasimulation
"net_efficacy"

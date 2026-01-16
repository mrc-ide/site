#' Add IRS
#'
#' @param p parameter list
#' @param irs interventions irs object
#' @param irs_adjust Operational downscaling of irs coverage
#'
#' @return modified parameter list
add_irs <- function(p, irs, irs_adjust = 1) {
  if (irs_adjust > 1 | irs_adjust < 0) {
    cli::cli_abort(c(
      "!" = "{.field irs_adjust} must be between 0 and 1",
      "x" = "You provided: {.val {irs_adjust}}"
    ))
  }
  supported_insecticide <- unique(site::irs_efficacy$insecticide)
  if (!all(unique(irs$implementation$insecticide) %in% supported_insecticide)) {
    unsupported <- setdiff(
      unique(irs$implementation$insecticide),
      supported_insecticide
    )
    wrong_irs_type(supported_insecticide, unsupported)
  }

  # Adjust coverage to capture lower operational impact than parameterisation
  irs$implementation$irs_cov <- irs$implementation$irs_cov * irs_adjust

  # Join to insecticide parameters
  irs$implementation <- irs$implementation |>
    dplyr::left_join(site::irs_efficacy, by = "insecticide")
  # Drop negative spray times
  irs$implementation <- irs$implementation[irs$implementation$spray_day > 0, ]

  check_for_nas(irs$implementation)

  n_species <- length(p$species)
  ls_theta <- matrix(
    rep(irs$implementation$ls_theta, n_species),
    ncol = n_species
  )
  ls_gamma <- matrix(
    rep(irs$implementation$ls_gamma, n_species),
    ncol = n_species
  )
  ks_theta <- matrix(
    rep(irs$implementation$ks_theta, n_species),
    ncol = n_species
  )
  ks_gamma <- matrix(
    rep(irs$implementation$ks_gamma, n_species),
    ncol = n_species
  )
  ms_theta <- matrix(
    rep(irs$implementation$ms_theta, n_species),
    ncol = n_species
  )
  ms_gamma <- matrix(
    rep(irs$implementation$ms_gamma, n_species),
    ncol = n_species
  )

  p <- malariasimulation::set_spraying(
    parameters = p,
    timesteps = irs$implementation$spray_day,
    coverages = irs$implementation$irs_cov,
    ls_theta = ls_theta,
    ls_gamma = ls_gamma,
    ks_theta = ks_theta,
    ks_gamma = ks_gamma,
    ms_theta = ms_theta,
    ms_gamma = ms_gamma
  )

  return(p)
}

wrong_irs_type <- function(supported_insecticde, unsupported) {
  cli::cli_abort(c(
    "!" = "Unsupported insecticide{?s} found: {.val {unsupported}}",
    " " = "",
    "i" = "Supported insecticides are:",
    "*" = "{.val {supported_insecticde}}",
    " " = "",
    "x" = "Please check your {.field insecticide} column in {.code irs$implementation}"
  ))
}

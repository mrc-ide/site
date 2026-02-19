#' Add ITNs
#'
#' @param p parameter list
#' @param itn interventions ITN object
#' @param resistance vectors resistance object
#'
#' @return modified parameter list
add_itns <- function(p, itn, resistance) {
  if (!"itn_input_dist" %in% names(itn$implementation)) {
    stop_missing_itn_input_dist()
  }

  # Link to efficacy parameters based on net-type and insecticide resistance
  supported_nets <- unique(site::net_efficacy$net_type)
  if (!all(unique(itn$implementation$net_type) %in% supported_nets)) {
    unsupported <- setdiff(unique(itn$implementation$net_type), supported_nets)
    wrong_net_type(supported_nets, unsupported)
  }
  join_cols <- intersect(colnames(itn$implementation), colnames(resistance))
  if (length(join_cols) == 0) {
    cli::cli_abort(c(
      "x" = "No shared columns between {.code itn$implementation} and {.code resistance}.",
      "i" = "Expected at least one shared join key (e.g., {.val {c('name', 'year')}})."
    ))
  }
  resistance$pyrethroid_resistance <- round(resistance$pyrethroid_resistance, 2)
  itn$implementation <- itn$implementation |>
    dplyr::left_join(resistance, by = join_cols) |>
    dplyr::left_join(
      site::net_efficacy,
      by = c("net_type", "pyrethroid_resistance")
    )
  check_for_nas(itn$implementation)

  timesteps <- calendar_to_timestep(
    year = itn$implementation$year,
    day_of_year = itn$implementation$distribution_day_of_year,
    start_year = p$start_year
  )

  # Net efficacy parameters
  n_species <- length(p$species)
  dn0 <- matrix(rep(itn$implementation$dn0, n_species), ncol = n_species)
  rn <- matrix(rep(itn$implementation$rn0, n_species), ncol = n_species)
  rnm <- matrix(rep(itn$implementation$rnm, n_species), ncol = n_species)
  gamman <- itn$implementation$gamman

  p <- malariasimulation::set_bednets(
    parameters = p,
    timesteps = timesteps,
    coverages = itn$implementation$itn_input_dist,
    dn0 = dn0,
    rn = rn,
    rnm = rnm,
    gamman = gamman,
    logistic_half_life = itn$retention_half_life,
    logistic_k = 20
  )

  return(p)
}

#' Convert ITN usage to model input distributions
#'
#' A convenience wrapper around [netz::usage_to_model_distribution()] that
#' accepts year and day-of-year inputs (as found in site files) rather than
#' pre-computed timesteps.
#'
#' @param usage Numeric vector of ITN usage values.
#' @param usage_year Integer vector of years corresponding to `usage`.
#' @param usage_day_of_year Integer vector of days of the year corresponding to
#'   `usage`.
#' @param distribution_year Integer vector of years for distribution time
#'   points.
#' @param distribution_day_of_year Integer vector of days of the year for
#'   distribution time points.
#' @param distribution_lower Numeric vector of lower bounds for possible
#'   distributions. Defaults to 0 for each distribution time point.
#' @param distribution_upper Numeric vector of upper bounds for possible
#'   distributions. Defaults to 1 for each distribution time point.
#' @param net_loss_function Function describing net loss over time. Defaults to
#'   [netz::net_loss_exp].
#' @param ... Additional arguments passed to
#'   [netz::usage_to_model_distribution()] (e.g. `half_life`).
#'
#' @return Numeric vector of model input distribution values, one per
#'   distribution time point.
#'
#' @export
site_usage_to_model_distribution <- function(
  usage,
  usage_year,
  usage_day_of_year,
  distribution_year,
  distribution_day_of_year,
  distribution_lower = NULL,
  distribution_upper = NULL,
  net_loss_function = netz::net_loss_exp,
  ...
) {
  reference_start_year <- min(c(usage_year, distribution_year))

  usage_timesteps <- calendar_to_timestep(
    year = usage_year,
    day_of_year = usage_day_of_year,
    start_year = reference_start_year
  )

  distribution_timesteps <- calendar_to_timestep(
    year = distribution_year,
    day_of_year = distribution_day_of_year,
    start_year = reference_start_year
  )

  if (is.null(distribution_lower)) {
    distribution_lower <- rep(0, length(distribution_timesteps))
  }
  if (is.null(distribution_upper)) {
    distribution_upper <- rep(1, length(distribution_timesteps))
  }

  netz::usage_to_model_distribution(
    usage = usage,
    usage_timesteps = usage_timesteps,
    distribution_timesteps = distribution_timesteps,
    distribution_lower = distribution_lower,
    distribution_upper = distribution_upper,
    net_loss_function = net_loss_function,
    ...
  )
}

#' Convert model input distributions to expected ITN usage
#'
#' A convenience wrapper around [netz::model_distribution_to_usage()] that
#' accepts year and day-of-year inputs (as found in site files) rather than
#' pre-computed timesteps.
#'
#' @param distribution Numeric vector of model input distribution values.
#' @param usage_year Integer vector of years at which to estimate usage.
#' @param usage_day_of_year Integer vector of days of the year at which to
#'   estimate usage.
#' @param distribution_year Integer vector of years for distribution time
#'   points.
#' @param distribution_day_of_year Integer vector of days of the year for
#'   distribution time points.
#' @param net_loss_function Function describing net loss over time. Defaults to
#'   [netz::net_loss_exp].
#' @param ... Additional arguments passed to
#'   [netz::model_distribution_to_usage()] (e.g. `half_life`).
#'
#' @return Numeric vector of expected usage values, one per usage time point.
#'
#' @export
site_model_distribution_to_usage <- function(
  distribution,
  usage_year,
  usage_day_of_year,
  distribution_year,
  distribution_day_of_year,
  net_loss_function = netz::net_loss_exp,
  ...
) {
  reference_start_year <- min(c(usage_year, distribution_year))

  usage_timesteps <- calendar_to_timestep(
    year = usage_year,
    day_of_year = usage_day_of_year,
    start_year = reference_start_year
  )

  distribution_timesteps <- calendar_to_timestep(
    year = distribution_year,
    day_of_year = distribution_day_of_year,
    start_year = reference_start_year
  )

  netz::model_distribution_to_usage(
    usage_timesteps = usage_timesteps,
    distribution = distribution,
    distribution_timesteps = distribution_timesteps,
    net_loss_function = net_loss_function,
    ...
  )
}

stop_missing_itn_input_dist <- function() {
  cli::cli_abort(c(
    "x" = "Missing required column: {.field itn_input_dist} in {.code itn$implementation}",
    " " = "",
    "i" = "The {.field itn_input_dist} column specifies the timing and magnitude of ITN distributions in the model.",
    " " = "",
    "i" = "{.strong Why is this needed?}",
    " " = "The {.field itn_use} column we want to match represents the cumulative result of multiple ITN distributions over time and how long nets are retained for. To accurately match ITN use in the model, we need to infer the modelled ITN distributions (when they occur and their size). This is what the {.field itn_input_dist} column specifies.",
    " " = "",
    "v" = "{.strong How to create this column:}",
    " " = "",
    "1" = "{.strong Calculate input distributions with {.fn site::site_usage_to_model_distribution}:}",
    " " = "  site$interventions$itn$implementation$itn_input_dist <- site::site_usage_to_model_distribution(",
    " " = "    usage = site$interventions$itn$use$itn_use,",
    " " = "    usage_year = site$interventions$itn$use$year,",
    " " = "    usage_day_of_year = site$interventions$itn$use$usage_day_of_year,",
    " " = "    distribution_year = site$interventions$itn$implementation$year,",
    " " = "    distribution_day_of_year = site$interventions$itn$implementation$distribution_day_of_year,",
    " " = "    distribution_lower = site$interventions$itn$implementation$distribution_lower,",
    " " = "    distribution_upper = site$interventions$itn$implementation$distribution_upper,",
    " " = "    net_loss_function = netz::net_loss_map,",
    " " = "    half_life = site$interventions$itn$retention_half_life)",
    " " = "",
    "!" = "{.strong Important:}",
    " " = "Pay strict attention to your {.field distribution_upper} specification. This determines how regularly large distributions can occur. For example, for a regular 3-yearly mass distribution cycle, {.code distribution_upper = 1} should only be specified every 3 years, with other timesteps set to 0.",
    " " = "",
    "!" = "{.strong Important:}",
    " " = "Do this for one location at a time (times/years must be monotonically increasing)",
    " " = "",
    "!" = "{.strong Recommended validation step:}",
    " " = "Check your predicted usage against target usage using {.fn site::site_model_distribution_to_usage}:",
    " " = "",
    " " = "  site$interventions$itn$use$expected_use <- site::site_model_distribution_to_usage(",
    " " = "    distribution = site$interventions$itn$implementation$itn_input_dist,",
    " " = "    usage_year = site$interventions$itn$use$year,",
    " " = "    usage_day_of_year = site$interventions$itn$use$usage_day_of_year,",
    " " = "    distribution_year = site$interventions$itn$implementation$year,",
    " " = "    distribution_day_of_year = site$interventions$itn$implementation$distribution_day_of_year,",
    " " = "    net_loss_function = netz::net_loss_map,",
    " " = "    half_life = site$interventions$itn$retention_half_life)",
    " " = "",
    " " = "Then plot to compare usage:",
    " " = "  plot(itn_use ~ year, data = site$interventions$itn$use)",
    " " = "  lines(expected_use ~ year, data = site$interventions$itn$use, col = \"darkred\")",
    " " = "",
    "!" = "{.strong Recommended validation step:}",
    " " = "Check the distribution timing and magnitude looks sensible:",
    " " = "",
    " " = "  plot(site$interventions$itn$implementation$itn_input_dist ~ distribution_timesteps, type = \"h\")"
  ))
}

wrong_net_type <- function(supported_nets, unsupported) {
  cli::cli_abort(c(
    "!" = "Unsupported net type{?s} found: {.val {unsupported}}",
    " " = "",
    "i" = "Supported net types are:",
    "*" = "{.val {supported_nets}}",
    " " = "",
    "x" = "Please check your {.field net_type} column in {.code itn$implementation}"
  ))
}

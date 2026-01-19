#' Add ITNs
#'
#' @param p parameter list
#' @param itn interventions itn object
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
    start_year = p0$start_year
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

stop_missing_itn_input_dist <- function() {
  cli::cli_abort(c(
    "x" = "Missing required column: {.field itn_input_dist} in {.code itns$implementation}",
    " " = "",
    "i" = "The {.field itn_input_dist} column specifies the timing and magnitude of ITN 
    distributions in the model.",
    " " = "",
    "i" = "{.strong Why is this needed?}",
    " " = "The {.field itn_use} column we want to match represents the cumulative results of the
     multiple ITN distributions over time and how long nets are retained for.
      To accurately match ITN use in the model, we need to infer the 
    modelled ITN distributions (when they occur and their size). This is what 
    the {.field itn_input_dist} column specifies.",
    " " = "",
    "v" = "{.strong How to create this column:}",
    " " = "Use the {.fn netz::usage_to_model_distribution} function with these arguments:",
    "*" = "{.code usage = interventions$itn$use$itn_use}",
    "*" = "{.code usage_timesteps = interventions$itn$use$usage_timestep}",
    "*" = "{.code distribution_timesteps = interventions$itn$implementation$distribution_timestep}",
    "*" = "{.code distribution_lower = interventions$itn$implementation$distribution_lower}",
    "*" = "{.code distribution_upper = interventions$itn$implementation$distribution_upper}",
    "*" = "{.code net_loss_function = netz::net_loss_map}",
    "*" = "{.code half_life = interventions$itn$retention_half_life}",
    " " = "Assign this to {.code interventions$itn$implementation$itn_input_dist}",
    " " = "",
    "!" = "{.strong Important:}",
    " " = "Pay strict attention to your {.field distribution_upper} 
    specification.",
    " " = "This determines how regularly large distributions can occur. For example, for a 
    regular 3-yearly mass distribution cycle, {.code distribution_upper = 1} should only be 
    specified every 3 years, with other timesteps set to 0.",
    " " = "",
    "!" = "{.strong Recommended validation step:}",
    " " = "Running {.fn netz::model_distribution_to_usage} does
    not guarantee that you will have captured {.code interventions$itn$use$itn_use} well, or
    realistically. For example, you may not match {.code interventions$itn$use$itn_use} closely,
    or you might be distributing and replacing ITNs too frequently.
    Check your predicted usage against target usage using 
    {.fn netz::model_distribution_to_usage} with the same arguments plus 
    {.code distribution = itn_input_dist}."
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

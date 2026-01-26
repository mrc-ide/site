#' Create example IRS intervention data
#'
#' Creates a simple IRS (Indoor Residual Spraying) intervention list for testing
#' purposes with basic implementation parameters.
#'
#' @return A list containing an `implementation` data frame with columns:
#'   name, year, irs_cov, peak_season, insecticide, round, spray_day_of_year
create_example_irs <- function() {
  list(
    implementation = data.frame(
      name = "place",
      year = 2000:2002,
      irs_cov = 0.1,
      peak_season = 100,
      insecticide = c("ddt", "actellic", "sumishield"),
      round = 1,
      spray_day_of_year = 70
    )
  )
}

#' Create example ITN intervention data
#'
#' Creates an ITN (Insecticide Treated Nets) intervention list for testing with
#' retention parameters, usage data, and multiple distribution events including
#' mass and routine distributions across different net types.
#'
#' @return A list containing:
#'   - `retention_half_life`: Numeric half-life of net retention
#'   - `use`: Data frame with name, year, itn_use, usage_day_of_year
#'   - `implementation`: Data frame with distribution details including net_type,
#'     distribution_type, distribution bounds, and distribution_day_of_year
create_example_itn <- function() {
  example_itns <- list(
    retention_half_life = 600,
    use = data.frame(
      name = "place",
      year = 2000:2002,
      itn_use = c(0.1, 0.2, 0.5),
      usage_day_of_year = 1
    ),
    implementation = data.frame(
      name = "place",
      year = rep(2000:2002, each = 5),
      net_type = rep(
        c("pyrethroid_only", "pyrethroid_pbo", "pyrethroid_pyrrole"),
        each = 5
      ),
      distribution_type = rep(c("mass", rep("routine", 4)), 3),
      distribution_lower = 0,
      distribution_upper = rep(c(1, rep(0.01, 4)), 3),
      distribution_day_of_year = rep(c(1, 2, 90, 180, 270), 3)
    )
  )
  # Remove distirbution inputs past last usage data date
  example_itns$implementation <- example_itns$implementation[
    example_itns$implementation$year < 2002 |
      example_itns$implementation$distribution_type == "mass",
  ]

  return(example_itns)
}

#' Create example resistance data
#'
#' Creates a simple pyrethroid resistance data frame for testing with resistance
#' levels varying over time.
#'
#' @return A data frame with columns: name, year, pyrethroid_resistance
create_example_resistance <- function() {
  data.frame(
    name = "place",
    year = 2000:2002,
    pyrethroid_resistance = c(0.2345, 0.3283, 0.224)
  )
}

#' Create example SMC intervention data
#'
#' Creates an SMC (Seasonal Malaria Chemoprevention) intervention list for testing
#' with drug type and implementation parameters including multiple rounds per year.
#'
#' @return A list containing:
#'   - `drug`: Character drug combination identifier
#'   - `implementation`: Data frame with name, year, smc_cov, peak_season,
#'     age limits (smc_min_age, smc_max_age), round, and round_day_of_year
create_example_smc <- function() {
  list(
    drug = "sp_aq",
    implementation = data.frame(
      name = "place",
      year = rep(2000:2002, each = 2),
      smc_cov = 0.1,
      peak_season = 45,
      smc_min_age = 91,
      smc_max_age = 1825,
      round = rep(1:2, 3),
      round_day_of_year = rep(c(30, 60), 3)
    )
  )
}

#' Create example PMC intervention data
#'
#' Creates a PMC (Perennial Malaria Chemoprevention) intervention list for testing
#' with drug type, target ages, and coverage implementation over multiple years.
#'
#' @return A list containing:
#'   - `drug`: Character drug identifier
#'   - `age`: Numeric vector of target ages in days
#'   - `implementation`: Data frame with name, year, pmc_coverage_timesteps, pmc_cov
create_example_pmc <- function() {
  list(
    drug = "sp",
    age = c(60, 90, 270),
    implementation = data.frame(
      name = "place",
      year = 2000:2003,
      pmc_coverage_timesteps = ((2000:2003) - 2000) * 365 + 1,
      pmc_cov = 0.1
    )
  )
}

#' Create vaccine intervention examples
#'
#' @param vaccine Character. Vaccine type: "rtss", "r21", or "rtss_r21"
#' @param delivery Character. Delivery method: "age-based" or "hybrid"
#' @param boosters Character. Booster schedule: "none", "single", or "multiple"
#' @param primary_cov Numeric. Primary vaccination coverage (0-1). Default: 0.8
#' @param booster_cov Numeric. Booster vaccination coverage (0-1). Default: 0.4
#' @param years Numeric vector. Years of implementation. Default: 2000:2002
#' @param name Character. Location name. Default: "place"
#' @param peak_season Numeric. Peak transmission day of year. Default: 100
#' @param primary_schedule Numeric vector. Primary vaccination ages in days. Default: c(180, 210, 240)
create_example_vaccine <- function(
  vaccine = "rtss",
  delivery = "age-based",
  boosters = "none",
  primary_cov = 0.8,
  booster_cov = 0.4,
  years = 2000:2002,
  name = "place",
  peak_season = 100,
  primary_schedule = c(180, 210, 240)
) {
  # Input validation
  vaccine <- match.arg(vaccine, c("rtss", "r21", "rtss_r21"))
  delivery <- match.arg(delivery, c("age-based", "hybrid"))
  boosters <- match.arg(boosters, c("none", "single", "multiple"))

  if (primary_cov < 0 || primary_cov > 1) {
    stop("primary_cov must be between 0 and 1")
  }
  if (booster_cov < 0 || booster_cov > 1) {
    stop("booster_cov must be between 0 and 1")
  }

  # Set up booster spacing based on delivery and boosters
  booster_spacing <- switch(
    paste(delivery, boosters, sep = "_"),
    "age-based_none" = 365,
    "age-based_single" = 365,
    "age-based_multiple" = c(365, 365 * 2),
    "hybrid_none" = NA,
    "hybrid_single" = NA,
    "hybrid_multiple" = c(NA, 365)
  )

  # Set up coverage columns based on vaccine type and boosters
  implementation <- data.frame(
    name = name,
    year = years,
    peak_season = peak_season,
    rtss_primary_cov = 0,
    rtss_booster1_cov = 0,
    r21_primary_cov = 0,
    r21_booster1_cov = 0
  )

  # Set primary coverage
  if (vaccine %in% c("rtss", "rtss_r21")) {
    implementation$rtss_primary_cov <- primary_cov
  }
  if (vaccine %in% c("r21", "rtss_r21")) {
    implementation$r21_primary_cov <- ifelse(
      vaccine == "rtss_r21",
      0.3,
      primary_cov
    )
  }

  # Set booster coverage
  if (boosters != "none") {
    if (vaccine %in% c("rtss", "rtss_r21")) {
      implementation$rtss_booster1_cov <- booster_cov
    }
    if (vaccine %in% c("r21", "rtss_r21")) {
      booster_val <- ifelse(vaccine == "rtss_r21", 0.2, booster_cov)
      implementation$r21_booster1_cov <- booster_val
    }
  }

  # Add second booster column if needed
  if (boosters == "multiple") {
    implementation$rtss_booster2_cov <- ifelse(
      vaccine %in% c("rtss", "rtss_r21"),
      0.3,
      0
    )
    implementation$r21_booster2_cov <- ifelse(
      vaccine %in% c("r21", "rtss_r21"),
      0.3,
      0
    )
  }

  # Add hybrid booster day column for age-based delivery
  if (delivery == "age-based") {
    implementation$hybrid_booster_day_of_year <- NA
  }

  # Return the list
  list(
    delivery = delivery,
    primary_schedule = primary_schedule,
    booster_spacing = booster_spacing,
    implementation = implementation
  )
}

#' Create example LSM intervention data
#'
#' Creates a simple LSM (Larval Source Management) intervention list for testing
#' with basic implementation parameters.
#'
#' @return A list containing an `implementation` data frame with columns:
#'   name, year, lsm_cov
create_lsm_example <- function() {
  list(
    implementation = data.frame(
      name = "place",
      year = 2000:2002,
      lsm_cov = 0.2
    )
  )
}

#' Create example treatment intervention data
#'
#' Creates a treatment intervention list for testing with coverage and ACT
#' (Artemisinin-based Combination Therapy) proportion parameters.
#'
#' @return A list containing an `implementation` data frame with columns:
#'   name, year, tx_cov, prop_act
create_example_treatment <- function() {
  list(
    implementation = data.frame(
      name = "place",
      year = 2000:2002,
      tx_cov = 0.1,
      prop_act = 0.2
    )
  )
}

#' Create complete set of example interventions
#'
#' Creates a comprehensive list containing all intervention types (treatment, ITNs,
#' IRS, SMC, PMC, vaccine, and LSM) using default example parameters. Useful for
#' testing functions that require a full intervention specification.
#'
#' @return A named list with elements: treatment, itns, irs, smc, pmc, vaccine, lsm.
#'   Each element contains the output from the corresponding `create_example_*` function.
#' @keywords internal
#' Create complete set of example interventions
#'
#' Creates a comprehensive list containing all intervention types (treatment, ITNs,
#' IRS, SMC, PMC, vaccine, and LSM) using default example parameters. Useful for
#' testing functions that require a full intervention specification.
#'
#' @return A named list with elements: treatment, itns, irs, smc, pmc, vaccine, lsm.
#'   Each element contains the output from the corresponding `create_example_*` function.
create_example_interventions <- function() {
  list(
    treatment = create_example_treatment(),
    itns = create_example_itn(),
    irs = create_example_irs(),
    smc = create_example_smc(),
    pmc = create_example_pmc(),
    vaccine = create_example_vaccine(),
    lsm = create_example_lsm()
  )
}

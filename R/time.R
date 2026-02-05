#' Calculate total model timesteps
#'
#' @param start_year Numeric scalar. First year of the model period
#' @param end_year Numeric scalar. Last year of the model period (inclusive)
#'
#' @return Numeric scalar. Total number of timesteps (days) for the model run
#'
#' @details
#' Total runtime = (end_year - start_year + 1) years.
#' All years are treated as 365 days (no leap year adjustment).
#'
#' @examples
#' # Data from 2000-2020
#' calculate_total_timesteps(start_year = 2000, end_year = 2020)
#' # Returns: 7665 (21 years * 365 days)
#'
#' # Short period
#' calculate_total_timesteps(start_year = 2000, end_year = 2005)
#' # Returns: 2190 (6 years * 365 days)
#'
#' @export
calculate_total_timesteps <- function(start_year, end_year) {
  # Input validation
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("All inputs must be numeric")
  }

  if (length(start_year) != 1 || length(end_year) != 1) {
    stop("All inputs must be scalars (length 1)")
  }

  if (end_year < start_year) {
    stop("end_year must be >= start_year")
  }

  if (any(!is.finite(c(start_year, end_year)))) {
    stop("All inputs must be finite (no NA, NaN, or Inf values)")
  }

  # Calculate total years
  total_years <- end_year - start_year + 1

  # Convert to timesteps (days)
  total_timesteps <- total_years * 365

  return(total_timesteps)
}

#' Convert calendar dates to model timesteps
#'
#' @param year Numeric vector of years (e.g., c(2000, 2001, 2005))
#' @param day_of_year Numeric vector of day within year (1-365)
#' @param start_year Numeric scalar indicating the model start year
#'
#' @return Numeric vector of model timesteps in days since model start
#'
#' @details
#' Model timestep 1 corresponds to January 1st of the start_year.
#' All years are treated as 365 days (no leap year adjustment).
#'
#' @examples
#' # Model starts in 1980, interventions begin in 2000
#' calendar_to_timestep(year = 2000, day_of_year = 1, start_year = 1980)
#' # Returns: 7306 (20 years * 365 days + 1)
#'
#' # Multiple dates
#' calendar_to_timestep(
#'   year = c(2000, 2001, 2005),
#'   day_of_year = c(1, 100, 200),
#'   start_year = 1980
#' )
#'
#' @export
calendar_to_timestep <- function(
  year,
  day_of_year = rep(1, length(year)),
  start_year
) {
  # Input validation
  if (
    !is.numeric(year) || !is.numeric(day_of_year) || !is.numeric(start_year)
  ) {
    stop("All inputs must be numeric")
  }

  if (length(start_year) != 1) {
    stop("start_year must be a scalar (length 1)")
  }

  if (length(year) != length(day_of_year)) {
    stop("year and day_of_year must have the same length")
  }

  if (any(day_of_year < 1 | day_of_year > 365)) {
    stop("day_of_year must be between 1 and 365")
  }

  if (any(year < start_year)) {
    stop("All years must be >= start_year")
  }

  if (any(!is.finite(c(year, day_of_year, start_year)))) {
    stop("All inputs must be finite (no NA, NaN, or Inf values)")
  }

  # Calculate timesteps
  # Days from start_year to beginning of target year
  years_elapsed <- year - start_year
  days_from_years <- years_elapsed * 365

  # Add the specific day within the target year
  timestep <- days_from_years + day_of_year

  if (length(timestep) > 1 && any(diff(timestep) < 0)) {
    stop(
      "Resulting timesteps are not monotonically increasing. ",
      "Ensure your time inputs are ordered by time (year, then day of year), ",
      "or sort them before calculating timesteps."
    )
  }

  return(timestep)
}

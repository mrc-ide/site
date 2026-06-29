# Convert calendar dates to model timesteps

Convert calendar dates to model timesteps

## Usage

``` r
calendar_to_timestep(year, day_of_year = rep(1, length(year)), start_year)
```

## Arguments

- year:

  Numeric vector of years (e.g., c(2000, 2001, 2005))

- day_of_year:

  Numeric vector of day within year (1-365)

- start_year:

  Numeric scalar indicating the model start year

## Value

Numeric vector of model timesteps in days since model start

## Details

Model timestep 1 corresponds to January 1st of the start_year. All years
are treated as 365 days (no leap year adjustment).

## Examples

``` r
# Model starts in 1980, interventions begin in 2000
calendar_to_timestep(year = 2000, day_of_year = 1, start_year = 1980)
#> [1] 7301
# Returns: 7306 (20 years * 365 days + 1)

# Multiple dates
calendar_to_timestep(
  year = c(2000, 2001, 2005),
  day_of_year = c(1, 100, 200),
  start_year = 1980
)
#> [1] 7301 7765 9325
```

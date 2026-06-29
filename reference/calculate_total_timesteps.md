# Calculate total model timesteps

Calculate total model timesteps

## Usage

``` r
calculate_total_timesteps(start_year, end_year)
```

## Arguments

- start_year:

  Numeric scalar. First year of the model period

- end_year:

  Numeric scalar. Last year of the model period (inclusive)

## Value

Numeric scalar. Total number of timesteps (days) for the model run

## Details

Total runtime = (end_year - start_year + 1) years. All years are treated
as 365 days (no leap year adjustment).

## Examples

``` r
# Data from 2000-2020
calculate_total_timesteps(start_year = 2000, end_year = 2020)
#> [1] 7665
# Returns: 7665 (21 years * 365 days)

# Short period
calculate_total_timesteps(start_year = 2000, end_year = 2005)
#> [1] 2190
# Returns: 2190 (6 years * 365 days)
```

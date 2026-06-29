# Prepare ITN data for intervention plot

Prepare ITN data for intervention plot

## Usage

``` r
prepare_itn(itn_implementation, itn_use, retention_half_life, years)
```

## Arguments

- itn_implementation:

  Data frame with ITN distribution data including itn_input_dist and
  distribution_day_of_year columns.

- itn_use:

  Data frame with ITN usage data including itn_use and usage_day_of_year
  columns.

- retention_half_life:

  Numeric net retention half-life in days.

- years:

  Numeric vector of years.

## Value

A list with elements: model_use, itn_use, itn_input_dist.

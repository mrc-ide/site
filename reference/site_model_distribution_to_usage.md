# Convert model input distributions to expected ITN usage

A convenience wrapper around \[netz::model_distribution_to_usage()\]
that accepts year and day-of-year inputs (as found in site files) rather
than pre-computed timesteps.

## Usage

``` r
site_model_distribution_to_usage(
  distribution,
  usage_year,
  usage_day_of_year,
  distribution_year,
  distribution_day_of_year,
  net_loss_function = netz::net_loss_map,
  ...
)
```

## Arguments

- distribution:

  Numeric vector of model input distribution values.

- usage_year:

  Integer vector of years at which to estimate usage.

- usage_day_of_year:

  Integer vector of days of the year at which to estimate usage.

- distribution_year:

  Integer vector of years for distribution time points.

- distribution_day_of_year:

  Integer vector of days of the year for distribution time points.

- net_loss_function:

  Function describing net loss over time. Defaults to
  \[netz::net_loss_map\].

- ...:

  Additional arguments passed to the net_loss_function (e.g.
  \`half_life\`).

## Value

Numeric vector of expected usage values, one per usage time point.

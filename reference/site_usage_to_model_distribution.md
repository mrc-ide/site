# Convert ITN usage to model input distributions

A convenience wrapper around \[netz::usage_to_model_distribution()\]
that accepts year and day-of-year inputs (as found in site files) rather
than pre-computed timesteps.

## Usage

``` r
site_usage_to_model_distribution(
  usage,
  usage_year,
  usage_day_of_year,
  distribution_year,
  distribution_day_of_year,
  distribution_lower = NULL,
  distribution_upper = NULL,
  net_loss_function = netz::net_loss_map,
  ...
)
```

## Arguments

- usage:

  Numeric vector of ITN usage values.

- usage_year:

  Integer vector of years corresponding to \`usage\`.

- usage_day_of_year:

  Integer vector of days of the year corresponding to \`usage\`.

- distribution_year:

  Integer vector of years for distribution time points.

- distribution_day_of_year:

  Integer vector of days of the year for distribution time points.

- distribution_lower:

  Numeric vector of lower bounds for possible distributions. Defaults to
  0 for each distribution time point.

- distribution_upper:

  Numeric vector of upper bounds for possible distributions. Defaults to
  1 for each distribution time point.

- net_loss_function:

  Function describing net loss over time. Defaults to
  \[netz::net_loss_map\].

- ...:

  Additional arguments passed to the net_loss_function (e.g.
  \`half_life\`).

## Value

Numeric vector of model input distribution values, one per distribution
time point.

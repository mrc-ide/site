# Create example ITN intervention data

Creates an ITN (Insecticide Treated Nets) intervention list for testing
with retention parameters, usage data, and multiple distribution events
including mass and routine distributions across different net types.

## Usage

``` r
create_example_itn(itn_use = c(0.1, 0.2, 0.5))
```

## Arguments

- itn_use:

  Numeric vector. ITN use values by year; length should match the years
  used in the example (default: 2000:2002).

## Value

A list containing: - \`retention_half_life\`: Numeric half-life of net
retention - \`use\`: Data frame with name, year, itn_use,
usage_day_of_year - \`implementation\`: Data frame with distribution
details including net_type, distribution_type, distribution bounds, and
distribution_day_of_year

# Prepare continuous intervention coverage data

Prepare continuous intervention coverage data

## Usage

``` r
prepare_continuous_interventions(interventions, years)
```

## Arguments

- interventions:

  A list containing treatment, pmc, vaccine, and lsm sub-lists, each
  with an implementation data frame.

- years:

  Numeric vector of years.

## Value

Data frame with columns: t, element, value.

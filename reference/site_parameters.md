# Create site parameters

Create site parameters

## Usage

``` r
site_parameters(
  interventions,
  demography,
  vectors,
  seasonality,
  min_ages = c(0, 5, 15) * 365,
  species = "pf",
  eir = NULL,
  overrides = list(),
  burnin = 0
)
```

## Arguments

- interventions:

  Site intervention inputs

- demography:

  Site demography inputs

- vectors:

  Site vectors inputs

- seasonality:

  Site seasonality inputs

- min_ages:

  Lower age bands for incidence and N age outputs

- species:

  Can be falciparum: "pf" or vivax: "pv", for vivax SMC, RTSS and PMC
  are not implemented

- eir:

  Site baseline EIR

- overrides:

  List of malariasimulation default parameter overrides

- burnin:

  Number of burn in years

## Value

A malariasimulation parameter list

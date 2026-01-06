# An example country site file

A site file containing all of the input components for running a
setting-specific
[malariasimulation](https://mrc-ide.github.io/malariasimulation/) model
run.

## Usage

``` r
example_site
```

## Format

A list with 10 variables:

- country:

  The country name

- version:

  Site file version

- admin_level:

  The levels of spatial disaggregation

- sites:

  Unique sites

- shape:

  Sptial boundaries (set to NA in testing shapefile)

- cases_deaths:

  Epidemiological site data

- prevalence:

  Prevalence data

- interventions:

  Intervention coverage and specification

- population:

  Population, population at risk mand age-disaggregated population
  projections

- demography:

  Demographic projections

- vectors:

  Vector specieis and pyrethroid resistance

- seasonality:

  Seasonal profile parameters, monthly rainfall and fourier predictions

- blood_disorders:

  Blood disorder data

- accessibility:

  Accessibility data

- eir:

  Calibrated eir

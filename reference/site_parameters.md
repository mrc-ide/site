# Create site parameters

Create site parameters

## Usage

``` r
site_parameters(
  interventions,
  demography,
  vectors,
  seasonality,
  age_group = c(0, 5, 15, 100) * 365,
  clinical_incidence = c(0, 5, 15, 100) * 365,
  severe_incidence = c(0, 5, 15, 100) * 365,
  prevalence = c(2, 10) * 365,
  parasite = "falciparum",
  eir = NULL,
  draw = NULL,
  overrides = list(),
  start_year,
  end_year,
  irs_adjust = 0.75
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

- age_group:

  Age breaks for population size outputs in days; default = c(0, 5,
  15, 100) \* 365

- clinical_incidence:

  Age breaks for clinical incidence outputs in days; default = c(0, 5,
  15, 100) \* 365

- severe_incidence:

  Age breaks for severe incidence outputs in days (pf only); default =
  c(0, 5, 15, 100) \* 365

- prevalence:

  Age breaks for prevalence outputs in days; default = c(2, 10) \* 365

- parasite:

  Can be "falciparum" or "vivax". For vivax, SMC, RTSS and PMC are not
  implemented

- eir:

  Site baseline EIR

- draw:

  malariasimulation parameter draw. Default NULL is best-fit parameter
  set

- overrides:

  List of malariasimulation default parameter overrides

- start_year:

  The year the simulation starts. This should be inclusive of any
  burn-in (which is recommended). For example, if the site file data
  started in year 2000 and I wanted to specify a 20 year burn-in I would
  set start_year to 1980. Interventions prior to the site file start
  year are assumed to be 0 coverage, and demography set to the same
  profile as the first site file entry.

- end_year:

  The end year (inclusive) of the simulation

- irs_adjust:

  A scalar adjustment to IRS coverage inputs to account for observed
  discrepancies between IRS impact in trial versus real-world
  implementation.

## Value

A malariasimulation parameter list

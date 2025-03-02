
<!-- README.md is generated from README.Rmd. Please edit that file -->

# site <img src="man/figures/Site.png" align="right" width=30% height=30% />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/site/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/site/actions)
[![Codecov test
coverage](https://codecov.io/gh/mrc-ide/site/graph/badge.svg)](https://app.codecov.io/gh/mrc-ide/site)
<!-- badges: end -->

The site package supports malariaverse users to:

1.  Access and download the latest country site-files 🌐 ➡️ 💻
2.  Translate site file information into malariasimulation parameters 🌍
    ➡️ 📉

## The site-file

The site-file is the file storing all of the sub nationally
disaggregated information for a country. Components of the site-file
include:

1.  The
    [Metadata](https://mrc-ide.github.io/site/articles/Metadata.html),
    with high level information about the country and site-file version.
2.  [Historical Epidemiological
    Data](https://mrc-ide.github.io/site/articles/historical_epi.html)
    with estimates of cases, deaths and parasite prevalence.
3.  [Population and
    Demography](https://mrc-ide.github.io/site/articles/pop_demog.html)
    information.
4.  Estimates of the [Historical Intervention
    Use](https://mrc-ide.github.io/site/articles/Interventions.html) in
    the country.
5.  [Rainfalll and
    Seasonality](https://mrc-ide.github.io/site/articles/Seasonality.html)
    data.
6.  Information on the [Mosquito
    Vectors](https://mrc-ide.github.io/site/articles/vectors.html)

## Accessing and downloading site-files

Please see [this detailed
guide](https://mrc-ide.github.io/site/articles/Accessing-site-files.html)
for information on how to gain access and download site files.

## Translating site-file information to malariasimulation parameters

If we have access a site file, then all we need to do to run the model
is

1.  Isolate a single site, this is a single sub-national unit within the
    country
2.  Create the parameter list
3.  Pass that to malariasimulation to run the simulation

``` r

# Pull information for a single sub-national unit from the site-file
site <- subset_site(
  site = example_site,
  site_filter = data.frame(
  country = "Burkina Faso",
  iso3c = "BFA",
  name_1 = "Sahel",
  urban_rural = "rural")
  )

# Convert site information to malariasimulation parameters
site_par <- site_parameters(
  interventions = site$interventions,
  demography = site$demography,
  vectors = site$vectors$vector_species,
  seasonality = site$seasonality$seasonality_parameters,
  eir = site$eir$eir,
  overrides = list(
    human_population = 1000
  )
)

# Run the model!
site_sim <- malariasimulation::run_simulation(
  timesteps = site_par$timesteps,
  parameters = site_par
)
```

## Installation

You can install the development version of site from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mrc-ide/site")
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# site <img src="man/figures/Site.png" align="right" width=30% height=30% />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/site/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/site/actions)
<!-- badges: end -->

Site acts as a translational layer between a site file and
malariasimulation: 🌍 ➡️ 📉

The site file is the file storing all of the context specific
information for a site, such as historical intervention coverage,
seasonality, vectors etc. To simulate a given site, we must convert this
information into an input parameter list for malaria simulation. That is
what site is here to do!

If we have a correctly configured site file `example_site`, then all we
need to do is create the parameter list and pass that to
malariasimulation to run:

``` r
site <- subset_site(example_site, example_site$eir[1,])
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
site_sim <- malariasimulation::run_simulation(
  timesteps = site_par$timesteps,
  parameters = site_par
)
```

## Installation

You can install the development version of site from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/site")
```

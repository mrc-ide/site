# Create a site diagnostic report

Assembles a multi-panel diagnostic plot combining map, prevalence,
interventions, vectors, pyrethroid resistance, and age distribution.

## Usage

``` r
plot_site_diagnostic(site, max_year = 2030)
```

## Arguments

- site:

  A site object (subset to a single site).

- max_year:

  Maximum year for resistance and age panels. Default 2030.

## Value

A patchwork object

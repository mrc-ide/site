# Plot intervention coverage over time

Creates a comprehensive visualisation of malaria intervention coverage
overlaid on rainfall seasonality.

## Usage

``` r
plot_interventions(
  interventions,
  seasonality,
  colours = default_intervention_colours(),
  labels = default_intervention_labels(),
  show_legend = TRUE,
  legend_position = "right",
  years = NULL,
  title = NULL
)
```

## Arguments

- interventions:

  A list containing intervention data with sub-elements: itn, smc, irs,
  treatment, pmc, vaccine, lsm.

- seasonality:

  A list with monthly_rainfall and fourier_prediction.

- colours:

  Named character vector of colours. Defaults to
  \[default_intervention_colours()\].

- labels:

  Named character vector of labels. Defaults to
  \[default_intervention_labels()\].

- show_legend:

  Whether to display the legend. Default TRUE.

- legend_position:

  Legend position. Default "right".

- years:

  Optional numeric vector of years. If NULL, inferred from data.

- title:

  Optional plot title.

## Value

A ggplot2 object

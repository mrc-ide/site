# Plot population at risk by age group over time

Stacked bar chart of population at risk (PAR) by age group and year,
with total population outlined on top.

## Usage

``` r
plot_age_distribution_stacked(
  population_by_age,
  breaks = c(0, 1, 5, 15, 30, 50, 70, 100),
  title = NULL
)
```

## Arguments

- population_by_age:

  Data frame with columns: year, age_lower, pop, par

- breaks:

  Age group boundaries.

- title:

  Optional plot title.

## Value

A ggplot2 object

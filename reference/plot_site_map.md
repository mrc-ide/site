# Plot a site's admin unit highlighted on a map

Plot a site's admin unit highlighted on a map

## Usage

``` r
plot_site_map(
  site,
  fill_colour = "#1B2A6C",
  country_fill = "#E2D5C1",
  border_colour = "#444444",
  title = NULL
)
```

## Arguments

- site:

  A site list with \`sites\` and \`shape\` elements.

- fill_colour:

  Fill colour for the focal admin unit.

- country_fill:

  Fill colour for the country background.

- border_colour:

  Colour for admin boundary lines.

- title:

  Optional plot title.

## Value

A ggplot2 object

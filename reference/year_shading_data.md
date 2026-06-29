# Create year shading data

Create year shading data

## Usage

``` r
year_shading_data(years, offset = "continuous", ymin = -Inf, ymax = Inf)
```

## Arguments

- years:

  Numeric vector of years.

- offset:

  "continuous" (xmin=year, xmax=year+1) or "discrete" (xmin=year-0.5,
  xmax=year+0.5).

- ymin, ymax:

  Y-axis bounds for shading rectangles.

## Value

A data frame with xmin, xmax, ymin, ymax columns.

# Prepare rainfall profile data for intervention plot

Prepare rainfall profile data for intervention plot

## Usage

``` r
prepare_rainfall_profile(fourier_prediction, years, max_rainfall)
```

## Arguments

- fourier_prediction:

  Data frame with columns: t, profile.

- years:

  Numeric vector of years.

- max_rainfall:

  Maximum rainfall value for normalisation.

## Value

Data frame with columns: t, value, element.

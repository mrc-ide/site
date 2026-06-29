# Prepare segment-style intervention data

Prepare segment-style intervention data

## Usage

``` r
prepare_segment_intervention(data, day_col, value_col, element)
```

## Arguments

- data:

  Data frame with year and intervention columns.

- day_col:

  Character name of the day-of-year column.

- value_col:

  Character name of the coverage value column.

- element:

  Character element identifier for the plot legend.

## Value

Data frame with columns: t, value, element.

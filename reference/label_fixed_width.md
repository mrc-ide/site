# Fixed-width axis label formatter

Wraps a label function to right-pad its output to a consistent character
width. This prevents plot panels from shifting when label widths vary
across pages in multi-page documents.

## Usage

``` r
label_fixed_width(label_fn, width = 11)
```

## Arguments

- label_fn:

  A labelling function (e.g. \[scales::label_comma()\]).

- width:

  Minimum character width to pad to. Default 11 accommodates
  comma-formatted values up to 99,999,999.

## Value

A labelling function that returns fixed-width character strings.

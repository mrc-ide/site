# Look up fixed colours for vector species

Returns a named character vector of colours for the given species names.
Each species always maps to the same colour regardless of which subset
is plotted, ensuring visual consistency across multi-page reports.

## Usage

``` r
species_colour_lookup(species)
```

## Arguments

- species:

  Character vector of species names (title case).

## Value

Named character vector of hex colours.

# Create example PMC intervention data

Creates a PMC (Perennial Malaria Chemoprevention) intervention list for
testing with drug type, target ages, and coverage implementation over
multiple years.

## Usage

``` r
create_example_pmc()
```

## Value

A list containing: - \`drug\`: Character drug identifier - \`age\`:
Numeric vector of target ages in days - \`implementation\`: Data frame
with name, year, day_of_year, pmc_cov

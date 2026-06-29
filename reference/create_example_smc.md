# Create example SMC intervention data

Creates an SMC (Seasonal Malaria Chemoprevention) intervention list for
testing with drug type and implementation parameters including multiple
rounds per year.

## Usage

``` r
create_example_smc()
```

## Value

A list containing: - \`drug\`: Character drug combination identifier -
\`implementation\`: Data frame with name, year, smc_cov, peak_season,
age limits (smc_min_age, smc_max_age), round, and round_day_of_year

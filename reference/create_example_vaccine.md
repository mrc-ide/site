# Create vaccine intervention examples

Create vaccine intervention examples

## Usage

``` r
create_example_vaccine(
  vaccine = "rtss",
  delivery = "age-based",
  boosters = "none",
  primary_cov = 0.8,
  booster_cov = 0.4,
  years = 2000:2002,
  name = "place",
  peak_season = 100,
  primary_schedule = c(180, 210, 240)
)
```

## Arguments

- vaccine:

  Character. Vaccine type: "rtss", "r21", or "rtss_r21"

- delivery:

  Character. Delivery method: "age-based" or "hybrid"

- boosters:

  Character. Booster schedule: "none", "single", or "multiple"

- primary_cov:

  Numeric. Primary vaccination coverage (0-1). Default: 0.8

- booster_cov:

  Numeric. Booster vaccination coverage (0-1). Default: 0.4

- years:

  Numeric vector. Years of implementation. Default: 2000:2002

- name:

  Character. Location name. Default: "place"

- peak_season:

  Numeric. Peak transmission day of year. Default: 100

- primary_schedule:

  Numeric vector. Primary vaccination ages in days. Default: c(180, 210,
  240)

## Value

A list containing: - \`delivery\`: Character delivery method -
\`primary_schedule\`: Numeric vector of primary schedule ages in days -
\`booster_spacing\`: Numeric vector of booster spacing values -
\`implementation\`: Data frame with coverage and schedule columns

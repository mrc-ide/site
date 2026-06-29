# IRS insecticide efficacy parameters

A dataset containing efficacy parameters for different Indoor Residual
Spraying (IRS) insecticides. These parameters are used by
malariasimulation to model the impact of IRS interventions on mosquito
populations.

## Usage

``` r
irs_efficacy
```

## Format

A data frame with 4 rows and 7 columns:

- insecticide:

  Character. Name of the insecticide (ddt, actellic, bendiocarb,
  sumishield)

- ls_theta:

  Numeric. Theta parameter for mosquito repelling effect

- ls_gamma:

  Numeric. Gamma parameter for mosquito repelling effect

- ks_theta:

  Numeric. Theta parameter for mosquito killing effect

- ks_gamma:

  Numeric. Gamma parameter for mosquito killing effect

- ms_theta:

  Numeric. Theta parameter for successful feeding reduction

- ms_gamma:

  Numeric. Gamma parameter for successful feeding reduction

## Source

Parameterized from field trial data for malariasimulation

# ITN net efficacy parameters

A dataset containing efficacy parameters for Insecticide Treated Nets
(ITNs) based on pyrethroid resistance levels and net type. These
parameters are used by malariasimulation to model the protective effect
of bed nets.

## Usage

``` r
net_efficacy
```

## Format

A data frame with 303 rows and 7 columns:

- pyrethroid_resistance:

  Numeric. Pyrethroid resistance level (0 to 1)

- bioassay_mortality:

  Numeric. Bioassay mortality rate

- dn0:

  Numeric. Initial deterrence parameter

- rn0:

  Numeric. Initial killing parameter

- gamman:

  Numeric. Net half-life parameter (in days)

- rnm:

  Numeric. Minimum killing rate parameter

- net_type:

  Character. Type of net (e.g., pyrethroid_only, pyrethroid_pbo,
  pyrethroid_pyrrole)

## Source

Parameterized from field trial data for malariasimulation

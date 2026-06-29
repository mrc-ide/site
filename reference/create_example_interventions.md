# Create complete set of example interventions

Creates a comprehensive list containing all intervention types
(treatment, ITNs, IRS, SMC, PMC, vaccine, and LSM) using default example
parameters. Useful for testing functions that require a full
intervention specification.

## Usage

``` r
create_example_interventions(itn_use = c(0.1, 0.2, 0.5))
```

## Arguments

- itn_use:

  Numeric vector. ITN use values to pass to \`create_example_itn()\`.
  Default: c(0.1, 0.2, 0.5).

## Value

A named list with elements: treatment, itns, irs, smc, pmc, vaccine,
lsm. Each element contains the output from the corresponding
\`create_example\_\*\` function.

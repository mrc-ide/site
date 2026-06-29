# Historical Epidemiological Data

## Cases and deaths

`site_file$cases_deaths`

World Malaria Report cases and deaths estimates.

### Variables

`wmr_cases_l` WMR case estimate, lower bound  
`wmr_cases` WMR case point estimate  
`wmr_cases_u` WMR case estimate, upper bound  
`wmr_deaths_l` WMR death estimate, lower bound  
`wmr_deaths` WMR death point estimate  
`wmr_deaths_u` WMR death estimate, upper bound  
`wmr_par` WMR population at risk estimates  
`wmr_incidence_l` WMR case incidence estimate, lower bound  
`wmr_incidence` WMR case incidence point estimate  
`wmr_incidence_u` WMR case incidence estimate, upper bound  
`wmr_mortality_l` WMR malaria mortality rate estimate, lower bound  
`wmr_mortality` WMR malaria mortality rate point estimate  
`wmr_mortality_u` WMR malaria mortality rate estimate, upper bound

### Description

Time series take all information from the most recent WMR. Older entries
are from previous editions of the WMR.

### Sources

[⚕️ \|
WHO](https://mrc-ide.github.io/site/articles/data-sources.html#WHO)

## Prevalence

`site_file$prevalence`

Plasmodium falciparum and Plasmodium vivax prevalence estimates

### Variables

`pfpr` Plasmodium falciparum parasite prevalence in 2-10 year olds  
`pvpr` Plasmodium vivax parasite prevalence in 1-99 year olds

### Description

Pixel-level estimates of prevalence indicators are aggregated at the
site-level by taking the population at risk weighted average.

Prevalence estimates are used for calibrating the baseline EIR using the
[cali package](https://mrc-ide.github.io/cali/).

### Sources

[🌍 \|
MAP](https://mrc-ide.github.io/site/articles/data-sources.html#MAP)

## Entomological inoculation rate (EIR)

`site_file$eir`

The calibrated baseline entomological inoculation rate (EIR) for each
site.

### Variables

`sp` Malaria species the EIR applies to (`pf` = *Plasmodium falciparum*,
`pv` = *Plasmodium vivax*)  
`eir` Calibrated baseline annual entomological inoculation rate
(infectious bites per person per year)

### Description

The baseline EIR is the key parameter setting transmission intensity in
the model. It is not observed directly: for each site (and species) it
is calibrated so that the simulated parasite prevalence matches the MAP
prevalence estimates (2010–2024) using the [cali
package](https://mrc-ide.github.io/cali/).

### Sources

[🌍 \|
MAP](https://mrc-ide.github.io/site/articles/data-sources.html#MAP)

## Bias correction

`site_file$bias_correction`

Annual factors used to reconcile modelled case and death counts with the
WHO World Malaria Report estimates.

### Variables

`year` Year  
`case_bias_correction` Multiplicative correction applied to modelled
cases  
`death_bias_correction` Multiplicative correction applied to modelled
deaths

### Description

Modelled clinical case and death counts do not exactly reproduce the
country-level totals reported in the World Malaria Report. These annual
factors rescale the modelled output so that it aligns with the reported
WHO burden.

### Sources

[⚕️ \|
WHO](https://mrc-ide.github.io/site/articles/data-sources.html#WHO)

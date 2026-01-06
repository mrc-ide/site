# Historical Epidemiological Data

## Cases and deaths

`site_file$cases_deaths`

Where available this includes the time series of cases and deaths for
the country as reported in previous editions of the world malaria
report. Other useful data include the world malaria
report^([1](#ref-WMR)) population at risk and uncertainty intervals for
estimates.

## Prevalence

`site_file$prevalence`

Where available this includes the population at risk weighted average
parasite prevalence time series from the Malaria Atlas
Project^([2](#ref-MAP))

*Plasmodium falciparuim* parasite prevalence (`PfPr`) is reported for
children aged between 2-10 years, summarized from the malaria atlas
project raster entitled: “Malaria\_\_202406_Global_Pf_Parasite_Rate”.

*Plasmodium vivax* parasite prevalence (`PvPr`) is reported for
individuals aged between 1-99 years, summarized from the malaria atlas
project raster entitled: “Malaria\_\_202406_Global_Pv_Parasite_Rate”.

Prevalence estimates are used for calibrating the baseline EIR using the
[cali package](https://mrc-ide.github.io/cali/).

## Citations

1\.

World Health Organization. *World malaria report*. (2024).

2\.

[Malaria atlas project](https://malariaatlas.org/).

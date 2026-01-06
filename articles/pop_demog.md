# Population and Demography

### Population projections

`site_file$population`

Site-level population estimates are extracted at the pixel level from
WorldPop^([1](#ref-worldpop)). We use the “Unconstrained individual
countries 2000-2020 UN adjusted ( 1km resolution )” rasters.

Options disaggregated by 1-year age bands
(`site_file$population$population_by_age`) or total
(`site_file$population$population_total`) are provided

#### Urban rural

Urban rural splits within administrative unit are defined using a
threshold density of 1500 people per square km.

#### Population at risk

`site_file$population$par_pf` `site_file$population$par_pv`
`site_file$population$par`

Population at risk from *Plasmodium falciparum*, *Plasmodium vivax* or
both are estimated by masking the total population raster by areas with
active transmission (prevalence \>0%) in year 2000^([2](#ref-MAP)).

#### Population growth and urbanisation

We provide annual population projections to 2050 using UN data on total
population growth^([3](#ref-UNWPP)) combined with UN projections of the
levels of urbanisation^([4](#ref-UNWUP)). We use these projections to
produces estimates of the rate of urban and rural growth (relative to a
baseline year) which are then applied to our spatial-unit population
estimates. Final estimates are rescaled to ensure that population total
match UN totals.

### Demography

Population demographics (age structure) are defined over time for each
site. Demography is obtained using the [peeps R
package](https://github.com/mrc-ide/peeps), and are based on data from
the UN WPP^([3](#ref-UNWPP))

Mortality rates are specified for neonates (0-30 days), young infants
(31 days - 1 year), older infants (1 year - 5 years) and then in five
year age bands.

## Citations

1\.

[WorldPop](https://www.worldpop.org/).

2\.

[Malaria atlas project](https://malariaatlas.org/).

3\.

United Nations. [World population
prospects](https://population.un.org/wpp/).

4\.

United Nations. [World urbanization
prospects](https://population.un.org/wup/).

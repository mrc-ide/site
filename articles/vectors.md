# Mosquito Vectors

## Vector species

`site_file$vectors$vector_species`

Vector species

### Variables

`species` Species name  
`prop` Proportion of total vector population  
`blood_meal_rates` Rate of bloodmeal seeking  
`foraging_time` Time spent taking blood meals  
`Q0` Proportion of blood meals taken on humans  
`phi_bednets` Proportion of bites taken from humans in bed  
`phi_indoors` Proportion of bites taken from humans indoors  
`mum` Mortality rate of adult female mosquitoes

### Description

***Within sub-Saharan Africa:*** For countries in sub-Saharan Africa a
statistical model of relative abundance of major vectors has been
produced by Sinka *et al*. For these countries we extract the relative
abundance of arabiensis, funestus and gambiae using the malaria atlas
project “Anopheles arabiensis Patton, 1905”, “Anopheles funestus” and
“Anopheles gambiae Giles, 1902” rasters. Proportions are normalised to
sum to one (as other species may be present in the data).

***Outside of sub-Saharan Africa:*** For countries outside of
sub-Saharan Africa we do not have estimates of the relative abundance of
vector species. There are however probability of occurrence maps for a
large number of species Sinka et al. For these locations we select up to
a maximum of three species ranked by their average probability of
occurrence across a site. Each species is then given an equally weighted
relative abundance.

The bionomics parameters for vectors in sub-Saharan Africa are
parameterised using the fitted parameters in
[malariasimulation](https://mrc-ide.github.io/malariasimulation/). All
other vectors are parameterised from an (unpublished) literature review
by Arran Hamlet. Any unknown species are assigned the median vector
bionomics parameters across other species at the site. Please contact
Pete Winskill directly if using these estimates.

The blood_meal_rates, foraging_time and mum parameters are assumed fixed
at previously fitted values.

### Sources

[🦟 \|
Vectors](https://mrc-ide.github.io/site/articles/data-sources.html#vectors)

## Pyrethroid resistance

`site_file$vectors$pyrethroid_resistance`

Pyrethroid resistance

### Variables

`year` Year  
`pyrethroid_resistance` Pyrethroid resistance

For each site we include an estimated level of pyrethroid insecticide
resistance. This has been estimated by Tom Churcher and colleagues using
spatio-temporally distributed bioassay mortality data. This work is not
yet published. Therefore for attribution/citation and further
information on the methods used please contact Pete Winskill or Tom
Churcher.

[🦟 \|
Vectors](https://mrc-ide.github.io/site/articles/data-sources.html#vectors)

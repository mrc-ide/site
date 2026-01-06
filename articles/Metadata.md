# Metadata

## Country

`site_file$metadata$country`

Country name

`site_file$metadata$iso3c`

The ISO3c code identifying the country. The R package
`countrycode`^([1](#ref-countrycode)) is useful for converting between
ISO3c and country names.

## Boundary

`site_file$metadata$boundary`

The boundary file version used to create sites

## Resolution

`site_file$metadata$admin_level`

The administrative levels (e.g. state, region, province) of the sites
within the site file.

## Version

`site_file$metadata$version`

Site file version name. Please see the (change
log)\[<https://mrc-ide.github.io/site/news/index.html>\] for version
information.

## Sites

`site_file$sites`

The sites within the site file. These are the named sites disaggregated
to the specified administrative level. Further disaggregation may
include, for example, an [urban rural
split](https://mrc-ide.github.io/site/articles/pop_demog.html). We use
the GADM^([2](#ref-GADM)) version 4.1.0 administrative boundary simple
feature spatial files at the first or second administrative unit level.

## Citations

1\.

Arel-Bundock, V., Enevoldsen, N. & Yetman, C. [Countrycode: An r package
to convert country names and country
codes](https://doi.org/10.21105/joss.00848). *Journal of Open Source
Software* **3**, 848 (2018).

2\.

[GADM maps and data](https://gadm.org/).

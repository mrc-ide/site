# Metadata

Metadata holds high-level information about the country and the
site-file version. The `country`, `boundary` and `admin_level` fields
are available both at the top level of the site file
(e.g. `site_file$country`) and, mirrored, within `site_file$metadata`.

## Country

`site_file$metadata$country`

Country name

`site_file$metadata$iso3c`

The ISO3c code identifying the country. The R package
[countrycode](https://cran.r-project.org/web/packages/countrycode/index.html)
is useful for converting between ISO3c and country names.

## Boundary

`site_file$metadata$boundary`

The boundary file version used to create sites

## Resolution

`site_file$metadata$admin_level`

The administrative levels (e.g. state, region, province) of the sites
within the site file.

## Version

`site_file$metadata$version`

Site file version name. Please see the [change
log](https://mrc-ide.github.io/site/news/index.html) for version
information.

## Sites

`site_file$sites`

The sites within the site file. These are the named sites disaggregated
to the specified administrative level. Further disaggregation may
include, for example, an [urban rural
split](https://mrc-ide.github.io/site/articles/pop_demog.html). We use
the GADM administrative boundary simple feature spatial files at the
first or second administrative unit level.

### Sources

[📍 \|
GADM](https://mrc-ide.github.io/site/articles/data-sources.html#GADM)

## Shape

`site_file$shape`

The spatial boundary geometries for the site, stored as `sf`
simple-feature data frames (the geometry is held in the `geom` column).
The list contains one entry per administrative level present in the
file, for example `level_0` (the national outline) and `level_1` (the
first sub-national level). These are the GADM administrative boundaries
— version **4.1.0**, as recorded in `site_file$metadata$boundary`
(`GADM_4.1.0`) — used to define the sites and to draw maps such as those
produced by [`plot_site_map()`](../reference/plot_site_map.md).

### Sources

[📍 \|
GADM](https://mrc-ide.github.io/site/articles/data-sources.html#GADM)

## Common columns

Most of the data frames in a site file are keyed by a set of
administrative columns identifying the spatial unit each row refers to.
This set is **not fixed** — it depends on the resolution of the site
file (see `site_file$metadata$admin_level`):

- A country-level file may carry only `country` and `iso3c`.
- Finer files add successive sub-national levels: `name_1`, and possibly
  `name_2`, `name_3`, and so on.
- `urban_rural` is present only when an urban/rural split has been
  applied.

For brevity these key columns are not repeated in the per-table variable
lists elsewhere in this documentation.

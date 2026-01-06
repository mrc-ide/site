# Fetch a site file for a country from the malariaverse sitefile server.

The site file is identified by its country code, and optionally the
admin_level, urban/rural setting and version of the site files. The
latest packet from the server matching these parameters is used. By
default, calling \`fetch_site()\` with only \`iso3c\` downloads the
latest available version. For reproducibility and stable workflows, it
is strongly recommended to specify \`version\`, \`admin_level\`, and
\`urban_rural\` explicitly, or to download and store site files locally.
Otherwise, downstream pipelines may break when new site file versions
are released.

## Usage

``` r
fetch_site(
  iso3c = NULL,
  version = NULL,
  admin_level = NULL,
  urban_rural = NULL,
  id = NULL
)
```

## Arguments

- iso3c:

  the ISO country code, a scalar character.

- version:

  the dataset version, a scalar character.

- admin_level:

  a scalar number.

- urban_rural:

  a scalar logical.

- id:

  a packet ID used to select an exact packet.

## Value

The contents of the site file.

## Details

Alternatively, a packet ID can be specified in order to pick an exact
file set.

## Examples

``` r
if (FALSE) { # \dontrun{
fetch_site("NGA")
fetch_site("NGA", admin_level = 1)
fetch_site(id = "20240801-062621-6f95851a")
} # }
```

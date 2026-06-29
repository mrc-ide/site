# Extract a subset from a country site file

Extract a subset from a country site file

## Usage

``` r
subset_site(
  site,
  site_filter,
  skip = c("country", "version", "admin_level", "sites")
)
```

## Arguments

- site:

  Country site file

- site_filter:

  data frame to filter site file elements by. Filtering will be
  conducted on any matched columns for each element.

- skip:

  Character vector of field names to preserve without filtering. Default
  is c("country", "version", "admin_level", "sites").

## Value

Filtered site file

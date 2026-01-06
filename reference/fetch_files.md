# Get files from the malariaverse sitefile server.

Get files from the malariaverse sitefile server.

## Usage

``` r
fetch_files(name, parameters, dest, files, expr = NULL)
```

## Arguments

- name:

  The name of the orderly report.

- parameters:

  A named list of parameters to use when searching for the orderly
  packet. If a query expression \`expr\` is specified, these parameters
  are substituted into the query using the this: prefix. If no
  expression is specified, the latest packet matching these parameters
  exactly is selected.

- dest:

  A directory into which the files should be copied.

- files:

  An optionally-named character vector of files to copy from the packet
  and into the destination directory. If the vector is named, these
  names are used as the destination file path.

- expr:

  The query expression to filter packets. This may be an arbitrary
  orderly query, including a literal packet ID. If absent or NULL, the
  specified list of parameters is used and matched exactly.

## Value

the id of the orderly packet the files were copied from.

LOCATION_NAME <- "malariaverse-sitefiles"

location_configuration <- function() {
  token <- Sys.getenv("GITHUB_TOKEN")
  if (token == "") {
    token <- NULL
  }

  getOption("site.orderly_location", list(
    type = "packit",
    args = list(
      url = "https://malariaverse-sitefiles.packit.dide.ic.ac.uk",
      token = token)
  ))
}

#' Add or update an Orderly location.
#'
#' If a location with the given name already exists and its configuration does
#' not match the given parameters, it is removed first before being added with
#' the new parameters. If it exists and has the same parameters already nothing
#' happens.
#'
#' This functionality could probably be moved to the orderly2 package.
#' @noRd
location_add_or_update <- function(name, type, args, root) {
  locations <- orderly2::orderly_location_list(root = root, verbose = TRUE)
  locations <- locations[locations$name == name,]

  if (nrow(locations) == 0) {
    orderly2::orderly_location_add(name, type, args, root = root)
  } else if (locations[[1, "type"]] != type ||
             !identical(locations[[1, "args"]], args)) {
    orderly2::orderly_location_remove(name, root = root)
    orderly2::orderly_location_add(name, type, args, root = root)
  }
}


#' Configure the orderly root used to fetch sitefiles.
#'
#' This creates a folder in the user's home directory used to download and cache
#' site files. The location of the cache folder is determined by
#' [rappdirs::user_cache_dir()] and depends on the OS.
#'
#' A remote location from which the sitefiles will be fetched is configured on
#' the root. By default this is the malariaverse Packit instance hosted at
#' `https://packit.dide.ic.ac.uk/malariaverse-sitefiles`. This can be customized
#' by setting the `site.orderly_location` option.
#'
#' Users shouldn't need to call this function, as it is called implicitly by
#' [fetch_files] already.
#'
#' @return the path to the orderly root.
#' @noRd
configure_orderly <- function() {
  root <- file.path(rappdirs::user_cache_dir("malariaverse-sitefiles"), "store")

  orderly2::orderly_init(root, use_file_store = TRUE)

  cfg <- location_configuration()
  location_add_or_update(LOCATION_NAME, type = cfg$type, args = cfg$args,
                         root = root)

  root
}


#' Get files from the malariaverse sitefile server.
#'
#' @param name The name of the orderly report.
#' @param parameters A named list of parameters to use when searching for the
#'   orderly packet. If a query expression `expr` is specified, these parameters
#'   are substituted into the query using the this: prefix. If no expression is
#'   specified, the latest packet matching these parameters exactly is selected.
#' @param dest A directory into which the files should be copied.
#' @param files An optionally-named character vector of files to copy from the
#'   packet and into the destination directory. If the vector is named, these
#'   names are used as the destination file path.
#' @param expr The query expression to filter packets. This may be an arbitrary
#'   orderly query, including a literal packet ID. If absent or NULL, the
#'   specified list of parameters is used and matched exactly.
#' @return the id of the orderly packet the files were copied from.
fetch_files <- function(name, parameters, dest, files, expr = NULL) {
  root <- configure_orderly()

  if (is.null(expr)) {
    filter <- paste(sprintf("parameter:%1$s == this:%1$s", names(parameters)),
                    collapse = " && ")
    expr <- sprintf("latest(%s)", filter)
  }

  plan <- orderly2::orderly_copy_files(
    name = name,
    expr = expr,
    parameters = parameters,
    dest = dest,
    files = files,
    location = LOCATION_NAME,
    allow_remote = TRUE,
    fetch_metadata = TRUE,
    root = root
  )

  plan$id
}

#' Fetch a site file for a country from the malariaverse sitefile server.
#'
#' The site file is identified by its country code, and optionally the
#' admin_level, urban/rural setting and version of the site files. The latest
#' packet from the server matching these parameters is used.
#' By default, calling `fetch_site()` with only `iso3c` downloads the latest
#' available version. For reproducibility and stable workflows, it is strongly
#' recommended to specify `version`, `admin_level`, and `urban_rural` explicitly,
#' or to download and store site files locally. Otherwise, downstream pipelines
#' may break when new site file versions are released.
#'
#' Alternatively, a packet ID can be specified in order to pick an exact file
#' set.
#'
#' @param iso3c the ISO country code, a scalar character.
#' @param version the dataset version, a scalar character.
#' @param admin_level a scalar number.
#' @param urban_rural a scalar logical.
#' @param id a packet ID used to select an exact packet.
#' @return The contents of the site file.
#' @examples
#' \dontrun{
#' fetch_site("NGA")
#' fetch_site("NGA", admin_level = 1)
#' fetch_site(id = "20240801-062621-6f95851a")
#' }
#' @export
fetch_site <- function(iso3c = NULL, version = NULL,
                       admin_level = NULL, urban_rural = NULL,
                       id = NULL)
{
  dest <- withr::local_tempdir()
  if (!xor(is.null(iso3c), is.null(id))) {
    rlang::abort("Exactly one of `iso3c` and `id` must be supplied")
  }

  if (!is.null(iso3c)) {
    parameters <- list(
      iso3c = iso3c,
      version = version,
      admin_level = admin_level,
      urban_rural = urban_rural)
    parameters <- parameters[!sapply(parameters, is.null)]
    expr <- NULL
  } else {
    parameters <- list()
    expr <- id
  }

  fetch_files(
    name = "calibration",
    files = "calibrated_scaled_site.rds",
    expr = expr,
    parameters = parameters,
    dest = dest
  )

  readRDS(file.path(dest, "calibrated_scaled_site.rds"))
}

#' View available sites and versions on the server
#'
#' Queries the server for available site files. This could be of use if searching
#' for older site file versions.
#' @return Version table.
#' @export
available_sites <- function(){
  pulled <- orderly2::orderly_metadata_extract(
    name = "calibration",
    extract = c(
      version = "parameters.version",
      iso3c = "parameters.iso3c",
      admin_level = "parameters.admin_level",
      urban_rural = "parameters.urban_rural",
      boundary = "parameters.boundary"
    ),
    allow_remote = TRUE, fetch_metadata = TRUE, root = configure_orderly()
  )

  pulled[,c("version", "iso3c", "admin_level", "urban_rural", "boundary", "id", "local")]
}

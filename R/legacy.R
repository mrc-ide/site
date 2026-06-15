#' Detect a legacy (pre-update) site-file structure
#'
#' The structure of site files changed in a major malariaverse update: elements
#' that used to be flat data frames (`interventions`, `vectors`, `seasonality`)
#' are now nested lists. A data.frame is also a list in R, so the type is tested
#' with [is.data.frame()]: the legacy form is always a data.frame and the current
#' form never is.
#'
#' @param interventions Site intervention inputs.
#' @param vectors Site vectors inputs.
#' @param seasonality Site seasonality inputs.
#'
#' @return `TRUE` if any supplied element looks like the legacy flat structure.
#' @noRd
is_legacy_site_structure <- function(
  interventions = NULL,
  vectors = NULL,
  seasonality = NULL
) {
  any(c(
    is.data.frame(interventions),
    is.data.frame(vectors),
    is.data.frame(seasonality)
  ))
}

#' Abort if a legacy site-file structure is detected
#'
#' Inspects the supplied site-file elements and, if they match the old flat
#' structure, stops with an informative message pointing to the upgrade guide.
#'
#' @inheritParams is_legacy_site_structure
#' @param call The calling environment, for error reporting.
#'
#' @return Invisibly `NULL`; called for its side effect of aborting.
#' @noRd
check_site_structure <- function(
  interventions = NULL,
  vectors = NULL,
  seasonality = NULL,
  call = rlang::caller_env()
) {
  if (is_legacy_site_structure(interventions, vectors, seasonality)) {
    cli::cli_abort(
      c(
        "This looks like an {.strong older site-file structure}.",
        "i" = "The structure of site files has changed: {.code interventions},
               {.code vectors} and {.code seasonality} are now nested lists
               rather than flat data frames.",
        "i" = "See the upgrade guide:
               {.url https://mrc-ide.github.io/site/articles/Upcoming-changes.html}",
        "i" = "To reproduce older analyses, pair older site files with the frozen
               package snapshot {.code malariaverse_01_2025}."
      ),
      call = call
    )
  }
  invisible(NULL)
}

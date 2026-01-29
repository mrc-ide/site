#' Create a data source badge
#'
#' @param label Text to display in the badge
#' @param colour Background color (hex code or CSS color name)
#' @param link URL or anchor link (e.g., "https://mrc-ide.github.io/site/articles/data-sources.html#un")
#'
#' @return HTML string for the badge
#' @export
badge <- function(label, colour, link) {
  hex <- gsub("#", "", colour)
  r <- strtoi(substr(hex, 1, 2), 16L)
  g <- strtoi(substr(hex, 3, 4), 16L)
  b <- strtoi(substr(hex, 5, 6), 16L)
  bg_colour <- sprintf("rgba(%d, %d, %d, 0.75)", r, g, b)

  badge_html <- sprintf(
    '<a href="%s" class="data-badge" style="background-color: %s;">%s</a>',
    link,
    bg_colour,
    label
  )
  knitr::asis_output(badge_html)
}

#' UN badge
#' @export
un_badge <- function() {
  badge(
    "🌐 | UN",
    "#5B92E5",
    "https://mrc-ide.github.io/site/articles/data-sources.html#UN"
  )
}

#' WorldPop badge
#' @export
worldpop_badge <- function() {
  badge(
    "👥 | WorldPop",
    "#1E4A8B",
    "https://mrc-ide.github.io/site/articles/data-sources.html#WorldPop"
  )
}

#' MAP badge
#' @export
map_badge <- function() {
  badge(
    "🌍 | MAP",
    "#EBBC40",
    "https://mrc-ide.github.io/site/articles/data-sources.html#MAP"
  )
}

#' CHIRPS badge
#' @export
chirps_badge <- function() {
  badge(
    "☔ | CHIRPS",
    "#407E1B",
    "https://mrc-ide.github.io/site/articles/data-sources.html#CHIRPS"
  )
}

#' WHO badge
#' @export
who_badge <- function() {
  badge(
    "⚕️ | WHO",
    "#ACCFDF",
    "https://mrc-ide.github.io/site/articles/data-sources.html#WHO"
  )
}

#' Vectors badge
#' @export
vectors_badge <- function() {
  badge(
    "🦟 | Vectors",
    "#dbc8adff",
    "https://mrc-ide.github.io/site/articles/data-sources.html#vectors"
  )
}

#' GADM badge
#' @export
gadm_badge <- function() {
  badge(
    "📍 | GADM",
    "#FFAD00",
    "https://mrc-ide.github.io/site/articles/data-sources.html#GADM"
  )
}

#' DHS badge
#' @export
dhs_badge <- function() {
  badge(
    "📋 | DHS",
    "#C606B4",
    "https://mrc-ide.github.io/site/articles/data-sources.html#DHS"
  )
}

#' SMC badge
#' @export
smc_badge <- function() {
  badge(
    "💊 | SMC",
    "#385B1F",
    "https://mrc-ide.github.io/site/articles/data-sources.html#SMC"
  )
}

#' UNICEF badge
#' @export
unicef_badge <- function() {
  badge(
    "🚸 | UNICEF",
    "#4DC7F4",
    "https://mrc-ide.github.io/site/articles/data-sources.html#UNICEF"
  )
}

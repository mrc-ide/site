#' Create a data source badge
#'
#' @param label Text to display in the badge
#' @param colour Background color (hex code or CSS color name)
#' @param link URL or anchor link (e.g., "data-sources.html#un")
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
  badge("🌐 | UN", "#5B92E5", "data-sources.html#UN")
}

#' WorldPop badge
#' @export
worldpop_badge <- function() {
  badge("👥 | WorldPop", "#1E4A8B", "data-sources.html#WorldPop")
}

#' MAP badge
#' @export
map_badge <- function() {
  badge("🌍 | MAP", "#EBBC40", "data-sources.html#MAP")
}

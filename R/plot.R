#' Site diagnostic plot theme
#'
#' @description
#' A consistent ggplot2 theme for all diagnostic plots. Based on theme_bw()
#' with navy blue axis text, no minor gridlines, no major x gridlines,
#' rotated x-axis labels, and a default 1/3 aspect ratio.
#'
#' @param aspect.ratio Aspect ratio for the plot panel. Default 1/3.
#' @return A ggplot2 theme
#' @export
theme_site <- function(aspect.ratio = 1 / 3) {
  navy <- "#1B2A6C"

  ggplot2::theme_bw() +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1,
        colour = navy
      ),
      axis.text.y = ggplot2::element_text(colour = navy),
      axis.title = ggplot2::element_text(colour = navy),
      plot.title = ggplot2::element_text(
        colour = navy,
        face = "bold",
        size = 12
      ),
      aspect.ratio = aspect.ratio
    )
}

#' Age group colour palette
#'
#' @description
#' A continuous-style gradient palette for ordered age groups.
#' Interpolates from teal through gold to coral.
#'
#' @param n Number of colours needed. Default 7.
#' @return A character vector of hex colours
#' @export
site_age_palette <- function(n = 7) {
  grDevices::colorRampPalette(c("#2A9D8F", "#E9C46A", "#E76F51"))(n)
}

#' Vector species colour palette
#'
#' @description
#' Distinct colour palette for vector species (categorical, not ordered).
#'
#' @param n Number of colours needed.
#' @return A character vector of hex colours
#' @export
site_vector_palette <- function(n) {
  colours <- c(
    "#2A9D8F",
    "#E76F51",
    "#1B2A6C",
    "#E9C46A",
    "#D81B60",
    "#6A4C93",
    "#1982C4",
    "#FF595E"
  )
  colours[seq_len(n)]
}

#' Plot site prevalence over time
#'
#' @param prevalence Data frame with columns: year, pfpr, pvpr
#' @param title Optional plot title.
#' @return A ggplot2 object
#' @export
plot_site_prevalence <- function(prevalence, title = NULL) {
  plot_data <- prevalence |>
    tidyr::pivot_longer(
      cols = c("pfpr", "pvpr"),
      names_to = "Species",
      values_to = "Prevalence"
    )

  years <- sort(unique(prevalence$year))
  year_shading <- year_shading_data(years)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = year + 0.5, y = .data$Prevalence, colour = .data$Species)
  ) +
    year_shading_layer(year_shading) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(
      values = c(pfpr = "#D81B60", pvpr = "#1E88E5"),
      labels = c(
        pfpr = expression(italic(Pf) * PR[2 - 10]),
        pvpr = expression(italic(Pv) * PR[1 - 100])
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = years + 0.5, labels = years) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0.02)) +
    ggplot2::labs(x = "Year", y = "Prevalence", title = title) +
    theme_site()
}

#' Plot vector species characteristics as faceted horizontal bars
#'
#' @param vector_species Data frame with columns: species, prop, Q0,
#'   phi_indoors, phi_bednets
#' @param title Optional plot title.
#' @return A ggplot2 object
#' @export
plot_vector_species <- function(vector_species, title = NULL) {
  vs <- vector_species
  vs$species <- tools::toTitleCase(vs$species)
  vs$species <- factor(vs$species, levels = vs$species[order(vs$prop)])

  vs_long <- vs |>
    dplyr::select("species", "prop", "Q0", "phi_indoors", "phi_bednets") |>
    tidyr::pivot_longer(
      cols = c("prop", "Q0", "phi_indoors", "phi_bednets"),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      metric = factor(
        .data$metric,
        levels = c("prop", "Q0", "phi_indoors", "phi_bednets"),
        labels = c(
          "Proportion",
          "Anthropophagy (HBI)",
          "Bites indoors",
          "Bites at night"
        )
      )
    )

  species_colours <- stats::setNames(
    site_vector_palette(nrow(vs)),
    levels(vs$species)
  )

  ggplot2::ggplot(
    vs_long,
    ggplot2::aes(x = .data$value, y = .data$species, fill = .data$species)
  ) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~metric, ncol = 2) +
    ggplot2::scale_fill_manual(values = species_colours) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, title = title) +
    theme_site(aspect.ratio = NULL) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 7),
      axis.text.y = ggplot2::element_text(face = "italic"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Plot pyrethroid resistance over time
#'
#' @param pyrethroid_resistance Data frame with columns: year,
#'   pyrethroid_resistance (0-1)
#' @param title Optional plot title.
#' @return A ggplot2 object
#' @export
plot_pyrethroid_resistance <- function(pyrethroid_resistance, title = NULL) {
  years <- sort(unique(pyrethroid_resistance$year))
  year_shading <- year_shading_data(years)

  ggplot2::ggplot(
    pyrethroid_resistance,
    ggplot2::aes(x = year + 0.5, y = .data$pyrethroid_resistance)
  ) +
    year_shading_layer(year_shading) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = years + 0.5, labels = years) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0.02)) +
    ggplot2::labs(x = "Year", y = "Pyrethroid resistance", title = title) +
    theme_site()
}

#' Plot population at risk by age group over time
#'
#' @description
#' Stacked bar chart of population at risk (PAR) by age group and year,
#' with total population outlined on top.
#'
#' @param population_by_age Data frame with columns: year, age_lower, pop, par
#' @param breaks Age group boundaries.
#' @param title Optional plot title.
#' @return A ggplot2 object
#' @export
plot_age_distribution_stacked <- function(
  population_by_age,
  breaks = c(0, 1, 5, 15, 30, 50, 70, 100),
  title = NULL
) {
  labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])

  plot_data <- population_by_age |>
    dplyr::mutate(
      age_group = factor(
        cut(
          .data$age_lower,
          breaks = breaks,
          right = FALSE,
          include.lowest = TRUE,
          labels = labels
        ),
        levels = rev(labels)
      )
    ) |>
    dplyr::summarise(
      par = sum(.data$par),
      pop = sum(.data$pop),
      .by = c(year, age_group)
    )

  pop_totals <- plot_data |>
    dplyr::summarise(pop = sum(.data$pop), .by = year)

  years <- sort(unique(plot_data$year))
  max_y <- max(pop_totals$pop)
  year_shading <- year_shading_data(
    years,
    offset = "discrete",
    ymin = 0,
    ymax = max_y
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$year, y = .data$par, fill = .data$age_group)
  ) +
    year_shading_layer(year_shading) +
    ggplot2::geom_col() +
    ggplot2::geom_col(
      data = pop_totals,
      ggplot2::aes(x = .data$year, y = .data$pop),
      inherit.aes = FALSE,
      fill = NA,
      colour = "#333333",
      linewidth = 0.4
    ) +
    ggplot2::scale_fill_manual(
      name = "Age group",
      values = stats::setNames(site_age_palette(length(labels)), rev(labels))
    ) +
    ggplot2::scale_x_continuous(breaks = years) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = "Year", y = "Population", title = title) +
    theme_site()
}

#' Plot a site's admin unit highlighted on a map
#'
#' @param site A site list with `sites` and `shape` elements.
#' @param fill_colour Fill colour for the focal admin unit.
#' @param country_fill Fill colour for the country background.
#' @param border_colour Colour for admin boundary lines.
#' @param title Optional plot title.
#' @return A ggplot2 object
#' @export
plot_site_map <- function(
  site,
  fill_colour = "#1B2A6C",
  country_fill = "#E2D5C1",
  border_colour = "#444444",
  title = NULL
) {
  shape <- lapply(site$shape, sf::st_as_sf)
  n_levels <- length(shape)

  admin <- shape[[1]] |>
    dplyr::semi_join(
      site$sites,
      by = intersect(names(site$sites), names(shape[[1]]))
    )

  max_lw <- 0.8
  min_lw <- 0.15

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = shape[[n_levels]],
      fill = country_fill,
      colour = NA
    )

  for (lvl in seq(from = n_levels, to = 2)) {
    frac <- (lvl - 1) / (n_levels - 1)
    p <- p +
      ggplot2::geom_sf(
        data = shape[[lvl]],
        fill = NA,
        colour = scales::alpha(border_colour, 0.3 + frac * 0.7),
        linewidth = min_lw + frac * (max_lw - min_lw)
      )
  }

  p +
    ggplot2::geom_sf(
      data = admin,
      fill = fill_colour,
      colour = "#333333",
      linewidth = 0.4,
      alpha = 0.85
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        colour = "#1B2A6C",
        face = "bold",
        size = 12
      ),
      plot.margin = ggplot2::margin(5, 5, 5, 5)
    )
}

#' Default colour palette for intervention plot elements
#'
#' @return Named character vector of colours
#' @export
default_intervention_colours <- function() {
  c(
    rainfall_bars = "#A8D4F0",
    rainfall_profile = "#6BB8E0",
    itn_use = "#D81B60",
    itn_model_use = "#FF4081",
    itn_distribution = "#FF80AB",
    smc = "#FFB300",
    irs = "#FF8F00",
    tx_cov = "#00BFA5",
    rtss_cov = "#8E24AA",
    r21_cov = "#BA68C8",
    pmc_cov = "#00ACC1",
    lsm_cov = "#C0CA33"
  )
}

#' Default labels for intervention plot elements
#'
#' @return Named character vector of labels
#' @export
default_intervention_labels <- function() {
  c(
    rainfall_bars = "Rainfall",
    rainfall_profile = "Rainfall profile",
    itn_use = "ITN use (MAP)",
    itn_model_use = "ITN use (modelled)",
    itn_distribution = "ITN model distribution",
    smc = "SMC",
    irs = "IRS",
    tx_cov = "Treatment",
    rtss_cov = "RTS,S vaccine",
    r21_cov = "R21 vaccine",
    pmc_cov = "PMC",
    lsm_cov = "LSM"
  )
}

#' Prepare rainfall bar data for intervention plot
#'
#' @param monthly_rainfall Data frame with columns: year, t, rainfall.
#' @return Data frame with columns: t, value, element.
#' @keywords internal
prepare_rainfall_bars <- function(monthly_rainfall) {
  max_rain <- max(monthly_rainfall$rainfall, na.rm = TRUE)
  monthly_rainfall |>
    dplyr::mutate(
      t = .data$year + (.data$t / 365),
      value = .data$rainfall / max_rain,
      element = "rainfall_bars",
      .keep = "none"
    )
}

#' Prepare rainfall profile data for intervention plot
#'
#' @param fourier_prediction Data frame with columns: t, profile.
#' @param years Numeric vector of years.
#' @param max_rainfall Maximum rainfall value for normalisation.
#' @return Data frame with columns: t, value, element.
#' @keywords internal
prepare_rainfall_profile <- function(fourier_prediction, years, max_rainfall) {
  fourier_prediction |>
    dplyr::select("t", "profile") |>
    tidyr::expand_grid(year = years) |>
    dplyr::mutate(
      t = .data$year + .data$t / 365,
      value = .data$profile / max_rainfall,
      element = "rainfall_profile",
      .keep = "none"
    )
}

#' Prepare ITN data for intervention plot
#'
#' @param itn_implementation Data frame with ITN distribution data including
#'   itn_input_dist and distribution_day_of_year columns.
#' @param itn_use Data frame with ITN usage data including itn_use and
#'   usage_day_of_year columns.
#' @param retention_half_life Numeric net retention half-life in days.
#' @param years Numeric vector of years.
#' @return A list with elements: model_use, itn_use, itn_input_dist.
#' @keywords internal
prepare_itn <- function(
  itn_implementation,
  itn_use,
  retention_half_life,
  years
) {
  if (!"itn_input_dist" %in% colnames(itn_implementation)) {
    stop(
      "Column 'itn_input_dist' not found in ITN implementation data. ",
      "Please run netz::usage_to_model_distribution() first.",
      call. = FALSE
    )
  }

  n_years <- length(years)
  distribution_timesteps <- site::calendar_to_timestep(
    year = itn_implementation$year,
    day_of_year = itn_implementation$distribution_day_of_year,
    start_year = min(years)
  )

  model_use_vector <- netz::model_distribution_to_usage(
    distribution = itn_implementation$itn_input_dist,
    usage_timesteps = seq_len(365 * n_years),
    distribution_timesteps = distribution_timesteps,
    net_loss_function = netz::net_loss_map,
    half_life = retention_half_life
  )

  model_use <- data.frame(
    year = rep(years, each = 365),
    day_of_year = rep(seq_len(365), n_years),
    value = model_use_vector
  ) |>
    dplyr::mutate(
      t = .data$year + .data$day_of_year / 365,
      value = .data$value,
      element = "itn_model_use",
      .keep = "none"
    )

  itn_use_df <- itn_use |>
    dplyr::mutate(
      t = .data$year + .data$usage_day_of_year / 365,
      value = .data$itn_use,
      element = "itn_use",
      .keep = "none"
    )

  itn_dist_df <- itn_implementation |>
    dplyr::mutate(
      t = .data$year + .data$distribution_day_of_year / 365,
      value = .data$itn_input_dist,
      element = "itn_distribution",
      .keep = "none"
    )

  list(
    model_use = model_use,
    itn_use = itn_use_df,
    itn_input_dist = itn_dist_df
  )
}

#' Prepare segment-style intervention data
#'
#' @param data Data frame with year and intervention columns.
#' @param day_col Character name of the day-of-year column.
#' @param value_col Character name of the coverage value column.
#' @param element Character element identifier for the plot legend.
#' @return Data frame with columns: t, value, element.
#' @keywords internal
prepare_segment_intervention <- function(data, day_col, value_col, element) {
  data |>
    dplyr::mutate(
      t = .data$year + .data[[day_col]] / 365,
      value = .data[[value_col]],
      element = element,
      .keep = "none"
    )
}

#' Prepare continuous intervention coverage data
#'
#' @param interventions A list containing treatment, pmc, vaccine, and lsm
#'   sub-lists, each with an implementation data frame.
#' @param years Numeric vector of years.
#' @return Data frame with columns: t, element, value.
#' @keywords internal
prepare_continuous_interventions <- function(interventions, years) {
  time_grid <- data.frame(
    year = rep(years, each = 365),
    day_of_year = rep(seq_len(365), length(years))
  ) |>
    dplyr::mutate(t = .data$year + .data$day_of_year / 365)

  result <- time_grid |>
    dplyr::left_join(
      dplyr::select(interventions$treatment$implementation, "year", "tx_cov"),
      by = "year"
    ) |>
    dplyr::left_join(
      dplyr::select(interventions$pmc$implementation, "year", "pmc_cov"),
      by = "year"
    ) |>
    dplyr::left_join(
      dplyr::select(
        interventions$vaccine$implementation,
        "year",
        dplyr::any_of(c("r21_primary_cov", "rtss_primary_cov"))
      ),
      by = "year"
    ) |>
    dplyr::left_join(
      dplyr::select(interventions$lsm$implementation, "year", "lsm_cov"),
      by = "year"
    )

  cov_cols <- intersect(
    names(result),
    c("tx_cov", "pmc_cov", "r21_primary_cov", "rtss_primary_cov", "lsm_cov")
  )

  rename_map <- c(
    r21_primary_cov = "r21_cov",
    rtss_primary_cov = "rtss_cov"
  )

  result |>
    dplyr::select("t", dplyr::all_of(cov_cols)) |>
    tidyr::pivot_longer(
      cols = -"t",
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::mutate(
      element = dplyr::recode(.data$element, !!!rename_map)
    ) |>
    dplyr::filter(!is.na(.data$value))
}

#' Build legend data from present plot elements
#'
#' @param elements Character vector of element names present in the plot.
#' @param colours Named character vector of colours.
#' @param labels Named character vector of labels.
#' @return Data frame with columns: element, colour, label.
#' @keywords internal
build_legend_data <- function(elements, colours, labels) {
  present <- elements[elements %in% names(colours)]
  data.frame(
    element = present,
    colour = colours[present],
    label = labels[present],
    stringsAsFactors = FALSE
  )
}

#' Plot intervention coverage over time
#'
#' @description
#' Creates a comprehensive visualisation of malaria intervention coverage
#' overlaid on rainfall seasonality.
#'
#' @param interventions A list containing intervention data with sub-elements:
#'   itn, smc, irs, treatment, pmc, vaccine, lsm.
#' @param seasonality A list with monthly_rainfall and fourier_prediction.
#' @param colours Named character vector of colours.
#'   Defaults to [default_intervention_colours()].
#' @param labels Named character vector of labels.
#'   Defaults to [default_intervention_labels()].
#' @param show_legend Whether to display the legend. Default TRUE.
#' @param legend_position Legend position. Default "right".
#' @param years Optional numeric vector of years. If NULL, inferred from data.
#' @param title Optional plot title.
#' @return A ggplot2 object
#' @export
plot_interventions <- function(
  interventions,
  seasonality,
  colours = default_intervention_colours(),
  labels = default_intervention_labels(),
  show_legend = TRUE,
  legend_position = "right",
  years = NULL,
  title = NULL
) {
  if (is.null(years)) {
    all_years <- c(
      interventions$itn$use$year,
      interventions$itn$implementation$year,
      seasonality$monthly_rainfall$year
    )
    years <- seq(min(all_years, na.rm = TRUE), max(all_years, na.rm = TRUE))
  }

  max_rainfall <- max(seasonality$monthly_rainfall$rainfall, na.rm = TRUE)

  itn_data <- prepare_itn(
    interventions$itn$implementation,
    interventions$itn$use,
    interventions$itn$retention_half_life,
    years
  )

  data_list <- list(
    rainfall_bars = prepare_rainfall_bars(seasonality$monthly_rainfall),
    rainfall_profile = prepare_rainfall_profile(
      seasonality$fourier_prediction,
      years,
      max_rainfall
    ),
    itn_use = itn_data$itn_use,
    itn_model_use = itn_data$model_use,
    itn_distribution = itn_data$itn_input_dist,
    smc = prepare_segment_intervention(
      interventions$smc$implementation,
      "round_day_of_year",
      "smc_cov",
      "smc"
    ),
    irs = prepare_segment_intervention(
      interventions$irs$implementation,
      "spray_day_of_year",
      "irs_cov",
      "irs"
    ),
    continuous = prepare_continuous_interventions(interventions, years)
  )

  present_elements <- c(
    "rainfall_bars",
    "rainfall_profile",
    "itn_use",
    "itn_model_use",
    "itn_distribution"
  )

  shading <- year_shading_data(years, ymin = 0, ymax = 1)

  p <- ggplot2::ggplot() +
    year_shading_layer(shading)

  p <- p +
    ggplot2::geom_bar(
      data = data_list$rainfall_bars,
      ggplot2::aes(x = .data$t, y = .data$value),
      fill = colours["rainfall_bars"],
      stat = "identity",
      width = 0.08
    ) +
    ggplot2::geom_line(
      data = data_list$rainfall_profile,
      ggplot2::aes(x = .data$t, y = .data$value),
      colour = colours["rainfall_profile"],
      linewidth = 0.8
    )

  if (any(data_list$smc$value > 0, na.rm = TRUE)) {
    p <- p +
      ggplot2::geom_segment(
        data = dplyr::filter(data_list$smc, .data$value > 0),
        ggplot2::aes(x = .data$t, xend = .data$t, y = 0, yend = .data$value),
        colour = colours["smc"],
        linewidth = 1
      )
    present_elements <- c(present_elements, "smc")
  }

  if (any(data_list$irs$value > 0, na.rm = TRUE)) {
    p <- p +
      ggplot2::geom_segment(
        data = dplyr::filter(data_list$irs, .data$value > 0),
        ggplot2::aes(x = .data$t, xend = .data$t, y = 0, yend = .data$value),
        colour = colours["irs"],
        linewidth = 1
      )
    present_elements <- c(present_elements, "irs")
  }

  p <- p +
    ggplot2::geom_segment(
      data = data_list$itn_distribution,
      ggplot2::aes(x = .data$t, xend = .data$t, y = 0, yend = .data$value),
      colour = colours["itn_distribution"],
      linewidth = 2.5
    ) +
    ggplot2::geom_line(
      data = data_list$itn_model_use,
      ggplot2::aes(x = .data$t, y = .data$value),
      colour = colours["itn_model_use"],
      linewidth = 0.8,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = data_list$itn_use,
      ggplot2::aes(x = .data$t, y = .data$value),
      colour = colours["itn_use"],
      size = 2
    )

  cont_elements <- unique(data_list$continuous$element)
  present_elements <- c(present_elements, cont_elements)
  for (elem in cont_elements) {
    elem_data <- dplyr::filter(data_list$continuous, .data$element == elem)
    p <- p +
      ggplot2::geom_line(
        data = elem_data,
        ggplot2::aes(x = .data$t, y = .data$value),
        colour = colours[elem],
        linewidth = 0.8
      )
  }

  if (show_legend) {
    legend_data <- build_legend_data(present_elements, colours, labels)
    if (nrow(legend_data) > 0) {
      legend_df <- data.frame(
        x = NA_real_,
        y = NA_real_,
        element = factor(legend_data$element, levels = legend_data$element)
      )
      p <- p +
        ggplot2::geom_point(
          data = legend_df,
          ggplot2::aes(x = .data$x, y = .data$y, colour = .data$element),
          size = 0,
          show.legend = TRUE,
          na.rm = TRUE
        ) +
        ggplot2::scale_colour_manual(
          name = NULL,
          values = stats::setNames(legend_data$colour, legend_data$element),
          labels = stats::setNames(legend_data$label, legend_data$element),
          guide = ggplot2::guide_legend(
            override.aes = list(shape = 15, size = 5)
          )
        )
    }
  }

  p +
    ggplot2::labs(
      x = "Year",
      y = "Coverage / Normalised rainfall",
      title = title
    ) +
    ggplot2::scale_x_continuous(breaks = years + 0.5, labels = years) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0.02)) +
    theme_site() +
    ggplot2::theme(legend.position = legend_position)
}

#' Plot interventions from a site object
#'
#' @param site A site object with interventions and seasonality elements.
#' @param ... Additional arguments passed to [plot_interventions()].
#' @return A ggplot2 object
#' @export
plot_site_interventions <- function(site, ...) {
  plot_interventions(
    interventions = site$interventions,
    seasonality = site$seasonality,
    ...
  )
}

#' Create a site diagnostic report
#'
#' @description
#' Assembles a multi-panel diagnostic plot combining map, prevalence,
#' interventions, vectors, pyrethroid resistance, and age distribution.
#'
#' @param site A site object (subset to a single site).
#' @param max_year Maximum year for resistance and age panels. Default 2030.
#' @return A patchwork object
#' @export
plot_site_diagnostic <- function(site, max_year = 2030) {
  map <- plot_site_map(site)
  prev <- plot_site_prevalence(site$prevalence, title = "Prevalence")
  int <- plot_site_interventions(site, title = "Interventions")
  vectors <- plot_vector_species(
    site$vectors$vector_species,
    title = "Vectors"
  )
  resistance <- plot_pyrethroid_resistance(
    dplyr::filter(site$vectors$pyrethroid_resistance, .data$year <= max_year),
    title = "Pyrethroid resistance"
  )
  age <- plot_age_distribution_stacked(
    dplyr::filter(site$population$population_by_age, .data$year <= max_year),
    title = "Age distribution"
  )

  name <- paste(dplyr::select(site$sites[1, ], -"iso3c"), collapse = " | ")

  tr <- (map | (prev / int)) +
    patchwork::plot_layout(widths = c(1, 2))
  br <- ((age / resistance) | vectors)

  (tr / br) +
    patchwork::plot_layout(heights = c(2, 1.25)) +
    patchwork::plot_annotation(
      title = name,
      caption = paste(
        "NOTES",
        "Data duplicated for recent years where there are no inputs.",
        "Monthly historical rainfall data are included, but the site implements a single seasonal profile for all years.",
        "Where urban/rural split is implemented, you may see jumps in population size as urbanisation occurs through time.",
        sep = "\n"
      )
    )
}

#' Create year shading data
#'
#' @param years Numeric vector of years.
#' @param offset "continuous" (xmin=year, xmax=year+1) or "discrete"
#'   (xmin=year-0.5, xmax=year+0.5).
#' @param ymin,ymax Y-axis bounds for shading rectangles.
#' @return A data frame with xmin, xmax, ymin, ymax columns.
#' @keywords internal
year_shading_data <- function(
  years,
  offset = "continuous",
  ymin = -Inf,
  ymax = Inf
) {
  shading_years <- years[seq(1, length(years), by = 2)]
  if (offset == "discrete") {
    data.frame(
      xmin = shading_years - 0.5,
      xmax = shading_years + 0.5,
      ymin = ymin,
      ymax = ymax
    )
  } else {
    data.frame(
      xmin = shading_years,
      xmax = shading_years + 1,
      ymin = ymin,
      ymax = ymax
    )
  }
}

#' Year shading geom_rect layer
#'
#' @param shading_data Data frame from [year_shading_data()].
#' @return A ggplot2 geom_rect layer.
#' @keywords internal
year_shading_layer <- function(shading_data) {
  ggplot2::geom_rect(
    data = shading_data,
    ggplot2::aes(
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax
    ),
    inherit.aes = FALSE,
    fill = "grey93",
    colour = NA
  )
}

# ggplot2-based plotting for interpElections_result objects


#' Plot an interpolated variable as a choropleth map
#'
#' Produces a ggplot2 choropleth map of an interpolated variable.
#' Variables can be referenced by column name, ballot number,
#' candidate name, or party abbreviation.
#'
#' @param x An `interpElections_result` object.
#' @param variable Variable to plot. Accepts a column name (e.g.,
#'   `"CAND_13"`), ballot number (numeric, e.g. `13`), candidate name
#'   substring (e.g., `"Lula"`), or party abbreviation (e.g., `"PT"`).
#'   Multiple values produce a faceted comparison.
#'   If NULL, auto-selects the first candidate variable.
#' @param type Quantity to map: `"pct_tract"` (default), `"absolute"`,
#'   `"pct_muni"`, `"pct_valid"`, `"pct_eligible"`, `"density"`.
#' @param palette Color palette name. RColorBrewer palettes
#'   (e.g., `"RdYlBu"`, `"YlGnBu"`, `"Spectral"`) or viridis palettes
#'   (e.g., `"viridis"`, `"magma"`, `"plasma"`). Default: `"RdYlBu"`
#'   (diverging, colorblind-friendly).
#' @param breaks Scale breaks: `"quantile"` (default), `"continuous"`,
#'   `"jenks"` (requires classInt), or a numeric vector of custom
#'   break points.
#' @param n_breaks Number of breaks for `"quantile"` or `"jenks"`.
#'   Default: 5.
#' @param title Plot title. NULL = auto-generated from dictionary
#'   (title-cased candidate name with party).
#' @param subtitle Plot subtitle. NULL = auto-generated from
#'   municipality/year/quantity metadata.
#' @param legend_title Legend title. NULL = auto-generated from
#'   quantity type.
#' @param caption Plot caption. NULL = auto-generated source note.
#'   Use `""` to suppress.
#' @param show_sources Overlay source points (polling stations)
#'   on the map. Requires `sources_sf` in the result
#'   (use `keep = "sources_sf"` when interpolating). Default: FALSE.
#' @param border_color Tract border color. Default: `"white"`.
#' @param border_width Tract border width. Default: 0.05.
#' @param limits Bounding box for the map extent as
#'   `c(xmin, xmax, ymin, ymax)` in the CRS of the data (typically
#'   longitude/latitude). NULL (default) shows the full extent.
#'   Use this to zoom into a region of interest.
#' @param scale_bar Add a scale bar (requires ggspatial). Default: TRUE.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly). Can be further customized
#'   with ggplot2 `+` syntax.
#'
#' @examples
#' \dontrun{
#' result <- interpolate_election_br(3304557, 2020, 2022)
#'
#' # By candidate name (default: % of tract votes, quantile breaks)
#' plot(result, variable = "Lula")
#'
#' # By ballot number
#' plot(result, variable = 13)
#'
#' # Absolute counts with continuous scale
#' plot(result, variable = "PT", type = "absolute",
#'      breaks = "continuous")
#'
#' # Faceted comparison
#' plot(result, variable = c("Lula", "Bolsonaro"))
#'
#' # Zoom into a region
#' plot(result, variable = "Lula",
#'      limits = c(-43.2, -43.1, -22.95, -22.85))
#'
#' # Composable with ggplot2
#' library(ggplot2)
#' plot(result, variable = "PT") + theme_dark()
#' }
#'
#' @exportS3Method
plot.interpElections_result <- function(
    x, variable = NULL, type = "pct_tract",
    palette = "RdYlBu", breaks = "quantile", n_breaks = 5L,
    title = NULL, subtitle = NULL, legend_title = NULL,
    caption = NULL,
    show_sources = FALSE, border_color = "white", border_width = 0.05,
    limits = NULL, scale_bar = TRUE, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "The 'ggplot2' package is required for plot().\n",
      "Install with: install.packages(\"ggplot2\")",
      call. = FALSE
    )
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for plot().", call. = FALSE)
  }
  if (is.null(x$tracts_sf)) {
    stop("No tracts_sf in result object.", call. = FALSE)
  }

  # Resolve variable(s)
  if (is.null(variable)) variable <- .auto_select_var(x)

  # Multi-variable: faceted plot
  if (length(variable) > 1L) {
    p <- .plot_faceted(x, variable, type, palette, breaks, n_breaks,
                       title, subtitle, legend_title, caption,
                       show_sources, border_color, border_width,
                       limits, scale_bar)
    print(p)
    return(invisible(p))
  }

  col <- tryCatch(.resolve_var(variable, x), error = function(e) {
    message(conditionMessage(e))
    NULL
  })
  if (is.null(col)) return(invisible(NULL))
  values <- .compute_quantity(x, col, type)

  # Build plotting sf
  plot_sf <- x$tracts_sf
  plot_sf$.plot_value <- values

  # Auto-generate labels
  if (is.null(title)) title <- .auto_title(col, x)
  if (is.null(subtitle)) subtitle <- .auto_subtitle(x, type, breaks)
  if (is.null(legend_title)) legend_title <- .quantity_label(type)
  if (is.null(caption)) caption <- .auto_caption()

  # Compute breaks
  brk <- .compute_breaks(values, breaks, n_breaks)

  # Build plot
  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.plot_value),
      color = border_color,
      linewidth = border_width
    ) +
    .build_fill_scale(palette, brk, n_breaks, type = type) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  fill = legend_title, caption = caption) +
    .map_theme()

  # Crop to limits
  if (!is.null(limits)) {
    p <- p + ggplot2::coord_sf(
      xlim = limits[1:2], ylim = limits[3:4], expand = FALSE
    )
  }

  # Scale bar
  if (scale_bar && requireNamespace("ggspatial", quietly = TRUE)) {
    p <- p + ggspatial::annotation_scale(
      location = "bl", width_hint = 0.25,
      line_width = 0.5, height = ggplot2::unit(0.15, "cm"),
      text_cex = 0.6
    )
  }

  # Legend guide for binned scales
  if (!is.null(brk)) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = ggplot2::unit(1.5, "cm"),
      keyheight = ggplot2::unit(0.3, "cm"),
      nrow = 1
    ))
  }

  # Overlay source points if requested
  if (show_sources) {
    if (!is.null(x$sources_sf)) {
      p <- p + ggplot2::geom_sf(
        data = x$sources_sf,
        color = "black", size = 0.5, alpha = 0.6,
        inherit.aes = FALSE
      )
    } else {
      warning(
        "show_sources = TRUE but no sources_sf in result. ",
        "Re-run with keep = \"sources_sf\".",
        call. = FALSE
      )
    }
  }

  print(p)
  invisible(p)
}


#' @rdname plot.interpElections_result
#' @param object An `interpElections_result` object.
#' @exportS3Method ggplot2::autoplot
autoplot.interpElections_result <- function(object, ...) {
  plot.interpElections_result(object, ...)
}


# --- Faceted multi-variable plot ---

#' @noRd
.plot_faceted <- function(
    x, variables, type, palette, breaks, n_breaks,
    title, subtitle, legend_title, caption,
    show_sources, border_color, border_width,
    limits, scale_bar) {

  cols <- .resolve_vars(variables, x)

  # Build long-format data
  geom_col <- attr(x$tracts_sf, "sf_column") %||% "geometry"
  pieces <- lapply(cols, function(col) {
    vals <- .compute_quantity(x, col, type)
    label <- .auto_title(col, x)
    df <- data.frame(
      .facet_var = label,
      value = vals,
      stringsAsFactors = FALSE
    )
    df[[geom_col]] <- sf::st_geometry(x$tracts_sf)
    sf::st_as_sf(df)
  })
  sf_long <- do.call(rbind, pieces)
  # Preserve facet order as specified by user
  sf_long$.facet_var <- factor(sf_long$.facet_var,
                               levels = unique(sf_long$.facet_var))

  # Compute breaks across all values
  all_vals <- sf_long$value
  brk <- .compute_breaks(all_vals, breaks, n_breaks)

  if (is.null(title)) title <- NULL
  if (is.null(subtitle)) subtitle <- .auto_subtitle(x, type, breaks)
  if (is.null(legend_title)) legend_title <- .quantity_label(type)
  if (is.null(caption)) caption <- .auto_caption()

  p <- ggplot2::ggplot(sf_long) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$value),
      color = border_color,
      linewidth = border_width
    ) +
    ggplot2::facet_wrap(~ .facet_var) +
    .build_fill_scale(palette, brk, n_breaks, type = type) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  fill = legend_title, caption = caption) +
    .map_theme()

  # Crop to limits
  if (!is.null(limits)) {
    p <- p + ggplot2::coord_sf(
      xlim = limits[1:2], ylim = limits[3:4], expand = FALSE
    )
  }

  # Scale bar
  if (scale_bar && requireNamespace("ggspatial", quietly = TRUE)) {
    p <- p + ggspatial::annotation_scale(
      location = "bl", width_hint = 0.25,
      line_width = 0.5, height = ggplot2::unit(0.15, "cm"),
      text_cex = 0.6
    )
  }

  # Legend guide for binned scales
  if (!is.null(brk)) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = ggplot2::unit(1.5, "cm"),
      keyheight = ggplot2::unit(0.3, "cm"),
      nrow = 1
    ))
  }

  if (show_sources && !is.null(x$sources_sf)) {
    p <- p + ggplot2::geom_sf(
      data = x$sources_sf,
      color = "black", size = 0.5, alpha = 0.6,
      inherit.aes = FALSE
    )
  }

  p
}


# --- Label generators ---

#' Auto-generate plot title from dictionary (title-cased)
#' @noRd
.auto_title <- function(col, result) {
  dict <- result$dictionary
  if (is.null(dict)) return(col)
  row <- dict[dict$column == col, , drop = FALSE]
  if (nrow(row) == 0L) return(col)
  if (row$type[1L] == "candidate" && !is.na(row$candidate_name[1L])) {
    name <- .title_case_pt(row$candidate_name[1L])
    # Include party and ballot number: "Name (PT — 13)"
    meta <- character(0)
    if (!is.na(row$party[1L])) meta <- c(meta, row$party[1L])
    if (!is.na(row$ballot_number[1L])) meta <- c(meta, row$ballot_number[1L])
    suffix <- if (length(meta) > 0) {
      paste0(" (", paste(meta, collapse = " \u2014 "), ")")
    } else {
      ""
    }
    return(paste0(name, suffix))
  }
  label <- .format_dict_label(row[1L, ])
  if (nzchar(label)) label else col
}

#' Auto-generate subtitle from metadata, quantity type, and break method
#' @noRd
.auto_subtitle <- function(result, type = NULL, breaks = NULL) {
  parts <- character(0)
  if (!is.null(result$nome_municipio)) {
    parts <- c(parts, sprintf("%s (%s)", result$nome_municipio, result$uf))
  }
  if (!is.null(result$year)) {
    parts <- c(parts, as.character(result$year))
  }

  if (!is.null(type) && type != "absolute") {
    parts <- c(parts, .quantity_label(type))
  }
  if (is.character(breaks) && breaks != "continuous") {
    parts <- c(parts, tools::toTitleCase(breaks))
  }
  if (length(parts) == 0L) return(NULL)
  paste(parts, collapse = " \u2014 ")
}

#' Auto-generate caption with data source
#' @noRd
.auto_caption <- function() {
  "Source: TSE | Spatial interpolation to census tracts"
}

#' Shared map theme for all plot methods
#' @noRd
.map_theme <- function() {
  ggplot2::theme_void() +
    ggplot2::theme(
      axis.text        = ggplot2::element_text(size = 7, color = "grey40"),
      axis.ticks       = ggplot2::element_line(color = "grey60", linewidth = 0.3),
      axis.ticks.length = ggplot2::unit(0.1, "cm"),
      plot.title    = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 10, color = "grey40", hjust = 0,
                                             margin = ggplot2::margin(b = 8)),
      plot.caption  = ggplot2::element_text(size = 9, hjust = 1, color = "grey50"),
      legend.position = "bottom",
      plot.margin   = ggplot2::margin(10, 10, 10, 10)
    )
}

#' Human-readable label for quantity type
#' @noRd
.quantity_label <- function(type) {
  switch(type,
    absolute     = "Count",
    pct_tract    = "% of tract votes",
    pct_muni     = "% of municipality total",
    pct_valid    = "% of valid votes",
    pct_eligible = "% of eligible voters",
    density      = "per km\u00b2",
    type
  )
}


# --- Scale/break helpers ---

#' Compute break points for binned scales
#' @noRd
.compute_breaks <- function(values, method, n_breaks) {
  # Custom numeric breaks: passthrough

  if (is.numeric(method)) return(method)

  switch(method,
    continuous = NULL,
    quantile = {
      brks <- stats::quantile(values,
                               probs = seq(0, 1, length.out = n_breaks + 1L),
                               na.rm = TRUE)
      as.numeric(unique(brks))
    },
    jenks = {
      if (!requireNamespace("classInt", quietly = TRUE)) {
        stop(
          "The 'classInt' package is required for breaks = 'jenks'.\n",
          "Install with: install.packages(\"classInt\")",
          call. = FALSE
        )
      }
      clean <- values[!is.na(values)]
      classInt::classIntervals(clean, n = n_breaks, style = "jenks")$brks
    },
    stop(sprintf("Unknown breaks method: '%s'. Use 'continuous', 'quantile', 'jenks', or a numeric vector.",
                 method), call. = FALSE)
  )
}


#' Build the ggplot2 fill scale
#' @noRd
.build_fill_scale <- function(palette, breaks, n_breaks, direction = -1,
                               type = "absolute") {
  viridis_names <- c("viridis", "magma", "plasma", "inferno",
                     "cividis", "mako", "rocket", "turbo")
  is_viridis <- tolower(palette) %in% viridis_names
  labels_fn <- .scale_labels(type)

  if (is.null(breaks)) {
    # Continuous scale
    if (is_viridis) {
      ggplot2::scale_fill_viridis_c(option = palette, direction = direction,
                                     na.value = "grey90", labels = labels_fn)
    } else {
      ggplot2::scale_fill_distiller(palette = palette, direction = direction,
                                     na.value = "grey90", labels = labels_fn)
    }
  } else {
    # Binned scale
    if (is_viridis) {
      ggplot2::scale_fill_viridis_b(option = palette, breaks = breaks,
                                     direction = direction,
                                     na.value = "grey90", labels = labels_fn)
    } else {
      n_colors <- max(length(breaks) - 1L, 3L)
      pal_info <- tryCatch(
        RColorBrewer::brewer.pal.info[palette, ],
        error = function(e) NULL
      )
      max_n <- if (!is.null(pal_info)) pal_info$maxcolors else 9L
      colors <- RColorBrewer::brewer.pal(min(n_colors, max_n), palette)
      if (n_colors > max_n) {
        colors <- grDevices::colorRampPalette(colors)(n_colors)
      }
      if (direction == -1) colors <- rev(colors)
      ggplot2::scale_fill_stepsn(colours = colors, breaks = breaks,
                                  na.value = "grey90", labels = labels_fn)
    }
  }
}

#' Build label formatting function based on quantity type
#' @noRd
.scale_labels <- function(type) {
  pct_types <- c("pct_tract", "pct_muni", "pct_valid", "pct_eligible")
  if (type %in% pct_types) {
    function(x) ifelse(is.na(x), "", paste0(round(x, 1), "%"))
  } else if (type == "density") {
    function(x) ifelse(is.na(x), "", format(round(x, 1), big.mark = ","))
  } else {
    function(x) ifelse(is.na(x), "", format(round(x), big.mark = ","))
  }
}


# Base R %||% operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x

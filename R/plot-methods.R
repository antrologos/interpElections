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
#' @param type Quantity to map: `"absolute"` (default), `"pct_tract"`,
#'   `"pct_muni"`, `"pct_valid"`, `"pct_eligible"`, `"density"`.
#' @param palette Color palette name. RColorBrewer sequential palettes
#'   (e.g., `"YlOrRd"`, `"Blues"`, `"Spectral"`) or viridis palettes
#'   (e.g., `"viridis"`, `"magma"`, `"plasma"`). Default: `"YlOrRd"`.
#' @param breaks Scale breaks: `"continuous"` (default), `"quantile"`,
#'   `"jenks"` (requires classInt), or a numeric vector of custom
#'   break points.
#' @param n_breaks Number of breaks for `"quantile"` or `"jenks"`.
#'   Default: 5.
#' @param title Plot title. NULL = auto-generated from dictionary.
#' @param subtitle Plot subtitle. NULL = auto-generated from
#'   municipality/year metadata.
#' @param legend_title Legend title. NULL = auto-generated from
#'   quantity type.
#' @param show_sources Overlay source points (polling stations)
#'   on the map. Requires `sources_sf` in the result
#'   (use `keep = "sources_sf"` when interpolating). Default: FALSE.
#' @param border_color Tract border color. Default: NA (no borders).
#' @param border_width Tract border width. Default: 0.1.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly). Can be further customized
#'   with ggplot2 `+` syntax.
#'
#' @examples
#' \dontrun{
#' result <- interpolate_election_br(3304557, 2020, 2022)
#'
#' # By candidate name
#' plot(result, variable = "Lula")
#'
#' # By ballot number
#' plot(result, variable = 13)
#'
#' # Percentage of tract votes, quantile breaks
#' plot(result, variable = "PT", type = "pct_tract",
#'      breaks = "quantile", n_breaks = 5)
#'
#' # Faceted comparison
#' plot(result, variable = c("Lula", "Bolsonaro"), type = "pct_tract")
#'
#' # Composable with ggplot2
#' library(ggplot2)
#' plot(result, variable = "PT") + theme_dark()
#' }
#'
#' @exportS3Method
plot.interpElections_result <- function(
    x, variable = NULL, type = "absolute",
    palette = "YlOrRd", breaks = "continuous", n_breaks = 5L,
    title = NULL, subtitle = NULL, legend_title = NULL,
    show_sources = FALSE, border_color = NA, border_width = 0.1,
    ...) {

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
                       title, subtitle, legend_title,
                       show_sources, border_color, border_width)
    return(invisible(p))
  }

  col <- .resolve_var(variable, x)
  values <- .compute_quantity(x, col, type)

  # Build plotting sf
  plot_sf <- x$tracts_sf
  plot_sf$.plot_value <- values

  # Auto-generate labels
  if (is.null(title)) title <- .auto_title(col, x)
  if (is.null(subtitle)) subtitle <- .auto_subtitle(x)
  if (is.null(legend_title)) legend_title <- .quantity_label(type)

  # Compute breaks
  brk <- .compute_breaks(values, breaks, n_breaks)

  # Build plot
  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.plot_value),
      color = border_color,
      linewidth = border_width
    ) +
    .build_fill_scale(palette, brk, n_breaks) +
    ggplot2::labs(title = title, subtitle = subtitle, fill = legend_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

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
    title, subtitle, legend_title,
    show_sources, border_color, border_width) {

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
  if (is.null(subtitle)) subtitle <- .auto_subtitle(x)
  if (is.null(legend_title)) legend_title <- .quantity_label(type)

  p <- ggplot2::ggplot(sf_long) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$value),
      color = border_color,
      linewidth = border_width
    ) +
    ggplot2::facet_wrap(~ .facet_var) +
    .build_fill_scale(palette, brk, n_breaks) +
    ggplot2::labs(title = title, subtitle = subtitle, fill = legend_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

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

#' Auto-generate plot title from dictionary
#' @noRd
.auto_title <- function(col, result) {
  dict <- result$dictionary
  if (is.null(dict)) return(col)
  row <- dict[dict$column == col, , drop = FALSE]
  if (nrow(row) == 0L) return(col)
  label <- .format_dict_label(row[1L, ])
  if (nzchar(label)) label else col
}

#' Auto-generate subtitle from metadata
#' @noRd
.auto_subtitle <- function(result) {
  parts <- character(0)
  if (!is.null(result$nome_municipio)) {
    parts <- c(parts, sprintf("%s (%s)", result$nome_municipio, result$uf))
  }
  if (!is.null(result$year)) {
    parts <- c(parts, as.character(result$year))
  }
  if (length(parts) == 0L) return(NULL)
  paste(parts, collapse = " - ")
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
.build_fill_scale <- function(palette, breaks, n_breaks, direction = 1) {
  viridis_names <- c("viridis", "magma", "plasma", "inferno",
                     "cividis", "mako", "rocket", "turbo")
  is_viridis <- tolower(palette) %in% viridis_names

  if (is.null(breaks)) {
    # Continuous scale
    if (is_viridis) {
      ggplot2::scale_fill_viridis_c(option = palette, direction = direction)
    } else {
      ggplot2::scale_fill_distiller(palette = palette, direction = direction)
    }
  } else {
    # Binned scale
    if (is_viridis) {
      ggplot2::scale_fill_viridis_b(option = palette, breaks = breaks,
                                     direction = direction)
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
      ggplot2::scale_fill_stepsn(colours = colors, breaks = breaks)
    }
  }
}


# Base R %||% operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x

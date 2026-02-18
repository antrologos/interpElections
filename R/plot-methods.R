# ggplot2-based plotting for interpElections_result objects

# Package-level cache for municipality boundaries (avoids repeated downloads)
.plot_cache <- new.env(parent = emptyenv())


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

  # Inform about data range when custom breaks are used
  if (is.numeric(breaks)) {
    rng <- range(values, na.rm = TRUE)
    message(sprintf("Data range: [%.4g, %.4g]. Custom breaks: %s",
                    rng[1], rng[2],
                    paste(breaks, collapse = ", ")))
  }

  # Compute breaks
  brk <- .compute_breaks(values, breaks, n_breaks)

  # For binned scales: convert to factor with interval labels
  if (!is.null(brk)) {
    plot_sf$.plot_value <- .cut_values(values, brk, type)
  }

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

  # Municipality contour
  border_layer <- .muni_border_layer(x$code_muni, plot_sf)
  if (!is.null(border_layer)) p <- p + border_layer

  # Crop to limits (interpreted as lon/lat, transformed to data CRS)
  if (!is.null(limits)) {
    lim <- .prepare_limits(limits, plot_sf)
    p <- p + ggplot2::coord_sf(
      xlim = lim$xlim, ylim = lim$ylim, expand = FALSE
    )
  }

  # Scale bar
  if (scale_bar && requireNamespace("ggspatial", quietly = TRUE)) {
    p <- tryCatch(
      p + ggspatial::annotation_scale(
        location = "bl", width_hint = 0.25,
        line_width = 0.5, height = ggplot2::unit(0.15, "cm"),
        text_cex = 0.6
      ),
      error = function(e) {
        message("Scale bar could not be drawn: ", conditionMessage(e))
        p
      }
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

  # Inform about data range when custom breaks are used
  all_vals <- sf_long$value
  if (is.numeric(breaks)) {
    rng <- range(all_vals, na.rm = TRUE)
    message(sprintf("Data range: [%.4g, %.4g]. Custom breaks: %s",
                    rng[1], rng[2],
                    paste(breaks, collapse = ", ")))
  }

  # Compute breaks across all values
  brk <- .compute_breaks(all_vals, breaks, n_breaks)

  # For binned scales: convert to factor with interval labels
  if (!is.null(brk)) {
    sf_long$value <- .cut_values(sf_long$value, brk, type)
  }

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

  # Municipality contour
  border_layer <- .muni_border_layer(x$code_muni, x$tracts_sf)
  if (!is.null(border_layer)) p <- p + border_layer

  # Crop to limits (interpreted as lon/lat, transformed to data CRS)
  if (!is.null(limits)) {
    lim <- .prepare_limits(limits, x$tracts_sf)
    p <- p + ggplot2::coord_sf(
      xlim = lim$xlim, ylim = lim$ylim, expand = FALSE
    )
  }

  # Scale bar
  if (scale_bar && requireNamespace("ggspatial", quietly = TRUE)) {
    p <- tryCatch(
      p + ggspatial::annotation_scale(
        location = "bl", width_hint = 0.25,
        line_width = 0.5, height = ggplot2::unit(0.15, "cm"),
        text_cex = 0.6
      ),
      error = function(e) {
        message("Scale bar could not be drawn: ", conditionMessage(e))
        p
      }
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
      brks <- .clean_breaks(brks)
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
      brks <- classInt::classIntervals(clean, n = n_breaks, style = "jenks")$brks
      brks <- .clean_breaks(brks)
    },
    stop(sprintf("Unknown breaks method: '%s'. Use 'continuous', 'quantile', 'jenks', or a numeric vector.",
                 method), call. = FALSE)
  )
}


#' Round computed breaks to clean values for display
#'
#' Collapses near-duplicate break points (floating-point noise from
#' quantile interpolation), then rounds to the fewest significant
#' digits that still keep all remaining breaks distinct.
#' @noRd
.clean_breaks <- function(brks) {
  brks <- sort(as.numeric(unique(brks)))
  if (length(brks) < 2L) return(brks)

  # Collapse near-duplicates (within 1e-9 of total range)
  rng <- diff(range(brks))
  if (rng > 0) {
    tol <- rng * 1e-9
    keep <- c(TRUE, diff(brks) > tol)
    brks <- brks[keep]
  }

  # Round for display, increasing precision until distinct breaks are preserved
  n_target <- length(brks)
  for (digits in 3L:8L) {
    rounded <- signif(brks, digits)
    # Replace near-zero values with exact 0
    r <- diff(range(rounded))
    if (r > 0) rounded[abs(rounded) < r * 1e-4] <- 0
    result <- unique(rounded)
    if (length(result) >= n_target) return(result)
  }

  unique(brks)
}



#' Build the ggplot2 fill scale
#' @noRd
.build_fill_scale <- function(palette, breaks, n_breaks, direction = -1,
                               type = "absolute") {
  if (is.null(breaks)) {
    # Continuous scale
    viridis_names <- c("viridis", "magma", "plasma", "inferno",
                       "cividis", "mako", "rocket", "turbo")
    is_viridis <- tolower(palette) %in% viridis_names
    labels_fn <- .scale_labels(type, breaks)

    if (is_viridis) {
      ggplot2::scale_fill_viridis_c(option = palette, direction = direction,
                                     na.value = "grey90", labels = labels_fn)
    } else {
      ggplot2::scale_fill_distiller(palette = palette, direction = direction,
                                     na.value = "grey90", labels = labels_fn)
    }
  } else {
    # Discrete scale from cut() bins
    bin_labels <- .make_bin_labels(breaks, type)
    n_bins <- length(bin_labels)
    colors <- .bin_colors(palette, n_bins, direction)
    names(colors) <- bin_labels
    ggplot2::scale_fill_manual(values = colors, na.value = "grey90",
                                drop = FALSE)
  }
}

#' Build label formatting function based on quantity type
#' @noRd
.scale_labels <- function(type, breaks = NULL) {
  pct_types <- c("pct_tract", "pct_muni", "pct_valid", "pct_eligible")
  if (type %in% pct_types) {
    digits <- .label_precision(breaks, default = 1L)
    function(x) ifelse(is.na(x), "", paste0(round(x, digits), "%"))
  } else if (type == "density") {
    digits <- .label_precision(breaks, default = 1L)
    function(x) ifelse(is.na(x), "", format(round(x, digits), big.mark = ","))
  } else {
    digits <- .label_precision(breaks, default = 0L)
    function(x) ifelse(is.na(x), "", format(round(x, digits), big.mark = ","))
  }
}

#' Compute label decimal precision from break values
#'
#' Finds the minimum number of decimal places needed to represent
#' all finite, non-zero break values exactly (up to floating-point
#' tolerance).
#' @noRd
.label_precision <- function(breaks, default = 1L) {
  if (is.null(breaks)) return(default)
  finite_brks <- breaks[is.finite(breaks) & breaks != 0]
  if (length(finite_brks) == 0L) return(default)
  digits_needed <- vapply(finite_brks, function(v) {
    for (d in seq_len(10L)) {
      if (abs(round(v, d) - v) < .Machine$double.eps * 100) return(d)
    }
    10L
  }, integer(1L))
  max(default, max(digits_needed))
}


#' Generate human-readable interval labels from break points
#'
#' Percentages: at most 2 significant digits. Absolute: no decimals.
#'
#' @param breaks Numeric vector of break points (length >= 2).
#' @param type Quantity type for suffix formatting.
#' @return Character vector of length `length(breaks) - 1`.
#' @noRd
.make_bin_labels <- function(breaks, type) {
  n <- length(breaks) - 1L
  pct_types <- c("pct_tract", "pct_muni", "pct_valid", "pct_eligible")

  suffix <- if (type %in% pct_types) {
    "%"
  } else if (type == "density") {
    "/km\u00b2"
  } else {
    ""
  }

  is_pct <- type %in% c(pct_types, "density")

  if (is_pct) {
    # At least 1 decimal place; increase until all finite breaks are distinct
    finite_brks <- breaks[is.finite(breaks)]
    digits <- 1L
    for (d in seq_len(6L)) {
      digits <- d
      fmtd <- format(round(finite_brks, d), nsmall = d,
                      scientific = FALSE, trim = TRUE)
      if (length(unique(fmtd)) == length(finite_brks)) break
    }
    fmt <- function(x) {
      format(round(x, digits), nsmall = digits, scientific = FALSE,
             big.mark = ",", trim = TRUE)
    }
  } else {
    # Absolute: no decimal places
    fmt <- function(x) format(round(x), big.mark = ",", trim = TRUE)
  }

  labels <- character(n)
  for (i in seq_len(n)) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]

    if (is.infinite(lo) && lo < 0) {
      labels[i] <- paste0("<", fmt(hi), suffix)
    } else if (is.infinite(hi) && hi > 0) {
      labels[i] <- paste0(">", fmt(lo), suffix)
    } else {
      labels[i] <- paste0(fmt(lo), "\u2013", fmt(hi), suffix)
    }
  }
  labels
}


#' Generate N discrete colors from a palette
#'
#' @param palette Palette name (RColorBrewer or viridis).
#' @param n Number of colors needed.
#' @param direction Color direction (-1 = reversed).
#' @return Character vector of hex colors.
#' @noRd
.bin_colors <- function(palette, n, direction = -1) {
  viridis_names <- c("viridis", "magma", "plasma", "inferno",
                     "cividis", "mako", "rocket", "turbo")
  is_viridis <- tolower(palette) %in% viridis_names

  if (is_viridis && requireNamespace("viridisLite", quietly = TRUE)) {
    colors <- viridisLite::viridis(n, option = palette, direction = direction)
  } else {
    n_fetch <- max(n, 3L)
    pal_info <- tryCatch(
      RColorBrewer::brewer.pal.info[palette, ],
      error = function(e) NULL
    )
    max_n <- if (!is.null(pal_info)) pal_info$maxcolors else 9L
    base_colors <- RColorBrewer::brewer.pal(min(n_fetch, max_n), palette)
    if (n_fetch > max_n) {
      base_colors <- grDevices::colorRampPalette(base_colors)(n_fetch)
    }
    if (direction == -1) base_colors <- rev(base_colors)
    colors <- if (n == n_fetch) {
      base_colors
    } else {
      grDevices::colorRampPalette(base_colors)(n)
    }
  }
  colors
}


#' Cut numeric values into labeled factor bins
#'
#' @param values Numeric vector.
#' @param breaks Numeric break vector.
#' @param type Quantity type (for label formatting).
#' @return Factor with human-readable bin labels.
#' @noRd
.cut_values <- function(values, breaks, type) {
  if (length(breaks) < 2L) {
    return(factor(rep("All values equal", length(values))))
  }

  bin_labels <- .make_bin_labels(breaks, type)

  # Extend outermost breaks to cover full data range (prevents NAs)
  brk <- breaks
  rng <- range(values, na.rm = TRUE)
  if (is.finite(rng[1]) && rng[1] < brk[1]) brk[1] <- rng[1]
  if (is.finite(rng[2]) && rng[2] > brk[length(brk)]) brk[length(brk)] <- rng[2]

  cut(values, breaks = brk, labels = bin_labels,
      include.lowest = TRUE, right = TRUE)
}


#' Download/cache municipality boundary sf object
#'
#' Downloads the municipality boundary via geobr and caches it in
#' `.plot_cache`. Optionally transforms to a target CRS.
#'
#' @param code_muni IBGE municipality code.
#' @param target_crs Target CRS (from sf::st_crs). NULL to skip transform.
#' @return An sf object or NULL.
#' @noRd
.get_muni_sf <- function(code_muni, target_crs = NULL) {
  if (is.null(code_muni)) return(NULL)
  if (!requireNamespace("geobr", quietly = TRUE)) return(NULL)

  cache_key <- paste0("muni_", code_muni)

  if (exists(cache_key, envir = .plot_cache)) {
    muni_sf <- get(cache_key, envir = .plot_cache)
  } else {
    muni_sf <- tryCatch(
      suppressMessages(
        geobr::read_municipality(code_muni = as.numeric(code_muni), year = 2022)
      ),
      error = function(e) {
        message("Could not download municipality boundary: ",
                conditionMessage(e))
        NULL
      }
    )
    if (!is.null(muni_sf)) {
      assign(cache_key, muni_sf, envir = .plot_cache)
    }
  }
  if (is.null(muni_sf)) return(NULL)

  # Match CRS if requested
  if (!is.null(target_crs) && !is.na(target_crs) &&
      sf::st_crs(muni_sf) != target_crs) {
    muni_sf <- sf::st_transform(muni_sf, target_crs)
  }

  muni_sf
}


#' Create a ggplot2 layer for the municipality contour
#'
#' @param code_muni IBGE municipality code.
#' @param tracts_sf An sf object (for CRS matching).
#' @return A ggplot2 layer (geom_sf) or NULL.
#' @noRd
.muni_border_layer <- function(code_muni, tracts_sf) {
  muni_sf <- .get_muni_sf(code_muni, sf::st_crs(tracts_sf))
  if (is.null(muni_sf)) return(NULL)

  ggplot2::geom_sf(
    data = muni_sf,
    fill = NA,
    color = "grey30",
    linewidth = 0.5,
    inherit.aes = FALSE
  )
}


#' Transform lon/lat limits to the data CRS and validate overlap
#'
#' Limits are always interpreted as lon/lat (EPSG:4326) and
#' transformed to the data CRS for coord_sf. Returns a list with
#' xlim and ylim in the data CRS.
#' @noRd
.prepare_limits <- function(limits, sf_data) {
  data_crs <- sf::st_crs(sf_data)

  # Transform from lon/lat to data CRS
  limit_pts <- sf::st_sfc(
    sf::st_point(c(limits[1], limits[3])),
    sf::st_point(c(limits[2], limits[4])),
    crs = 4326
  )

  if (!is.na(data_crs) && data_crs != sf::st_crs(4326)) {
    limit_pts <- sf::st_transform(limit_pts, data_crs)
  }

  coords <- sf::st_coordinates(limit_pts)
  xlim <- sort(coords[, 1])
  ylim <- sort(coords[, 2])

  # Check overlap with data bbox
  bbox <- sf::st_bbox(sf_data)
  x_ok <- xlim[1] < bbox["xmax"] && xlim[2] > bbox["xmin"]
  y_ok <- ylim[1] < bbox["ymax"] && ylim[2] > bbox["ymin"]
  if (!x_ok || !y_ok) {
    message(
      "limits do not overlap with data extent. The map may appear empty.\n",
      sprintf("  Your limits (lon/lat): x [%.4f, %.4f], y [%.4f, %.4f]\n",
              limits[1], limits[2], limits[3], limits[4]),
      "  Check coordinate signs (e.g., South/West = negative)."
    )
  }

  list(xlim = xlim, ylim = ylim)
}


# Base R %||% operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x

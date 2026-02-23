# Diagnostic plots for alpha decay parameters

#' Plot alpha decay parameter diagnostics
#'
#' Visualize the spatial distribution and bracket variation of optimized
#' alpha decay parameters.
#'
#' @param result An `interpElections_result` object.
#' @param type Character. Plot type: `"map"`, `"histogram"`, or `"bracket"`.
#' @param summary_fn Summary function for collapsing alpha across brackets
#'   when `type = "map"`. One of `"median"` (default), `"mean"`,
#'   `"pop_weighted"`, `"min"`, `"max"`, `"range"`, or an integer
#'   bracket index.
#' @param brackets Integer vector. Bracket indices to include. NULL = all.
#' @param palette Character. Color palette. Default: `"YlOrRd"`.
#' @param breaks Break method: `"quantile"`, `"continuous"`, `"jenks"`, or
#'   numeric vector.
#' @param n_breaks Integer. Number of breaks. Default: 5.
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly). Prints the plot.
#' @export
plot_alpha <- function(result, type = c("map", "histogram", "bracket"),
                       summary_fn = "median", brackets = NULL,
                       palette = "YlOrRd", breaks = "quantile",
                       n_breaks = 5L, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "The 'ggplot2' package is required for plot_alpha().\n",
      "Install with: install.packages(\"ggplot2\")",
      call. = FALSE
    )
  }

  type <- match.arg(type)

  alpha <- result$alpha
  if (is.null(alpha)) {
    stop("No alpha values in result object.", call. = FALSE)
  }

  p <- switch(type,
    map       = .plot_alpha_map(result, alpha, summary_fn, palette,
                                breaks, n_breaks),
    histogram = .plot_alpha_histogram(result, alpha, brackets),
    bracket   = .plot_alpha_bracket(result, alpha, brackets)
  )

  print(p)
  invisible(p)
}


# --- Map type ---

#' @noRd
.plot_alpha_map <- function(result, alpha, summary_fn, palette,
                            breaks, n_breaks) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for map plots.", call. = FALSE)
  }
  if (is.null(result$tracts_sf)) {
    stop("No tracts_sf in result object.", call. = FALSE)
  }

  # Collapse matrix alpha to vector if needed
  if (is.matrix(alpha) && ncol(alpha) > 1L) {
    pop_matrix <- NULL
    if (identical(summary_fn, "pop_weighted") ||
        identical(summary_fn, "pop_weighted")) {
      tracts_df <- sf::st_drop_geometry(result$tracts_sf)
      pop_cols <- result$calib_cols$tracts
      pop_matrix <- as.matrix(tracts_df[, pop_cols, drop = FALSE])
      storage.mode(pop_matrix) <- "double"
    }
    values <- .collapse_alpha(alpha, pop_matrix, summary_fn)
  } else {
    values <- as.numeric(alpha)
  }

  plot_sf <- result$tracts_sf
  plot_sf$.plot_value <- values

  # Title
  if (is.matrix(alpha) && ncol(alpha) > 1L) {
    if (is.numeric(summary_fn)) {
      title_suffix <- paste0("bracket ", summary_fn)
    } else {
      title_suffix <- summary_fn
    }
    title <- paste0("Alpha decay parameter (", title_suffix, ")")
  } else {
    title <- "Alpha decay parameter"
  }

  # Compute breaks
  brk <- .compute_breaks(values, breaks, n_breaks)

  if (!is.null(brk)) {
    plot_sf$.plot_value <- .cut_values(values, brk, "absolute")
  }

  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.plot_value),
      color = "white",
      linewidth = 0.05
    ) +
    .build_fill_scale(palette, brk, n_breaks, type = "absolute") +
    ggplot2::labs(title = title, fill = "Alpha") +
    .map_theme()

  # Municipality contour
  border_layer <- .muni_border_layer(result$code_muni, plot_sf,
                                     result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

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

  p
}


# --- Histogram type ---

#' @noRd
.plot_alpha_histogram <- function(result, alpha, brackets) {
  if (is.matrix(alpha) && ncol(alpha) > 1L) {
    labels <- .bracket_labels(result)
    if (is.null(labels)) labels <- paste0("Bracket ", seq_len(ncol(alpha)))

    # Filter brackets
    idx <- seq_len(ncol(alpha))
    if (!is.null(brackets)) {
      idx <- brackets[brackets >= 1L & brackets <= ncol(alpha)]
    }

    long <- data.frame(
      bracket = rep(labels[idx], each = nrow(alpha)),
      alpha = as.numeric(alpha[, idx]),
      stringsAsFactors = FALSE
    )
    long$bracket <- factor(long$bracket, levels = labels[idx])

    nrow_facet <- if (length(idx) > 7L) 2L else 1L

    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$alpha)) +
      ggplot2::geom_density(fill = "steelblue", alpha = 0.5) +
      ggplot2::facet_wrap(~ .data$bracket, nrow = nrow_facet) +
      ggplot2::labs(x = "Alpha", y = "Density",
                    title = "Alpha distribution by bracket") +
      ggplot2::theme_minimal()
  } else {
    vals <- as.numeric(alpha)
    df <- data.frame(alpha = vals)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$alpha)) +
      ggplot2::geom_density(fill = "steelblue", alpha = 0.5) +
      ggplot2::labs(x = "Alpha", y = "Density",
                    title = "Alpha distribution") +
      ggplot2::theme_minimal()
  }

  p
}


# --- Bracket boxplot type ---

#' @noRd
.plot_alpha_bracket <- function(result, alpha, brackets) {
  if (!is.matrix(alpha) || ncol(alpha) <= 1L) {
    stop(
      "type = \"bracket\" requires a matrix alpha (multiple brackets).",
      call. = FALSE
    )
  }

  labels <- .bracket_labels(result)
  if (is.null(labels)) labels <- paste0("Bracket ", seq_len(ncol(alpha)))

  idx <- seq_len(ncol(alpha))
  if (!is.null(brackets)) {
    idx <- brackets[brackets >= 1L & brackets <= ncol(alpha)]
  }

  long <- data.frame(
    bracket = rep(labels[idx], each = nrow(alpha)),
    alpha = as.numeric(alpha[, idx]),
    stringsAsFactors = FALSE
  )
  long$bracket <- factor(long$bracket, levels = labels[idx])

  p <- ggplot2::ggplot(long,
                        ggplot2::aes(x = .data$bracket, y = .data$alpha)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Bracket", y = "Alpha",
                  title = "Alpha by calibration bracket") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  p
}

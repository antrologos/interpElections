# Weight matrix diagnostics: summary, weight maps, and connection plots


#' Summarize weight matrix properties per tract
#'
#' Computes per-tract statistics from the weight matrix, including
#' concentration measures (Herfindahl, entropy), effective number of
#' sources, and optionally travel-time-weighted means.
#'
#' @param result An `interpElections_result` object.
#' @return A data frame with one row per tract.
#' @export
weight_summary <- function(result) {
  if (!inherits(result, "interpElections_result")) {
    stop("'result' must be an interpElections_result object.", call. = FALSE)
  }
  .require_weights(result)
  W <- .get_weights(result)

  n <- nrow(W)
  m <- ncol(W)

  # Tract IDs
  tract_ids <- if (!is.null(result$tracts_sf) && !is.null(result$tract_id)) {
    if (requireNamespace("sf", quietly = TRUE)) {
      sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]]
    } else {
      result$tracts_sf[[result$tract_id]]
    }
  } else {
    seq_len(n)
  }

  dominant_source <- integer(n)
  dominant_weight <- numeric(n)
  top3_weight <- numeric(n)
  entropy <- numeric(n)
  herfindahl <- numeric(n)

  for (i in seq_len(n)) {
    w <- W[i, ]
    ord <- order(w, decreasing = TRUE)
    dominant_source[i] <- ord[1L]
    dominant_weight[i] <- w[ord[1L]]
    top3_weight[i] <- sum(w[ord[seq_len(min(3L, m))]])
    # Row-normalize for concentration measures
    # (W is column-normalized, so row sums != 1)
    rs <- sum(w)
    p <- if (rs > 0) w / rs else w
    pos <- p[p > 0]
    entropy[i] <- if (length(pos) > 0L) -sum(pos * log(pos)) else 0
    herfindahl[i] <- sum(p^2)
  }

  effective_n <- exp(entropy)

  # Travel-time weighted mean
  tt <- result$time_matrix
  mean_tt_weighted <- if (!is.null(tt) && nrow(tt) == n && ncol(tt) == m) {
    rowSums(W * tt)
  } else {
    rep(NA_real_, n)
  }

  data.frame(
    tract_id = tract_ids,
    dominant_source = dominant_source,
    dominant_weight = dominant_weight,
    top3_weight = top3_weight,
    entropy = entropy,
    effective_n_sources = effective_n,
    herfindahl = herfindahl,
    mean_travel_time_weighted = mean_tt_weighted,
    stringsAsFactors = FALSE
  )
}


#' Plot weight matrix diagnostics
#'
#' Produces choropleth maps of weight matrix properties: entropy
#' (effective number of sources), dominant station assignment, or
#' a catchment view showing weight distribution for a specific tract.
#'
#' @param result An `interpElections_result` object.
#' @param tract Tract ID or index for catchment mode. NULL for overview.
#' @param type Character. `"catchment"`, `"dominant"`, or `"entropy"`.
#' @param top_k Integer. Max stations to show in catchment.
#' @param threshold Numeric. Min weight to show a connection.
#' @param palette Color palette.
#' @param breaks Break method.
#' @param n_breaks Number of breaks.
#' @param interactive Logical. Use leaflet instead of ggplot2.
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly).
#' @export
plot_weights <- function(result, tract = NULL,
                          type = c("catchment", "dominant", "entropy"),
                          top_k = 5L, threshold = 0.01,
                          palette = "YlOrRd", breaks = "quantile",
                          n_breaks = 5L, interactive = FALSE, ...) {

  if (!inherits(result, "interpElections_result")) {
    stop("'result' must be an interpElections_result object.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plot_weights().", call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for plot_weights().", call. = FALSE)
  }
  .require_weights(result)
  if (is.null(result$tracts_sf)) {
    stop("No tracts_sf in result object.", call. = FALSE)
  }

  type <- match.arg(type)

  switch(type,
    entropy   = .plot_weights_entropy(result, palette, breaks, n_breaks),
    dominant  = .plot_weights_dominant(result, palette),
    catchment = .plot_weights_catchment(result, tract, top_k, threshold,
                                         palette, interactive)
  )
}


#' Plot tract-station connections
#'
#' Draws lines between tract centroids and their assigned stations,
#' with width and alpha proportional to weight.
#'
#' @param result An `interpElections_result` object.
#' @param tract Tract ID(s) or index(es). NULL for overview.
#' @param top_k Max connections per tract.
#' @param threshold Min weight for a connection.
#' @param show_all_tracts Logical. Show all tract polygons as background.
#' @param palette Color palette.
#' @param interactive Logical.
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly).
#' @export
plot_connections <- function(result, tract = NULL, top_k = NULL,
                              threshold = 0.01, show_all_tracts = TRUE,
                              palette = "viridis", interactive = FALSE, ...) {

  if (!inherits(result, "interpElections_result")) {
    stop("'result' must be an interpElections_result object.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plot_connections().",
         call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for plot_connections().", call. = FALSE)
  }
  .require_weights(result)
  .require_electoral_sf(result)
  if (is.null(result$tracts_sf)) {
    stop("No tracts_sf in result object.", call. = FALSE)
  }

  W <- .get_weights(result)
  n <- nrow(W)
  m <- ncol(W)

  # Tract centroids
  centroids <- sf::st_coordinates(
    sf::st_centroid(sf::st_geometry(result$tracts_sf))
  )
  # Station coordinates
  station_coords <- .get_station_coords(result)
  if (is.null(station_coords)) {
    stop("Cannot extract station coordinates from result.", call. = FALSE)
  }

  if (is.null(tract)) {
    # Overview mode: one line per tract to its dominant station
    p <- .plot_connections_overview(result, W, centroids, station_coords,
                                     show_all_tracts)
  } else {
    # Specific tract(s) mode
    tract_indices <- .resolve_tract_indices(tract, result)
    p <- .plot_connections_detail(result, W, centroids, station_coords,
                                   tract_indices, top_k, threshold,
                                   show_all_tracts, palette)
  }

  print(p)
  invisible(p)
}


# --- Internal: plot_weights type handlers ---

#' @noRd
.plot_weights_entropy <- function(result, palette, breaks, n_breaks) {
  ws <- weight_summary(result)
  plot_sf <- result$tracts_sf
  plot_sf$.eff_n <- ws$effective_n_sources

  values <- ws$effective_n_sources
  brk <- .compute_breaks(values, breaks, n_breaks)

  if (!is.null(brk)) {
    plot_sf$.eff_n <- .cut_values(values, brk, "absolute")
  }

  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.eff_n),
      color = "white", linewidth = 0.05
    ) +
    .build_fill_scale(palette, brk, n_breaks, type = "absolute") +
    ggplot2::labs(
      title = "Effective number of sources",
      fill = "Eff. sources"
    ) +
    .map_theme()

  border_layer <- .muni_border_layer(result$code_muni, plot_sf,
                                      result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

  if (!is.null(brk)) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = ggplot2::unit(1.5, "cm"),
      keyheight = ggplot2::unit(0.3, "cm"),
      nrow = 1
    ))
  }

  print(p)
  invisible(p)
}


#' @noRd
.plot_weights_dominant <- function(result, palette) {
  W <- .get_weights(result)
  n <- nrow(W)
  dom <- apply(W, 1, which.max)

  # Group into manageable number of categories
  unique_dom <- sort(unique(dom))
  n_stations <- length(unique_dom)

  plot_sf <- result$tracts_sf

  if (n_stations > 20L) {
    # Keep top 19 by frequency, group rest as "Other"
    freq <- sort(table(dom), decreasing = TRUE)
    top_ids <- as.integer(names(freq)[seq_len(min(19L, length(freq)))])
    labels <- ifelse(dom %in% top_ids, paste0("Station ", dom), "Other")
    plot_sf$.dominant <- factor(labels)
  } else {
    plot_sf$.dominant <- factor(paste0("Station ", dom))
  }

  n_levels <- nlevels(plot_sf$.dominant)
  colors <- .qualitative_colors(n_levels, palette)
  names(colors) <- levels(plot_sf$.dominant)

  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.dominant),
      color = "white", linewidth = 0.05
    ) +
    ggplot2::scale_fill_manual(values = colors, na.value = "grey90") +
    ggplot2::labs(
      title = "Dominant station per tract",
      fill = "Station"
    ) +
    .map_theme()

  # Overlay station points if available
  if (!is.null(result$electoral_sf)) {
    p <- p + ggplot2::geom_sf(
      data = result$electoral_sf,
      color = "black", size = 1.5, shape = 4,
      inherit.aes = FALSE
    )
  }

  border_layer <- .muni_border_layer(result$code_muni, plot_sf,
                                      result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

  print(p)
  invisible(p)
}


#' @noRd
.plot_weights_catchment <- function(result, tract, top_k, threshold,
                                      palette, interactive) {
  W <- .get_weights(result)
  n <- nrow(W)

  # Resolve tract index
  if (is.null(tract)) {
    # Pick tract with highest effective_n_sources
    ws <- weight_summary(result)
    tract_idx <- which.max(ws$effective_n_sources)
  } else {
    tract_idx <- .resolve_tract_indices(tract, result)
    if (length(tract_idx) > 1L) {
      tract_idx <- tract_idx[1L]
      message("plot_weights catchment mode uses first tract only.")
    }
  }

  w <- W[tract_idx, ]

  # Filter by threshold and top_k
  keep <- w >= threshold
  if (!is.null(top_k) && sum(keep) > top_k) {
    ord <- order(w, decreasing = TRUE)
    keep <- rep(FALSE, length(w))
    keep[ord[seq_len(top_k)]] <- TRUE
  }

  station_coords <- .get_station_coords(result)

  # Build plot
  p <- ggplot2::ggplot(result$tracts_sf) +
    ggplot2::geom_sf(fill = "grey95", color = "grey80", linewidth = 0.1)

  # Highlight selected tract
  tract_sf <- result$tracts_sf[tract_idx, , drop = FALSE]
  p <- p + ggplot2::geom_sf(
    data = tract_sf,
    fill = "#3182bd", color = "#08519c", linewidth = 0.6, alpha = 0.4
  )

  # Station points sized and colored by weight
  if (!is.null(station_coords) && any(keep)) {
    station_df <- data.frame(
      x = station_coords[keep, 1],
      y = station_coords[keep, 2],
      weight = w[keep]
    )
    p <- p + ggplot2::geom_point(
      data = station_df,
      ggplot2::aes(
        x = .data$x, y = .data$y,
        size = .data$weight, color = .data$weight
      ),
      inherit.aes = FALSE
    ) +
      ggplot2::scale_size_continuous(range = c(1, 6), guide = "none") +
      ggplot2::scale_color_distiller(
        palette = palette, direction = 1, na.value = "grey50",
        name = "Weight"
      )
  }

  # Tract label
  tract_label <- if (!is.null(result$tract_id) && !is.null(result$tracts_sf)) {
    sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]][tract_idx]
  } else {
    tract_idx
  }

  p <- p +
    ggplot2::labs(
      title = paste0("Catchment: tract ", tract_label),
      subtitle = sprintf("%d sources above threshold %.3f",
                         sum(keep), threshold)
    ) +
    .map_theme()

  border_layer <- .muni_border_layer(result$code_muni, result$tracts_sf,
                                      result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

  print(p)
  invisible(p)
}


# --- Internal: plot_connections helpers ---

#' @noRd
.plot_connections_overview <- function(result, W, centroids, station_coords,
                                        show_all_tracts) {
  n <- nrow(W)
  dom <- apply(W, 1, which.max)

  seg_df <- data.frame(
    cx = centroids[, 1],
    cy = centroids[, 2],
    sx = station_coords[dom, 1],
    sy = station_coords[dom, 2]
  )

  p <- ggplot2::ggplot()

  if (show_all_tracts) {
    p <- p + ggplot2::geom_sf(
      data = result$tracts_sf,
      fill = "grey95", color = "grey80", linewidth = 0.1,
      inherit.aes = FALSE
    )
  }

  p <- p +
    ggplot2::geom_segment(
      data = seg_df,
      ggplot2::aes(
        x = .data$cx, y = .data$cy,
        xend = .data$sx, yend = .data$sy
      ),
      color = "steelblue", alpha = 0.3, linewidth = 0.3
    )

  # Station points
  st_df <- data.frame(x = station_coords[, 1], y = station_coords[, 2])
  p <- p + ggplot2::geom_point(
    data = st_df,
    ggplot2::aes(x = .data$x, y = .data$y),
    color = "black", size = 1.2,
    inherit.aes = FALSE
  )

  p <- p +
    ggplot2::labs(
      title = "Tract-station connections (dominant)",
      subtitle = sprintf("%d tracts, %d stations", n, ncol(W))
    ) +
    .map_theme()

  border_layer <- .muni_border_layer(result$code_muni, result$tracts_sf,
                                      result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

  p
}


#' @noRd
.plot_connections_detail <- function(result, W, centroids, station_coords,
                                      tract_indices, top_k, threshold,
                                      show_all_tracts, palette) {
  n_tracts <- length(tract_indices)
  m <- ncol(W)

  # Build segment data frame
  segs <- vector("list", n_tracts)
  for (k in seq_along(tract_indices)) {
    i <- tract_indices[k]
    w <- W[i, ]
    keep <- w >= threshold
    if (!is.null(top_k) && sum(keep) > top_k) {
      ord <- order(w, decreasing = TRUE)
      keep <- rep(FALSE, length(w))
      keep[ord[seq_len(top_k)]] <- TRUE
    }
    if (!any(keep)) next

    idx <- which(keep)
    segs[[k]] <- data.frame(
      cx = centroids[i, 1],
      cy = centroids[i, 2],
      sx = station_coords[idx, 1],
      sy = station_coords[idx, 2],
      weight = w[idx],
      tract = as.character(i),
      stringsAsFactors = FALSE
    )
  }
  seg_df <- do.call(rbind, segs)

  p <- ggplot2::ggplot()

  if (show_all_tracts) {
    p <- p + ggplot2::geom_sf(
      data = result$tracts_sf,
      fill = "grey95", color = "grey80", linewidth = 0.1,
      inherit.aes = FALSE
    )
  }

  # Highlight selected tracts
  sel_sf <- result$tracts_sf[tract_indices, , drop = FALSE]
  p <- p + ggplot2::geom_sf(
    data = sel_sf,
    fill = "#fee08b", color = "#d73027", linewidth = 0.5,
    inherit.aes = FALSE
  )

  if (!is.null(seg_df) && nrow(seg_df) > 0L) {
    if (n_tracts > 1L) {
      p <- p + ggplot2::geom_segment(
        data = seg_df,
        ggplot2::aes(
          x = .data$cx, y = .data$cy,
          xend = .data$sx, yend = .data$sy,
          alpha = .data$weight, linewidth = .data$weight,
          color = .data$tract
        )
      ) +
        ggplot2::scale_linewidth_continuous(range = c(0.3, 2), guide = "none") +
        ggplot2::scale_alpha_continuous(range = c(0.3, 1), guide = "none")
    } else {
      p <- p + ggplot2::geom_segment(
        data = seg_df,
        ggplot2::aes(
          x = .data$cx, y = .data$cy,
          xend = .data$sx, yend = .data$sy,
          alpha = .data$weight, linewidth = .data$weight
        ),
        color = "steelblue"
      ) +
        ggplot2::scale_linewidth_continuous(range = c(0.3, 2), guide = "none") +
        ggplot2::scale_alpha_continuous(range = c(0.3, 1), guide = "none")
    }
  }

  # Station points
  st_df <- data.frame(x = station_coords[, 1], y = station_coords[, 2])
  p <- p + ggplot2::geom_point(
    data = st_df,
    ggplot2::aes(x = .data$x, y = .data$y),
    color = "black", size = 1.2,
    inherit.aes = FALSE
  )

  p <- p +
    ggplot2::labs(
      title = "Tract-station connections",
      subtitle = sprintf("%d tract(s), threshold = %.3f",
                         n_tracts, threshold)
    ) +
    .map_theme()

  border_layer <- .muni_border_layer(result$code_muni, result$tracts_sf,
                                      result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer

  p
}


# --- Internal: shared utilities ---

#' Resolve tract ID or index to row indices
#' @noRd
.resolve_tract_indices <- function(tract, result) {
  if (is.numeric(tract)) {
    idx <- as.integer(tract)
    n <- nrow(.get_weights(result))
    bad <- idx < 1L | idx > n
    if (any(bad)) {
      stop(sprintf("Tract index out of range [1, %d]: %s", n,
                    paste(idx[bad], collapse = ", ")),
           call. = FALSE)
    }
    return(idx)
  }

  # Character: match against tract_id column
  if (!is.null(result$tract_id) && !is.null(result$tracts_sf)) {
    ids <- sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]]
    idx <- match(tract, ids)
    if (any(is.na(idx))) {
      stop("Tract ID(s) not found: ",
           paste(tract[is.na(idx)], collapse = ", "),
           call. = FALSE)
    }
    return(idx)
  }

  stop("Cannot resolve tract ID: no tract_id column in result.", call. = FALSE)
}


#' Generate qualitative colors
#'
#' Returns distinct colors for categorical data. Uses Set3 or Paired
#' palettes when available.
#' @noRd
.qualitative_colors <- function(n, palette = "viridis") {
  if (n <= 12L && requireNamespace("RColorBrewer", quietly = TRUE)) {
    pal <- if (n <= 9L) "Set1" else "Paired"
    return(RColorBrewer::brewer.pal(max(n, 3L), pal)[seq_len(n)])
  }
  # Fall back to hcl-based palette
  grDevices::hcl.colors(n, palette = "Dynamic")
}

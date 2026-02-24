# Travel time matrix diagnostics

#' Plot travel time matrix diagnostics
#'
#' Visualize the distribution and spatial structure of travel times
#' between census tracts and polling stations.
#'
#' @param result An `interpElections_result` object.
#' @param type Character. Plot type: `"histogram"`, `"heatmap"`, or
#'   `"map"`. Default: `"histogram"`.
#' @param tract Tract ID or index for `type = "map"`. NULL picks a
#'   random tract.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly). Prints the plot.
#'
#' @details
#' - `"histogram"`: Distribution of all travel time values. Annotates the
#'   maximum value (potential fill value for unreachable pairs).
#' - `"heatmap"`: Travel time matrix as a tile plot, with tracts ordered
#'   by latitude of their centroid. Reveals spatial locality.
#' - `"map"`: For a selected tract, overlays station points colored by
#'   travel time from that tract.
#'
#' @export
plot_travel_times <- function(result, type = c("histogram", "heatmap", "map"),
                               tract = NULL, ...) {
  type <- match.arg(type)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("The 'ggplot2' package is required.", call. = FALSE)
  if (is.null(result$time_matrix)) {
    message("No time_matrix in result. Cannot plot travel times.")
    return(invisible(NULL))
  }

  tt <- result$time_matrix
  n <- nrow(tt)
  m <- ncol(tt)

  if (type == "histogram") {
    p <- .tt_histogram(tt)
  } else if (type == "heatmap") {
    p <- .tt_heatmap(tt, result)
  } else {
    p <- .tt_map(tt, result, tract)
  }

  if (!is.null(p)) print(p)
  invisible(p)
}


#' @noRd
.tt_histogram <- function(tt) {
  vals <- as.numeric(tt)
  n_total <- length(vals)

  # Count and exclude NAs (unreachable pairs)
  n_na <- sum(is.na(vals))
  pct_na <- n_na / n_total * 100
  vals_plot <- vals[!is.na(vals)]

  if (length(vals_plot) == 0) {
    message("All travel times are NA. Cannot plot histogram.")
    return(NULL)
  }

  max_val <- max(vals_plot)
  n_at_max <- sum(vals_plot >= max_val - 0.1)
  pct_at_max <- n_at_max / n_total * 100

  df <- data.frame(time = vals_plot)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_histogram(bins = 50, fill = "steelblue", color = "white",
                             linewidth = 0.1) +
    ggplot2::geom_vline(xintercept = max_val, linetype = "dashed",
                         color = "red", linewidth = 0.7) +
    ggplot2::annotate("text", x = max_val, y = Inf,
                       label = sprintf("max = %.0f min\n(%d pairs, %.1f%%)",
                                       max_val, n_at_max, pct_at_max),
                       hjust = 1.1, vjust = 1.5, size = 3.5, color = "red")

  sub_text <- sprintf("%d tracts x %d stations = %s pairs",
                      nrow(tt), ncol(tt),
                      format(n_total, big.mark = ","))
  if (n_na > 0) {
    sub_text <- paste0(sub_text, sprintf(
      "\n%d unreachable pairs (%.1f%%) excluded (NA)",
      n_na, pct_na))
  }

  p <- p +
    ggplot2::labs(
      title = "Travel time distribution",
      subtitle = sub_text,
      x = "Travel time (minutes)",
      y = "Count"
    ) +
    ggplot2::theme_minimal()
  p
}


#' @noRd
.tt_heatmap <- function(tt, result) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    message("The 'sf' package is needed for heatmap ordering.")
    return(NULL)
  }

  n <- nrow(tt)
  m <- ncol(tt)

  # Order tracts by latitude of centroid
  centroids <- suppressWarnings(sf::st_centroid(result$tracts_sf))
  coords <- sf::st_coordinates(centroids)
  lat_order <- order(coords[, 2])

  tt_ordered <- tt[lat_order, , drop = FALSE]

  # NAs display as na.value in the color scale

  # Build long data frame
  df <- data.frame(
    tract = rep(seq_len(n), times = m),
    station = rep(seq_len(m), each = n),
    time = as.numeric(tt_ordered)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$station, y = .data$tract, fill = .data$time
  )) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1,
                                   na.value = "grey90") +
    ggplot2::labs(
      title = "Travel time matrix",
      subtitle = "Tracts ordered by latitude (south to north)",
      x = "Station index",
      y = "Tract index (by latitude)",
      fill = "Minutes"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )
  p
}


#' @noRd
.tt_map <- function(tt, result, tract) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    message("The 'sf' package is needed for map mode.")
    return(NULL)
  }
  if (is.null(result$electoral_sf)) {
    message("electoral_sf not available. Re-run with keep = \"electoral_sf\".")
    return(NULL)
  }

  n <- nrow(tt)
  # Resolve tract

  if (is.null(tract)) {
    tract <- sample.int(n, 1L)
    message(sprintf("No tract specified. Showing tract %d.", tract))
  } else if (is.character(tract)) {
    id_col <- result$tract_id
    tract_ids <- sf::st_drop_geometry(result$tracts_sf)[[id_col]]
    idx <- match(tract, as.character(tract_ids))
    if (is.na(idx)) stop("Tract '", tract, "' not found.", call. = FALSE)
    tract <- idx
  }

  times <- tt[tract, ]
  station_sf <- result$electoral_sf
  station_sf$.travel_time <- times

  tract_sf <- result$tracts_sf[tract, ]

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = result$tracts_sf, fill = "grey95",
                      color = "grey80", linewidth = 0.1) +
    ggplot2::geom_sf(data = tract_sf, fill = "gold", color = "black",
                      linewidth = 0.5) +
    ggplot2::geom_sf(data = station_sf,
                      ggplot2::aes(color = .data$.travel_time),
                      size = 2.5) +
    ggplot2::scale_color_viridis_c(option = "plasma", direction = -1,
                                    na.value = "grey50") +
    ggplot2::labs(
      title = sprintf("Travel times from tract %s", tract),
      color = "Minutes"
    ) +
    .map_theme()
  p
}

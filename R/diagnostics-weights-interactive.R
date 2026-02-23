# Interactive weight/connection diagnostics using mapview/leaflet


#' Interactive connections map
#'
#' Tract choropleth colored by dominant station, station markers sized by
#' population served, dominant connection polylines. Rich click popups.
#'
#' @param result An interpElections_result object.
#' @param W Weight matrix.
#' @param centroids Tract centroid coordinates (projected CRS).
#' @param station_coords Station coordinates (projected CRS).
#' @return A mapview object.
#' @noRd
.plot_connections_interactive <- function(result, W, centroids, station_coords) {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("The 'mapview' package is required for interactive connections.\n",
         "Install with: install.packages(\"mapview\")", call. = FALSE)
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The 'leaflet' package is required for interactive connections.\n",
         "Install with: install.packages(\"leaflet\")", call. = FALSE)
  }

  n <- nrow(W)
  m <- ncol(W)
  ws <- weight_summary(result)

  # --- Tract layer: colored by dominant station ---
  plot_sf <- sf::st_transform(result$tracts_sf, 4326)
  dom <- ws$dominant_source

  # Group stations into manageable categories
  unique_dom <- sort(unique(dom))
  n_stations <- length(unique_dom)

  if (n_stations > 20L) {
    freq <- sort(table(dom), decreasing = TRUE)
    top_ids <- as.integer(names(freq)[seq_len(min(19L, length(freq)))])
    labels <- ifelse(dom %in% top_ids, paste0("Station ", dom), "Other")
    plot_sf$.dominant <- factor(labels)
  } else {
    plot_sf$.dominant <- factor(paste0("Station ", dom))
  }

  n_levels <- nlevels(plot_sf$.dominant)
  colors <- .qualitative_colors(n_levels, "viridis")

  # Build tract popup
  tract_popup <- .build_connection_tract_popup(result, W, ws)

  m1 <- suppressWarnings(mapview::mapview(
    plot_sf,
    zcol = ".dominant",
    col.regions = colors,
    alpha.regions = 0.6,
    layer.name = "Dominant station",
    popup = tract_popup,
    map.types = "OpenStreetMap"
  ))

  # --- Station markers ---
  station_sf <- .make_station_sf(result, W, station_coords)
  station_popup <- .build_connection_station_popup(result, W, station_sf)

  # Scale size by total weight
  total_weight <- colSums(W)
  max_tw <- max(total_weight)
  radius <- 4 + 12 * (total_weight / max(max_tw, 1))

  m2 <- suppressWarnings(mapview::mapview(
    station_sf,
    cex = radius,
    col.regions = "black",
    alpha.regions = 0.8,
    color = "white",
    lwd = 1,
    layer.name = "Stations",
    popup = station_popup,
    legend = FALSE,
    map.types = "OpenStreetMap"
  ))

  # --- Connection lines (dominant only, to keep it light) ---
  conn_sf <- .make_connection_lines(result, W, centroids, station_coords)
  if (!is.null(conn_sf)) {
    m3 <- suppressWarnings(mapview::mapview(
      conn_sf,
      color = "steelblue",
      lwd = 1,
      alpha = 0.3,
      layer.name = "Connections",
      legend = FALSE,
      popup = FALSE,
      label = FALSE,
      map.types = "OpenStreetMap"
    ))
    m_out <- m1 + m2 + m3
  } else {
    m_out <- m1 + m2
  }

  # Municipality border
  muni_sf <- .get_muni_sf(result$code_muni,
                            sf::st_crs(result$tracts_sf),
                            muni_boundary = result$muni_boundary)
  if (!is.null(muni_sf)) {
    muni_sf <- sf::st_transform(muni_sf, 4326)
    muni_lines <- sf::st_cast(sf::st_geometry(muni_sf), "MULTILINESTRING")
    muni_lines_sf <- sf::st_sf(geometry = muni_lines)
    muni_map <- suppressWarnings(mapview::mapview(
      muni_lines_sf,
      color = "grey30", lwd = 2,
      legend = FALSE, popup = FALSE, label = FALSE,
      homebutton = FALSE,
      layer.name = "Municipality",
      map.types = "OpenStreetMap"
    ))
    m_out <- m_out + muni_map
  }

  m_out
}


#' Interactive weight map (entropy, dominant, or catchment)
#'
#' @param result An interpElections_result object.
#' @param type Character. "entropy", "dominant", or "catchment".
#' @param tract Tract index for catchment mode.
#' @param top_k Max stations in catchment.
#' @param threshold Min weight for catchment.
#' @param palette Color palette.
#' @return A mapview object.
#' @noRd
.plot_weights_interactive <- function(result, type, tract = NULL,
                                       top_k = 5L, threshold = 0.01,
                                       palette = "YlOrRd") {
  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop("The 'mapview' package is required for interactive weight maps.\n",
         "Install with: install.packages(\"mapview\")", call. = FALSE)
  }

  switch(type,
    entropy   = .plot_weights_entropy_interactive(result, palette),
    dominant  = .plot_weights_dominant_interactive(result, palette),
    catchment = .plot_weights_catchment_interactive(result, tract, top_k,
                                                     threshold, palette)
  )
}


# --- Entropy interactive ---

#' @noRd
.plot_weights_entropy_interactive <- function(result, palette) {
  ws <- weight_summary(result)
  W <- .get_weights(result)
  plot_sf <- sf::st_transform(result$tracts_sf, 4326)
  plot_sf$effective_n_sources <- ws$effective_n_sources

  popup <- .build_weight_summary_popup(result, W, ws)

  pal <- .mapview_palette(palette)

  m <- suppressWarnings(mapview::mapview(
    plot_sf,
    zcol = "effective_n_sources",
    col.regions = pal,
    alpha.regions = 0.7,
    layer.name = "Eff. sources",
    popup = popup,
    map.types = "OpenStreetMap"
  ))

  # Municipality border
  muni_sf <- .get_muni_sf(result$code_muni,
                            sf::st_crs(result$tracts_sf),
                            muni_boundary = result$muni_boundary)
  if (!is.null(muni_sf)) {
    muni_sf <- sf::st_transform(muni_sf, 4326)
    muni_lines <- sf::st_cast(sf::st_geometry(muni_sf), "MULTILINESTRING")
    muni_lines_sf <- sf::st_sf(geometry = muni_lines)
    muni_map <- suppressWarnings(mapview::mapview(
      muni_lines_sf,
      color = "grey30", lwd = 2,
      legend = FALSE, popup = FALSE, label = FALSE,
      homebutton = FALSE, layer.name = "Municipality",
      map.types = "OpenStreetMap"
    ))
    m <- m + muni_map
  }

  m
}


# --- Dominant interactive ---

#' @noRd
.plot_weights_dominant_interactive <- function(result, palette) {
  W <- .get_weights(result)
  ws <- weight_summary(result)
  plot_sf <- sf::st_transform(result$tracts_sf, 4326)
  dom <- ws$dominant_source

  unique_dom <- sort(unique(dom))
  n_stations <- length(unique_dom)

  if (n_stations > 20L) {
    freq <- sort(table(dom), decreasing = TRUE)
    top_ids <- as.integer(names(freq)[seq_len(min(19L, length(freq)))])
    labels <- ifelse(dom %in% top_ids, paste0("Station ", dom), "Other")
    plot_sf$.dominant <- factor(labels)
  } else {
    plot_sf$.dominant <- factor(paste0("Station ", dom))
  }

  n_levels <- nlevels(plot_sf$.dominant)
  colors <- .qualitative_colors(n_levels, palette)

  popup <- .build_weight_summary_popup(result, W, ws)

  m <- suppressWarnings(mapview::mapview(
    plot_sf,
    zcol = ".dominant",
    col.regions = colors,
    alpha.regions = 0.7,
    layer.name = "Dominant station",
    popup = popup,
    map.types = "OpenStreetMap"
  ))

  # Station overlay
  station_coords <- .get_station_coords(result)
  if (!is.null(station_coords)) {
    station_sf <- .make_station_sf(result, W, station_coords)
    station_popup <- .build_connection_station_popup(result, W, station_sf)

    m2 <- suppressWarnings(mapview::mapview(
      station_sf,
      cex = 5,
      col.regions = "black",
      alpha.regions = 0.8,
      color = "white",
      lwd = 1,
      layer.name = "Stations",
      popup = station_popup,
      legend = FALSE,
      map.types = "OpenStreetMap"
    ))
    m <- m + m2
  }

  # Municipality border
  muni_sf <- .get_muni_sf(result$code_muni,
                            sf::st_crs(result$tracts_sf),
                            muni_boundary = result$muni_boundary)
  if (!is.null(muni_sf)) {
    muni_sf <- sf::st_transform(muni_sf, 4326)
    muni_lines <- sf::st_cast(sf::st_geometry(muni_sf), "MULTILINESTRING")
    muni_lines_sf <- sf::st_sf(geometry = muni_lines)
    muni_map <- suppressWarnings(mapview::mapview(
      muni_lines_sf,
      color = "grey30", lwd = 2,
      legend = FALSE, popup = FALSE, label = FALSE,
      homebutton = FALSE, layer.name = "Municipality",
      map.types = "OpenStreetMap"
    ))
    m <- m + muni_map
  }

  m
}


# --- Catchment interactive ---

#' @noRd
.plot_weights_catchment_interactive <- function(result, tract, top_k,
                                                  threshold, palette) {
  W <- .get_weights(result)
  n <- nrow(W)

  # Resolve tract index
  if (is.null(tract)) {
    ws <- weight_summary(result)
    tract_idx <- which.max(ws$effective_n_sources)
  } else {
    tract_idx <- .resolve_tract_indices(tract, result)
    if (length(tract_idx) > 1L) {
      tract_idx <- tract_idx[1L]
      message("Catchment interactive uses first tract only.")
    }
  }

  w <- W[tract_idx, ]
  keep <- w >= threshold
  if (!is.null(top_k) && sum(keep) > top_k) {
    ord <- order(w, decreasing = TRUE)
    keep <- rep(FALSE, length(w))
    keep[ord[seq_len(top_k)]] <- TRUE
  }

  station_coords <- .get_station_coords(result)
  if (is.null(station_coords)) {
    stop("Cannot extract station coordinates.", call. = FALSE)
  }

  # Tract polygon
  tract_sf <- sf::st_transform(result$tracts_sf[tract_idx, , drop = FALSE], 4326)

  # Tract label
  tract_label <- if (!is.null(result$tract_id) && !is.null(result$tracts_sf)) {
    sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]][tract_idx]
  } else {
    tract_idx
  }

  m <- suppressWarnings(mapview::mapview(
    tract_sf,
    col.regions = "#3182bd",
    alpha.regions = 0.4,
    color = "#08519c",
    lwd = 2,
    layer.name = paste0("Tract ", tract_label),
    popup = paste0("<b>Selected tract:</b> ", tract_label),
    map.types = "OpenStreetMap"
  ))

  # Background tracts (light)
  bg_sf <- sf::st_transform(result$tracts_sf, 4326)
  m_bg <- suppressWarnings(mapview::mapview(
    bg_sf,
    col.regions = "grey90",
    alpha.regions = 0.3,
    color = "grey70",
    lwd = 0.5,
    layer.name = "All tracts",
    legend = FALSE,
    popup = FALSE,
    label = FALSE,
    map.types = "OpenStreetMap"
  ))

  if (any(keep)) {
    idx <- which(keep)
    # Transform station points to WGS84
    pts_proj <- sf::st_as_sf(
      data.frame(x = station_coords[idx, 1], y = station_coords[idx, 2]),
      coords = c("x", "y"),
      crs = sf::st_crs(result$tracts_sf)
    )
    pts_wgs <- sf::st_transform(pts_proj, 4326)
    pts_wgs$weight <- w[idx]
    pts_wgs$station <- idx

    # Travel time info
    tt <- result$time_matrix
    if (!is.null(tt)) {
      pts_wgs$travel_time <- tt[tract_idx, idx]
    } else {
      pts_wgs$travel_time <- NA_real_
    }

    # Popup for stations
    popup_html <- vapply(seq_len(nrow(pts_wgs)), function(j) {
      sprintf(
        paste0(
          '<div style="font-family:sans-serif;font-size:12px">',
          '<b>Station %d</b><br/>',
          'Weight: <b>%.4f</b><br/>',
          'Travel time: <b>%s min</b>',
          '</div>'
        ),
        pts_wgs$station[j],
        pts_wgs$weight[j],
        if (is.na(pts_wgs$travel_time[j])) "&mdash;"
        else sprintf("%.0f", pts_wgs$travel_time[j])
      )
    }, character(1))

    # Size by weight
    radius <- 4 + 12 * (pts_wgs$weight / max(pts_wgs$weight))

    pal <- .mapview_palette(palette)

    m_stations <- suppressWarnings(mapview::mapview(
      pts_wgs,
      zcol = "weight",
      cex = radius,
      col.regions = pal,
      alpha.regions = 0.9,
      layer.name = "Connected stations",
      popup = popup_html,
      legend = TRUE,
      map.types = "OpenStreetMap"
    ))
    m <- m_bg + m + m_stations
  } else {
    m <- m_bg + m
  }

  m
}


# --- Helpers for building popup HTML ---

#' Build HTML popup for tracts in connection map
#' @noRd
.build_connection_tract_popup <- function(result, W, ws) {
  n <- nrow(W)
  m <- ncol(W)
  tt <- result$time_matrix

  # Tract IDs
  tract_ids <- if (!is.null(result$tract_id) && !is.null(result$tracts_sf)) {
    sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]]
  } else {
    seq_len(n)
  }

  css <- paste0(
    '<style>',
    '.wt-popup { font-family: sans-serif; font-size: 12px; ',
    'line-height: 1.4; max-width: 340px; }',
    '.wt-popup h4 { margin: 0 0 4px; font-size: 13px; ',
    'border-bottom: 1px solid #ccc; padding-bottom: 3px; }',
    '.wt-popup table { border-collapse: collapse; width: 100%; ',
    'font-size: 11px; margin-top: 4px; }',
    '.wt-popup th { background: #f0f0f0; text-align: left; ',
    'padding: 2px 4px; }',
    '.wt-popup td { padding: 2px 4px; border-top: 1px solid #eee; }',
    '.wt-popup .wt-stats { margin-top: 6px; font-size: 11px; }',
    '.wt-popup .wt-row { display: flex; justify-content: space-between; }',
    '.wt-popup .wt-label { color: #666; }',
    '.wt-popup .wt-val { font-weight: 600; }',
    '</style>'
  )

  vapply(seq_len(n), function(i) {
    w <- W[i, ]
    ord <- order(w, decreasing = TRUE)
    top5 <- ord[seq_len(min(5L, m))]

    # Row-normalize for measures
    rs <- sum(w)
    p <- if (rs > 0) w / rs else w

    # Top-5 table
    cum_w <- cumsum(w[top5])
    rows <- vapply(seq_along(top5), function(j) {
      tt_val <- if (!is.null(tt)) sprintf("%.0f", tt[i, top5[j]]) else "&mdash;"
      sprintf("<tr><td>%d</td><td>%.4f</td><td>%s</td><td>%.3f</td></tr>",
              top5[j], w[top5[j]], tt_val, cum_w[j])
    }, character(1))

    table_html <- paste0(
      '<table>',
      '<tr><th>Station</th><th>Weight</th><th>Time (min)</th><th>Cum.</th></tr>',
      paste(rows, collapse = ""),
      '</table>'
    )

    html <- paste0(
      css, '<div class="wt-popup">',
      '<h4>Tract: ', tract_ids[i], '</h4>',
      '<div><b>Top connections:</b></div>',
      table_html,
      '<div class="wt-stats">',
      sprintf('<div class="wt-row"><span class="wt-label">Dominant station:</span><span class="wt-val">%d (%.1f%%)</span></div>',
              ws$dominant_source[i], ws$dominant_weight[i] * 100),
      sprintf('<div class="wt-row"><span class="wt-label">Top-3 weight:</span><span class="wt-val">%.1f%%</span></div>',
              ws$top3_weight[i] * 100),
      sprintf('<div class="wt-row"><span class="wt-label">Eff. sources:</span><span class="wt-val">%.1f</span></div>',
              ws$effective_n_sources[i]),
      sprintf('<div class="wt-row"><span class="wt-label">Herfindahl:</span><span class="wt-val">%.4f</span></div>',
              ws$herfindahl[i]),
      '</div></div>'
    )
    html
  }, character(1))
}


#' Build HTML popup for stations in connection map
#' @noRd
.build_connection_station_popup <- function(result, W, station_sf) {
  n <- nrow(W)
  m <- ncol(W)
  tt <- result$time_matrix

  # Tract IDs
  tract_ids <- if (!is.null(result$tract_id) && !is.null(result$tracts_sf)) {
    sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]]
  } else {
    seq_len(n)
  }

  dom <- apply(W, 1, which.max)

  css <- paste0(
    '<style>',
    '.st-popup { font-family: sans-serif; font-size: 12px; ',
    'line-height: 1.4; max-width: 340px; }',
    '.st-popup h4 { margin: 0 0 4px; font-size: 13px; ',
    'border-bottom: 1px solid #ccc; padding-bottom: 3px; }',
    '.st-popup table { border-collapse: collapse; width: 100%; ',
    'font-size: 11px; margin-top: 4px; }',
    '.st-popup th { background: #f0f0f0; text-align: left; ',
    'padding: 2px 4px; }',
    '.st-popup td { padding: 2px 4px; border-top: 1px solid #eee; }',
    '.st-popup .st-stat { margin-top: 4px; }',
    '</style>'
  )

  vapply(seq_len(m), function(j) {
    w_col <- W[, j]
    n_dominant <- sum(dom == j)
    total_weight <- sum(w_col)

    # Top-10 served tracts
    ord <- order(w_col, decreasing = TRUE)
    top10 <- ord[seq_len(min(10L, n))]
    top10 <- top10[w_col[top10] > 0]

    rows <- vapply(seq_along(top10), function(k) {
      tt_val <- if (!is.null(tt)) sprintf("%.0f", tt[top10[k], j]) else "&mdash;"
      sprintf("<tr><td>%s</td><td>%.4f</td><td>%s</td></tr>",
              tract_ids[top10[k]], w_col[top10[k]], tt_val)
    }, character(1))

    table_html <- paste0(
      '<table>',
      '<tr><th>Tract</th><th>Weight</th><th>Time (min)</th></tr>',
      paste(rows, collapse = ""),
      '</table>'
    )

    html <- paste0(
      css, '<div class="st-popup">',
      '<h4>Station ', j, '</h4>',
      sprintf('<div class="st-stat">Dominant for <b>%d</b> tract(s)</div>', n_dominant),
      sprintf('<div class="st-stat">Total weight: <b>%.2f</b></div>', total_weight),
      '<div style="margin-top:4px"><b>Top served tracts:</b></div>',
      table_html,
      '</div>'
    )
    html
  }, character(1))
}


#' Build weight summary popup (for entropy/dominant interactive)
#' @noRd
.build_weight_summary_popup <- function(result, W, ws) {
  n <- nrow(W)
  tt <- result$time_matrix

  tract_ids <- if (!is.null(result$tract_id) && !is.null(result$tracts_sf)) {
    sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]]
  } else {
    seq_len(n)
  }

  css <- paste0(
    '<style>',
    '.ws-popup { font-family: sans-serif; font-size: 12px; ',
    'line-height: 1.4; max-width: 300px; }',
    '.ws-popup h4 { margin: 0 0 4px; font-size: 13px; ',
    'border-bottom: 1px solid #ccc; padding-bottom: 3px; }',
    '.ws-popup .ws-row { display: flex; justify-content: space-between; }',
    '.ws-popup .ws-label { color: #666; }',
    '.ws-popup .ws-val { font-weight: 600; }',
    '</style>'
  )

  vapply(seq_len(n), function(i) {
    tt_val <- if (!is.null(tt) && !is.na(ws$mean_travel_time_weighted[i])) {
      sprintf("%.0f min", ws$mean_travel_time_weighted[i])
    } else {
      "&mdash;"
    }

    html <- paste0(
      css, '<div class="ws-popup">',
      '<h4>Tract: ', tract_ids[i], '</h4>',
      sprintf('<div class="ws-row"><span class="ws-label">Eff. sources:</span><span class="ws-val">%.1f</span></div>',
              ws$effective_n_sources[i]),
      sprintf('<div class="ws-row"><span class="ws-label">Dominant station:</span><span class="ws-val">%d (%.1f%%)</span></div>',
              ws$dominant_source[i], ws$dominant_weight[i] * 100),
      sprintf('<div class="ws-row"><span class="ws-label">Top-3 weight:</span><span class="ws-val">%.1f%%</span></div>',
              ws$top3_weight[i] * 100),
      sprintf('<div class="ws-row"><span class="ws-label">Herfindahl:</span><span class="ws-val">%.4f</span></div>',
              ws$herfindahl[i]),
      sprintf('<div class="ws-row"><span class="ws-label">Mean travel time:</span><span class="ws-val">%s</span></div>',
              tt_val),
      '</div>'
    )
    html
  }, character(1))
}


# --- Spatial helpers ---

#' Create sf point object for stations
#' @noRd
.make_station_sf <- function(result, W, station_coords) {
  station_pts <- sf::st_as_sf(
    data.frame(
      station = seq_len(ncol(W)),
      x = station_coords[, 1],
      y = station_coords[, 2]
    ),
    coords = c("x", "y"),
    crs = sf::st_crs(result$tracts_sf)
  )
  sf::st_transform(station_pts, 4326)
}


#' Create linestring sf for dominant connections
#' @noRd
.make_connection_lines <- function(result, W, centroids, station_coords) {
  n <- nrow(W)
  dom <- apply(W, 1, which.max)

  # Build lines in the tracts CRS, then transform
  lines <- lapply(seq_len(n), function(i) {
    sf::st_linestring(matrix(
      c(centroids[i, 1], centroids[i, 2],
        station_coords[dom[i], 1], station_coords[dom[i], 2]),
      ncol = 2, byrow = TRUE
    ))
  })

  conn_sf <- sf::st_sf(
    tract = seq_len(n),
    geometry = sf::st_sfc(lines, crs = sf::st_crs(result$tracts_sf))
  )
  sf::st_transform(conn_sf, 4326)
}

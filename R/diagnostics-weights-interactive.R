# Interactive weight/connection diagnostics using mapview/leaflet


#' Interactive connections map with click-to-highlight
#'
#' Built with raw leaflet for full control over layerId and JavaScript
#' event handling. Clicking a station shows its connections to tracts;
#' clicking a tract shows its connections to stations. Line width is
#' proportional to weight.
#'
#' @param result An interpElections_result object.
#' @param W Weight matrix.
#' @param centroids Tract centroid coordinates (projected CRS).
#' @param station_coords Station coordinates (projected CRS).
#' @param top_k Max connections per tract (NULL = all above threshold).
#' @param threshold Min weight for a connection.
#' @param show_all_tracts Logical. Show all tract polygons.
#' @param palette Color palette for tract fill.
#' @return A leaflet htmlwidget.
#' @noRd
.plot_connections_interactive <- function(result, W, centroids, station_coords,
                                           top_k, threshold, show_all_tracts,
                                           palette) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("The 'leaflet' package is required for interactive connections.\n",
         "Install with: install.packages(\"leaflet\")", call. = FALSE)
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("The 'htmlwidgets' package is required for interactive connections.\n",
         "Install with: install.packages(\"htmlwidgets\")", call. = FALSE)
  }

  n <- nrow(W)
  m <- ncol(W)
  ws <- weight_summary(result)
  tt <- result$time_matrix
  station_ids <- .get_station_ids(result)
  tract_ids <- .get_tract_ids(result)

  # --- Transform geometries to WGS84 ---
  tracts_wgs <- sf::st_transform(result$tracts_sf, 4326)

  # Station points in WGS84
  station_pts <- sf::st_as_sf(
    data.frame(x = station_coords[, 1], y = station_coords[, 2]),
    coords = c("x", "y"),
    crs = sf::st_crs(result$tracts_sf)
  )
  station_wgs <- sf::st_transform(station_pts, 4326)
  station_wgs_coords <- sf::st_coordinates(station_wgs)

  # Tract centroids in WGS84
  centroid_pts <- sf::st_as_sf(
    data.frame(x = centroids[, 1], y = centroids[, 2]),
    coords = c("x", "y"),
    crs = sf::st_crs(result$tracts_sf)
  )
  centroid_wgs <- sf::st_transform(centroid_pts, 4326)
  centroid_wgs_coords <- sf::st_coordinates(centroid_wgs)

  # --- Build connection data (filtered by threshold/top_k) ---
  conn_list <- vector("list", n)
  for (i in seq_len(n)) {
    w <- W[i, ]
    keep <- w >= threshold
    if (!is.null(top_k) && sum(keep) > top_k) {
      ord <- order(w, decreasing = TRUE)
      keep <- rep(FALSE, m)
      keep[ord[seq_len(top_k)]] <- TRUE
    }
    if (!any(keep)) next
    idx <- which(keep)
    conn_list[[i]] <- data.frame(
      tract_id = tract_ids[i],
      station_id = station_ids[idx],
      tract_lat = centroid_wgs_coords[i, 2],
      tract_lng = centroid_wgs_coords[i, 1],
      station_lat = station_wgs_coords[idx, 2],
      station_lng = station_wgs_coords[idx, 1],
      weight = w[idx],
      travel_time = if (!is.null(tt)) tt[i, idx] else NA_real_,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  conn_df <- do.call(rbind, conn_list)

  # Safety cap: if too many connections, auto-reduce
  if (!is.null(conn_df) && nrow(conn_df) > 30000L) {
    message(sprintf(
      "Too many connections (%d). Auto-reducing to top_k = 3 per tract.",
      nrow(conn_df)
    ))
    conn_list2 <- vector("list", n)
    for (i in seq_len(n)) {
      w <- W[i, ]
      ord <- order(w, decreasing = TRUE)
      keep <- rep(FALSE, m)
      keep[ord[seq_len(min(3L, m))]] <- TRUE
      keep <- keep & (w >= threshold)
      if (!any(keep)) next
      idx <- which(keep)
      conn_list2[[i]] <- data.frame(
        tract_id = tract_ids[i],
        station_id = station_ids[idx],
        tract_lat = centroid_wgs_coords[i, 2],
        tract_lng = centroid_wgs_coords[i, 1],
        station_lat = station_wgs_coords[idx, 2],
        station_lng = station_wgs_coords[idx, 1],
        weight = w[idx],
        travel_time = if (!is.null(tt)) tt[i, idx] else NA_real_,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
    conn_df <- do.call(rbind, conn_list2)
  }

  conn_json <- if (!is.null(conn_df) && nrow(conn_df) > 0L) {
    # Build JSON manually to avoid jsonlite dependency
    rows <- vapply(seq_len(nrow(conn_df)), function(r) {
      tt_str <- if (is.na(conn_df$travel_time[r])) "null"
                else sprintf("%.1f", conn_df$travel_time[r])
      sprintf(
        paste0('{\"tract_id\":\"%s\",\"station_id\":\"%s\",',
               '\"tract_lat\":%.6f,\"tract_lng\":%.6f,',
               '\"station_lat\":%.6f,\"station_lng\":%.6f,',
               '\"weight\":%.6f,\"travel_time\":%s}'),
        gsub('"', '\\\\"', conn_df$tract_id[r]),
        gsub('"', '\\\\"', conn_df$station_id[r]),
        conn_df$tract_lat[r], conn_df$tract_lng[r],
        conn_df$station_lat[r], conn_df$station_lng[r],
        conn_df$weight[r], tt_str
      )
    }, character(1))
    paste0("[", paste(rows, collapse = ","), "]")
  } else {
    "[]"
  }

  # --- Tract fill color: effective number of sources ---
  eff_n <- ws$effective_n_sources
  pal <- leaflet::colorNumeric(
    palette = palette, domain = eff_n, na.color = "#cccccc"
  )

  # --- Tract popups ---
  tract_popup <- .build_connection_tract_popup(result, W, ws)

  # --- Station popups ---
  station_popup <- .build_connection_station_popup(result, W)

  # Scale station radius by total weight
  total_weight <- colSums(W)
  max_tw <- max(total_weight, 1)
  station_radius <- 4 + 12 * (total_weight / max_tw)

  # --- Build leaflet map ---
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap)

  # Tract polygons
  if (show_all_tracts) {
    map <- map |>
      leaflet::addPolygons(
        data = tracts_wgs,
        layerId = tract_ids,
        fillColor = pal(eff_n),
        fillOpacity = 0.6,
        color = "white",
        weight = 0.5,
        opacity = 1,
        popup = tract_popup,
        label = tract_ids,
        group = "Tracts"
      )
  }

  # Station markers
  map <- map |>
    leaflet::addCircleMarkers(
      lng = station_wgs_coords[, 1],
      lat = station_wgs_coords[, 2],
      layerId = station_ids,
      radius = station_radius,
      fillColor = "black",
      fillOpacity = 0.8,
      color = "white",
      weight = 1,
      popup = station_popup,
      label = station_ids,
      group = "Stations"
    )

  # Legend for tract fill
  map <- map |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = eff_n,
      title = "Eff. sources",
      opacity = 0.7
    )

  # Municipality border
  muni_sf <- .get_muni_sf(result$code_muni,
                            sf::st_crs(result$tracts_sf),
                            muni_boundary = result$muni_boundary)
  if (!is.null(muni_sf)) {
    muni_wgs <- sf::st_transform(muni_sf, 4326)
    map <- map |>
      leaflet::addPolylines(
        data = muni_wgs,
        color = "grey30", weight = 2, opacity = 0.8,
        group = "Municipality"
      )
  }

  # Layer control
  overlays <- c("Tracts", "Stations")
  if (!is.null(muni_sf)) overlays <- c(overlays, "Municipality")
  map <- map |>
    leaflet::addLayersControl(
      overlayGroups = overlays,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  # --- JavaScript click-to-highlight handler ---
  js_code <- sprintf('
function(el, x) {
  var map = this;
  var connData = %s;
  var connGroup = L.layerGroup().addTo(map);
  var activeId = null;

  function clearConns() {
    connGroup.clearLayers();
    activeId = null;
  }

  function showConns(filterFn, color) {
    clearConns();
    var matches = connData.filter(filterFn);
    if (matches.length === 0) return;
    var maxW = Math.max.apply(null, matches.map(function(c){ return c.weight; }));
    matches.forEach(function(c) {
      var lw = 2 + 8 * (c.weight / Math.max(maxW, 1e-6));
      var ttStr = (c.travel_time !== null) ? Math.round(c.travel_time) + " min" : "";
      var line = L.polyline(
        [[c.tract_lat, c.tract_lng], [c.station_lat, c.station_lng]],
        {color: color, weight: lw, opacity: 0.8}
      );
      line.bindPopup(
        "<b>" + c.tract_id + "</b> \\u2194 <b>" + c.station_id + "</b><br>" +
        "Weight: " + c.weight.toFixed(4) + "<br>" +
        (ttStr ? "Travel: " + ttStr : "")
      );
      connGroup.addLayer(line);
    });
  }

  var skipMapClick = false;

  map.on("popupopen", function(e) {
    skipMapClick = true;
    var src = e.popup._source;
    if (!src || !src.options || !src.options.layerId) return;
    var id = String(src.options.layerId);
    if (src instanceof L.CircleMarker) {
      activeId = id;
      showConns(function(c){ return c.station_id === id; }, "#2166ac");
    } else if (src instanceof L.Path) {
      activeId = id;
      showConns(function(c){ return c.tract_id === id; }, "#b2182b");
    }
  });

  map.on("click", function(e) {
    if (skipMapClick) { skipMapClick = false; return; }
    clearConns();
    map.closePopup();
  });
}
', conn_json)

  map <- htmlwidgets::onRender(map, js_code)
  map
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
    station_popup <- .build_connection_station_popup(result, W)

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
    sids <- .get_station_ids(result)
    pts_wgs$weight <- w[idx]
    pts_wgs$station_id <- sids[idx]

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
          '<b>Station: %s</b><br/>',
          'Weight: <b>%.4f</b><br/>',
          'Travel time: <b>%s min</b>',
          '</div>'
        ),
        pts_wgs$station_id[j],
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
  tract_ids <- .get_tract_ids(result)
  station_ids <- .get_station_ids(result)

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
    '.wt-popup .wt-row { display: flex; justify-content: ',
    'space-between; }',
    '.wt-popup .wt-label { color: #666; }',
    '.wt-popup .wt-val { font-weight: 600; }',
    '</style>'
  )

  vapply(seq_len(n), function(i) {
    w <- W[i, ]
    ord <- order(w, decreasing = TRUE)
    top5 <- ord[seq_len(min(5L, m))]

    # Top-5 table
    cum_w <- cumsum(w[top5])
    rows <- vapply(seq_along(top5), function(j) {
      tt_val <- if (!is.null(tt)) {
        sprintf("%.0f", tt[i, top5[j]])
      } else {
        "&mdash;"
      }
      sprintf(
        "<tr><td>%s</td><td>%.4f</td><td>%s</td><td>%.3f</td></tr>",
        station_ids[top5[j]], w[top5[j]], tt_val, cum_w[j]
      )
    }, character(1))

    table_html <- paste0(
      '<table>',
      '<tr><th>Station ID</th><th>Weight</th>',
      '<th>Time (min)</th><th>Cum.</th></tr>',
      paste(rows, collapse = ""),
      '</table>'
    )

    # Mean weighted travel time
    tt_line <- if (!is.null(tt)) {
      mean_tt <- sum(w * tt[i, ]) / max(sum(w), 1e-10)
      sprintf(
        paste0('<div class="wt-row">',
               '<span class="wt-label">Mean travel time:</span>',
               '<span class="wt-val">%.0f min</span></div>'),
        mean_tt
      )
    } else {
      ""
    }

    dom_id <- station_ids[ws$dominant_source[i]]
    html <- paste0(
      css, '<div class="wt-popup">',
      '<h4>Tract: ', tract_ids[i], '</h4>',
      '<div><b>Top connections:</b></div>',
      table_html,
      '<div class="wt-stats">',
      sprintf(
        paste0('<div class="wt-row">',
               '<span class="wt-label">Dominant:</span>',
               '<span class="wt-val">%s (%.1f%%)</span></div>'),
        dom_id, ws$dominant_weight[i] * 100
      ),
      sprintf(
        paste0('<div class="wt-row">',
               '<span class="wt-label">Top-3 weight:</span>',
               '<span class="wt-val">%.1f%%</span></div>'),
        ws$top3_weight[i] * 100
      ),
      sprintf(
        paste0('<div class="wt-row">',
               '<span class="wt-label">Eff. sources:</span>',
               '<span class="wt-val">%.1f</span></div>'),
        ws$effective_n_sources[i]
      ),
      sprintf(
        paste0('<div class="wt-row">',
               '<span class="wt-label">Herfindahl:</span>',
               '<span class="wt-val">%.4f</span></div>'),
        ws$herfindahl[i]
      ),
      tt_line,
      '</div>',
      '<div style="margin-top:6px;font-size:10px;color:#999">',
      'Click to show connections</div>',
      '</div>'
    )
    html
  }, character(1))
}


#' Build HTML popup for stations in connection map
#' @noRd
.build_connection_station_popup <- function(result, W) {
  n <- nrow(W)
  m <- ncol(W)
  tt <- result$time_matrix
  tract_ids <- .get_tract_ids(result)
  station_ids <- .get_station_ids(result)

  dom <- apply(W, 1, which.max)
  total_all <- sum(colSums(W))

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
    weight_share <- if (total_all > 0) {
      total_weight / total_all * 100
    } else {
      0
    }

    # Mean travel time to connected tracts
    mean_tt <- if (!is.null(tt)) {
      connected <- w_col > 0
      if (any(connected)) {
        sum(w_col[connected] * tt[connected, j]) /
          max(sum(w_col[connected]), 1e-10)
      } else {
        NA_real_
      }
    } else {
      NA_real_
    }

    # Top-10 served tracts
    ord <- order(w_col, decreasing = TRUE)
    top10 <- ord[seq_len(min(10L, n))]
    top10 <- top10[w_col[top10] > 0]

    rows <- vapply(seq_along(top10), function(k) {
      tt_val <- if (!is.null(tt)) {
        sprintf("%.0f", tt[top10[k], j])
      } else {
        "&mdash;"
      }
      sprintf(
        "<tr><td>%s</td><td>%.4f</td><td>%s</td></tr>",
        tract_ids[top10[k]], w_col[top10[k]], tt_val
      )
    }, character(1))

    table_html <- paste0(
      '<table>',
      '<tr><th>Tract</th><th>Weight</th><th>Time (min)</th></tr>',
      paste(rows, collapse = ""),
      '</table>'
    )

    tt_line <- if (!is.na(mean_tt)) {
      sprintf(
        '<div class="st-stat">Mean travel time: <b>%.0f min</b></div>',
        mean_tt
      )
    } else {
      ""
    }

    html <- paste0(
      css, '<div class="st-popup">',
      '<h4>Station: ', station_ids[j], '</h4>',
      sprintf(
        '<div class="st-stat">Dominant for <b>%d</b> tract(s)</div>',
        n_dominant
      ),
      sprintf(
        '<div class="st-stat">Total weight: <b>%.2f</b> (%.1f%%)</div>',
        total_weight, weight_share
      ),
      tt_line,
      '<div style="margin-top:4px"><b>Top served tracts:</b></div>',
      table_html,
      '<div style="margin-top:6px;font-size:10px;color:#999">',
      'Click to show connections</div>',
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
  tract_ids <- .get_tract_ids(result)
  station_ids <- .get_station_ids(result)

  css <- paste0(
    '<style>',
    '.ws-popup { font-family: sans-serif; font-size: 12px; ',
    'line-height: 1.4; max-width: 300px; }',
    '.ws-popup h4 { margin: 0 0 4px; font-size: 13px; ',
    'border-bottom: 1px solid #ccc; padding-bottom: 3px; }',
    '.ws-popup .ws-row { display: flex; justify-content: ',
    'space-between; }',
    '.ws-popup .ws-label { color: #666; }',
    '.ws-popup .ws-val { font-weight: 600; }',
    '</style>'
  )

  vapply(seq_len(n), function(i) {
    tt_val <- if (!is.null(tt) &&
                  !is.na(ws$mean_travel_time_weighted[i])) {
      sprintf("%.0f min", ws$mean_travel_time_weighted[i])
    } else {
      "&mdash;"
    }

    dom_id <- station_ids[ws$dominant_source[i]]
    html <- paste0(
      css, '<div class="ws-popup">',
      '<h4>Tract: ', tract_ids[i], '</h4>',
      sprintf(
        paste0('<div class="ws-row">',
               '<span class="ws-label">Eff. sources:</span>',
               '<span class="ws-val">%.1f</span></div>'),
        ws$effective_n_sources[i]
      ),
      sprintf(
        paste0('<div class="ws-row">',
               '<span class="ws-label">Dominant:</span>',
               '<span class="ws-val">%s (%.1f%%)</span></div>'),
        dom_id, ws$dominant_weight[i] * 100
      ),
      sprintf(
        paste0('<div class="ws-row">',
               '<span class="ws-label">Top-3 weight:</span>',
               '<span class="ws-val">%.1f%%</span></div>'),
        ws$top3_weight[i] * 100
      ),
      sprintf(
        paste0('<div class="ws-row">',
               '<span class="ws-label">Herfindahl:</span>',
               '<span class="ws-val">%.4f</span></div>'),
        ws$herfindahl[i]
      ),
      sprintf(
        paste0('<div class="ws-row">',
               '<span class="ws-label">Mean travel time:</span>',
               '<span class="ws-val">%s</span></div>'),
        tt_val
      ),
      '</div>'
    )
    html
  }, character(1))
}


# --- Spatial helpers ---

#' Create sf point object for stations
#' @noRd
.make_station_sf <- function(result, W, station_coords) {
  sids <- .get_station_ids(result)
  station_pts <- sf::st_as_sf(
    data.frame(
      station = seq_len(ncol(W)),
      station_id = sids,
      x = station_coords[, 1],
      y = station_coords[, 2],
      stringsAsFactors = FALSE
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

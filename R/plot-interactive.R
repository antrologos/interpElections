# Interactive map visualization using mapview


#' Interactive map of interpolated results
#'
#' Creates an interactive leaflet/mapview map of an interpolated variable.
#' Variables can be referenced by column name, ballot number, candidate
#' name, or party abbreviation. When multiple variables are provided,
#' creates synchronized side-by-side panels via `leafsync::sync()`.
#'
#' @param result An `interpElections_result` object.
#' @param variable Variable to map. Accepts a column name, ballot number
#'   (numeric), candidate name substring, or party abbreviation.
#'   Multiple values create synchronized comparison panels.
#'   If NULL, auto-selects the first candidate variable.
#' @param type Quantity to map: `"pct_tract"` (default), `"absolute"`,
#'   `"pct_muni"`, `"pct_valid"`, `"pct_eligible"`, `"density"`.
#' @param palette Color palette name. Default: `"RdYlBu"`
#'   (diverging, colorblind-friendly).
#' @param breaks Scale breaks: `"quantile"` (default), `"continuous"`,
#'   `"jenks"` (requires classInt), or a numeric vector of custom
#'   break points.
#' @param n_breaks Number of breaks for `"quantile"` or `"jenks"`.
#'   Default: 5.
#' @param popup_vars Character vector of column names to show in
#'   click popups. If NULL, auto-selects census tract ID, the plotted variable,
#'   turnout, and calibration columns (capped at 8).
#' @param alpha Fill opacity (0 to 1). Default: 0.7.
#' @param legend Show legend. Default: TRUE.
#' @param layer_name Layer name in the map. NULL = auto-generated
#'   from dictionary metadata.
#' @param basemap Base map tile provider. Default: `"OpenStreetMap"`.
#' @param ... Additional arguments passed to [mapview::mapview()].
#'
#' @return A `mapview` object (single variable) or a `leafsync`
#'   object (multiple variables with synchronized panels).
#'
#' @examples
#' \dontrun{
#' result <- interpolate_election_br(3304557, 2020, 2022,
#'                                    keep = "sources_sf")
#'
#' # Interactive map by candidate name
#' plot_interactive(result, variable = "Lula", type = "pct_tract")
#'
#' # Side-by-side comparison (synced panels)
#' plot_interactive(result, variable = c("Lula", "Bolsonaro"),
#'                  type = "pct_tract")
#'
#' # Custom popup
#' plot_interactive(result, variable = "PT",
#'                  popup_vars = c("code_tract", "PARTY_PT",
#'                                 "QT_COMPARECIMENTO"))
#' }
#'
#' @export
plot_interactive <- function(
    result, variable = NULL, type = "pct_tract",
    palette = "RdYlBu",
    breaks = "quantile", n_breaks = 5L,
    popup_vars = NULL,
    alpha = 0.7,
    legend = TRUE,
    layer_name = NULL,
    basemap = "OpenStreetMap",
    ...) {

  if (!requireNamespace("mapview", quietly = TRUE)) {
    stop(
      "The 'mapview' package is required for plot_interactive().\n",
      "Install with: install.packages(\"mapview\")",
      call. = FALSE
    )
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for plot_interactive().", call. = FALSE)
  }
  if (is.null(result$tracts_sf)) {
    stop("No tracts_sf in result object.", call. = FALSE)
  }

  # Resolve variable(s)
  if (is.null(variable)) variable <- .auto_select_var(result)

  # Multi-variable: synchronized panels
  if (length(variable) > 1L) {
    return(.interactive_sync(result, variable, type, palette,
                              breaks, n_breaks,
                              popup_vars, alpha, legend, basemap, ...))
  }

  col <- tryCatch(.resolve_var(variable, result), error = function(e) {
    message(conditionMessage(e))
    NULL
  })
  if (is.null(col)) return(invisible(NULL))
  values <- .compute_quantity(result, col, type)

  # Build sf for display
  plot_sf <- result$tracts_sf
  display_name <- .auto_title(col, result)

  # Compute breaks and optionally bin values (same logic as plot())
  brk <- .compute_breaks(values, breaks, n_breaks)
  if (!is.null(brk)) {
    plot_sf[[display_name]] <- .cut_values(values, brk, type)
    pal <- .bin_colors(palette, length(brk) - 1L, direction = -1)
  } else {
    plot_sf[[display_name]] <- values
    pal <- .mapview_palette(palette)
  }

  # Build popup
  if (!is.null(popup_vars)) {
    popup_cols <- intersect(popup_vars, names(plot_sf))
    popup <- .build_popup(plot_sf, popup_cols)
  } else {
    popup <- .build_detail_popup(result, col, display_name, plot_sf)
  }

  # Layer name
  if (is.null(layer_name)) {
    qty_suffix <- if (type != "absolute") paste0(" (", .quantity_label(type), ")") else ""
    layer_name <- paste0(display_name, qty_suffix)
  }

  m <- suppressWarnings(mapview::mapview(
    plot_sf,
    zcol = display_name,
    layer.name = layer_name,
    col.regions = pal,
    alpha.regions = alpha,
    legend = legend,
    popup = popup,
    map.types = basemap,
    ...
  ))

  # Municipality contour overlay (lines only, non-interactive)
  muni_sf <- .get_muni_sf(result$code_muni, sf::st_crs(plot_sf))
  if (!is.null(muni_sf)) {
    muni_lines <- sf::st_cast(sf::st_geometry(muni_sf), "MULTILINESTRING")
    muni_lines_sf <- sf::st_sf(geometry = muni_lines)
    muni_map <- suppressWarnings(mapview::mapview(
      muni_lines_sf,
      color = "grey30", lwd = 2,
      legend = FALSE,
      popup = FALSE,
      label = FALSE,
      homebutton = FALSE,
      layer.name = "Municipality",
      map.types = basemap
    ))
    m <- m + muni_map
  }

  m
}


# --- Synchronized multi-variable panels ---

#' @noRd
.interactive_sync <- function(
    result, variables, type, palette,
    breaks, n_breaks,
    popup_vars, alpha, legend, basemap, ...) {

  if (!requireNamespace("leafsync", quietly = TRUE)) {
    stop(
      "The 'leafsync' package is required for multi-variable interactive maps.\n",
      "Install with: install.packages(\"leafsync\")",
      call. = FALSE
    )
  }

  cols <- .resolve_vars(variables, result)

  # Compute all values first for shared breaks
  all_values <- lapply(cols, function(col) {
    .compute_quantity(result, col, type)
  })

  # Compute breaks across ALL variables for a shared scale
  combined <- unlist(all_values)
  brk <- .compute_breaks(combined, breaks, n_breaks)

  # Municipality contour (shared across panels)
  muni_sf <- .get_muni_sf(result$code_muni, sf::st_crs(result$tracts_sf))

  maps <- lapply(seq_along(cols), function(i) {
    col <- cols[i]
    values <- all_values[[i]]
    plot_sf <- result$tracts_sf
    display_name <- .auto_title(col, result)

    if (!is.null(brk)) {
      plot_sf[[display_name]] <- .cut_values(values, brk, type)
      pal <- .bin_colors(palette, length(brk) - 1L, direction = -1)
    } else {
      plot_sf[[display_name]] <- values
      pal <- .mapview_palette(palette)
    }

    if (!is.null(popup_vars)) {
      popup_cols_i <- intersect(popup_vars, names(plot_sf))
      popup <- .build_popup(plot_sf, popup_cols_i)
    } else {
      popup <- .build_detail_popup(result, col, display_name, plot_sf)
    }

    qty_suffix <- if (type != "absolute") {
      paste0(" (", .quantity_label(type), ")")
    } else {
      ""
    }
    lname <- paste0(display_name, qty_suffix)

    m <- suppressWarnings(mapview::mapview(
      plot_sf,
      zcol = display_name,
      layer.name = lname,
      col.regions = pal,
      alpha.regions = alpha,
      legend = legend,
      popup = popup,
      map.types = basemap,
      ...
    ))

    # Add municipality contour (lines only, non-interactive)
    if (!is.null(muni_sf)) {
      muni_lines <- sf::st_cast(sf::st_geometry(muni_sf), "MULTILINESTRING")
      muni_lines_sf <- sf::st_sf(geometry = muni_lines)
      muni_map <- suppressWarnings(mapview::mapview(
        muni_lines_sf,
        color = "grey30", lwd = 2,
        legend = FALSE,
        popup = FALSE,
        label = FALSE,
        homebutton = FALSE,
        layer.name = "Municipality",
        map.types = basemap
      ))
      m <- m + muni_map
    }

    m
  })

  do.call(leafsync::sync, maps)
}


# --- Popup helpers ---

#' Auto-select informative popup columns
#' @noRd
.auto_popup_cols <- function(plot_sf, result, plot_col) {
  sf_cols <- names(plot_sf)
  keep <- character(0)

  # Census tract ID
  if (!is.null(result$tract_id) && result$tract_id %in% sf_cols) {
    keep <- c(keep, result$tract_id)
  }

  # The plotted variable (computed quantity added to plot_sf)
  if (plot_col %in% sf_cols) {
    keep <- c(keep, plot_col)
  }

  # Turnout columns
  turnout <- intersect(c("QT_COMPARECIMENTO", "QT_APTOS", "QT_ABSTENCOES"),
                        sf_cols)
  keep <- c(keep, turnout)

  # Calibration columns (population by age group)
  if (!is.null(result$calib_cols) && length(result$calib_cols$tracts) > 0) {
    calib <- intersect(result$calib_cols$tracts, sf_cols)
    keep <- c(keep, calib)
  }

  # Deduplicate, cap at 8
  keep <- unique(keep)
  keep[seq_len(min(length(keep), 8L))]
}


#' Build a rich detail popup with absolute count, percentages, rank, and category
#'
#' Computes all derived quantities and formats them for the popup,
#' regardless of which `type` is used for map coloring.
#'
#' @param result An `interpElections_result` object.
#' @param col Resolved column name.
#' @param display_name Display name for the variable (may be factor column).
#' @param plot_sf The sf object used for plotting (may have binned factor column).
#' @return Character vector of HTML popup strings (one per feature).
#' @noRd
.build_detail_popup <- function(result, col, display_name, plot_sf) {
  n <- nrow(plot_sf)

  # Compute derived quantities
  abs_vals <- .compute_quantity(result, col, "absolute")
  pct_tract <- tryCatch(
    .compute_quantity(result, col, "pct_tract"),
    error = function(e) rep(NA_real_, n)
  )
  pct_muni <- tryCatch(
    .compute_quantity(result, col, "pct_muni"),
    error = function(e) rep(NA_real_, n)
  )

  # Rank by pct_tract (descending: highest % = rank 1)
  rank_vals <- rank(-pct_tract, ties.method = "min", na.last = "keep")
  n_valid <- sum(!is.na(pct_tract))

  # Add formatted columns to a local copy
  popup_sf <- plot_sf
  popup_cols <- character(0)

  # --- Municipality identification ---
  if (!is.null(result$nome_municipio)) {
    popup_sf[["Municipality"]] <- rep(result$nome_municipio, n)
    popup_cols <- c(popup_cols, "Municipality")
  }
  if (!is.null(result$code_muni)) {
    popup_sf[["IBGE Code"]] <- rep(result$code_muni, n)
    popup_cols <- c(popup_cols, "IBGE Code")
  }
  if (!is.null(result$code_muni_tse)) {
    popup_sf[["TSE Code"]] <- rep(result$code_muni_tse, n)
    popup_cols <- c(popup_cols, "TSE Code")
  }

  # --- Census tract ID ---
  tract_label <- "Census Tract"
  id_col <- result$tract_id
  if (!is.null(id_col) && id_col %in% names(popup_sf)) {
    names(popup_sf)[names(popup_sf) == id_col] <- tract_label
  }
  popup_cols <- c(popup_cols, tract_label)

  # --- Total population ---
  if ("pop_total" %in% names(plot_sf)) {
    popup_sf[["Total Population"]] <- format(
      round(plot_sf[["pop_total"]], 1),
      big.mark = ",", scientific = FALSE, trim = TRUE
    )
    popup_cols <- c(popup_cols, "Total Population")
  }

  # --- Population by age group ---
  pop_age_cols <- grep("^pop_", names(plot_sf), value = TRUE)
  pop_age_cols <- setdiff(pop_age_cols, "pop_total")
  if (length(pop_age_cols) > 0) {
    # Human-readable labels: pop_18_20 -> "Pop 18-20", pop_70mais -> "Pop 70+"
    for (pc in pop_age_cols) {
      label <- sub("^pop_", "", pc)
      label <- gsub("_", "-", label)
      label <- sub("mais$", "+", label)
      label <- paste0("Pop ", label)
      popup_sf[[label]] <- format(
        round(plot_sf[[pc]], 1),
        big.mark = ",", scientific = FALSE, trim = TRUE
      )
      popup_cols <- c(popup_cols, label)
    }
  }

  # --- Turnout (QT_COMPARECIMENTO) ---
  if ("QT_COMPARECIMENTO" %in% names(plot_sf)) {
    popup_sf[["Turnout"]] <- format(
      round(plot_sf[["QT_COMPARECIMENTO"]], 1),
      big.mark = ",", scientific = FALSE, trim = TRUE
    )
    popup_cols <- c(popup_cols, "Turnout")
  }

  # --- Variable-specific statistics ---
  popup_sf[["Votes"]] <- ifelse(is.na(abs_vals), "",
    format(round(abs_vals, 1), big.mark = ",", scientific = FALSE, trim = TRUE))

  popup_sf[["% of tract"]] <- ifelse(is.na(pct_tract), "",
    paste0(format(round(pct_tract, 2), nsmall = 2,
                  scientific = FALSE, trim = TRUE), "%"))

  popup_sf[["% of municipality"]] <- ifelse(is.na(pct_muni), "",
    paste0(format(round(pct_muni, 2), nsmall = 2,
                  scientific = FALSE, trim = TRUE), "%"))

  popup_sf[["Tract rank"]] <- ifelse(is.na(rank_vals), "",
    paste0(rank_vals, " / ", n_valid))

  popup_cols <- c(popup_cols, "Votes", "% of tract",
                  "% of municipality", "Tract rank")

  # Include bracket category if binned
  if (is.factor(plot_sf[[display_name]])) {
    popup_sf[["Category"]] <- as.character(plot_sf[[display_name]])
    popup_cols <- c(popup_cols, "Category")
  }

  .build_popup(popup_sf, popup_cols)
}


#' Build popup from selected columns
#' @noRd
.build_popup <- function(sf_data, popup_cols) {
  if (length(popup_cols) == 0) return(TRUE)  # mapview default
  if (requireNamespace("leafpop", quietly = TRUE)) {
    leafpop::popupTable(sf_data, zcol = popup_cols, row.numbers = FALSE)
  } else {
    # Fallback: let mapview handle it, but select only the popup columns
    # mapview accepts a character vector for popup to filter columns
    popup_cols
  }
}


# --- Palette helper ---

#' Convert palette name to a color ramp vector for mapview
#' @noRd
.mapview_palette <- function(palette, n = 100L) {
  viridis_names <- c("viridis", "magma", "plasma", "inferno",
                     "cividis", "mako", "rocket", "turbo")
  is_viridis <- tolower(palette) %in% viridis_names

  if (is_viridis && requireNamespace("viridisLite", quietly = TRUE)) {
    viridisLite::viridis(n, option = palette)
  } else if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    pal_info <- tryCatch(
      RColorBrewer::brewer.pal.info[palette, ],
      error = function(e) NULL
    )
    max_n <- if (!is.null(pal_info)) pal_info$maxcolors else 9L
    base_colors <- RColorBrewer::brewer.pal(min(max_n, 9L), palette)
    rev(grDevices::colorRampPalette(base_colors)(n))
  } else {
    grDevices::heat.colors(n)
  }
}

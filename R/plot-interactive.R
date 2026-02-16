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
#' @param popup_vars Character vector of column names to show in
#'   click popups. If NULL, auto-selects zone ID, the plotted variable,
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
  plot_sf[[display_name]] <- values

  # Build popup
  popup_cols <- if (!is.null(popup_vars)) {
    intersect(popup_vars, names(plot_sf))
  } else {
    .auto_popup_cols(result, display_name)
  }
  popup <- .build_popup(plot_sf, popup_cols)

  # Layer name
  if (is.null(layer_name)) {
    qty_suffix <- if (type != "absolute") paste0(" (", .quantity_label(type), ")") else ""
    layer_name <- paste0(display_name, qty_suffix)
  }

  suppressWarnings(mapview::mapview(
    plot_sf,
    zcol = display_name,
    layer.name = layer_name,
    col.regions = .mapview_palette(palette),
    alpha.regions = alpha,
    legend = legend,
    popup = popup,
    map.types = basemap,
    ...
  ))
}


# --- Synchronized multi-variable panels ---

#' @noRd
.interactive_sync <- function(
    result, variables, type, palette,
    popup_vars, alpha, legend, basemap, ...) {

  if (!requireNamespace("leafsync", quietly = TRUE)) {
    stop(
      "The 'leafsync' package is required for multi-variable interactive maps.\n",
      "Install with: install.packages(\"leafsync\")",
      call. = FALSE
    )
  }

  cols <- .resolve_vars(variables, result)

  maps <- lapply(cols, function(col) {
    values <- .compute_quantity(result, col, type)
    plot_sf <- result$tracts_sf
    display_name <- .auto_title(col, result)
    plot_sf[[display_name]] <- values

    popup_cols_i <- if (!is.null(popup_vars)) {
      intersect(popup_vars, names(plot_sf))
    } else {
      .auto_popup_cols(result, display_name)
    }
    popup <- .build_popup(plot_sf, popup_cols_i)

    qty_suffix <- if (type != "absolute") {
      paste0(" (", .quantity_label(type), ")")
    } else {
      ""
    }
    lname <- paste0(display_name, qty_suffix)

    suppressWarnings(mapview::mapview(
      plot_sf,
      zcol = display_name,
      layer.name = lname,
      col.regions = .mapview_palette(palette),
      alpha.regions = alpha,
      legend = legend,
      popup = popup,
      map.types = basemap,
      ...
    ))
  })

  do.call(leafsync::sync, maps)
}


# --- Popup helpers ---

#' Auto-select informative popup columns
#' @noRd
.auto_popup_cols <- function(result, plot_col) {
  sf_cols <- names(result$tracts_sf)
  keep <- character(0)

  # Zone ID
  if (!is.null(result$zone_id) && result$zone_id %in% sf_cols) {
    keep <- c(keep, result$zone_id)
  }

  # The plotted variable
  if (plot_col %in% sf_cols) {
    keep <- c(keep, plot_col)
  }

  # Turnout columns
  turnout <- intersect(c("QT_COMPARECIMENTO", "QT_APTOS", "QT_ABSTENCOES"),
                        sf_cols)
  keep <- c(keep, turnout)

  # Calibration columns (population by age group)
  if (!is.null(result$calib_cols) && length(result$calib_cols$zones) > 0) {
    calib <- intersect(result$calib_cols$zones, sf_cols)
    keep <- c(keep, calib)
  }

  # Deduplicate, cap at 8
  keep <- unique(keep)
  keep[seq_len(min(length(keep), 8L))]
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

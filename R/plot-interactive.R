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


#' Build a visual HTML popup with age pyramid, variable stats, and residuals
#'
#' Generates a compact, visual popup per tract with:
#' - Header: tract ID, neighborhood, population, turnout
#' - Inline SVG age pyramid (male/female by age bracket)
#' - Full variable statistics (absolute, %, rank, density)
#' - Residual quality indicator (when available)
#'
#' @param result An `interpElections_result` object.
#' @param col Resolved column name.
#' @param display_name Display name for the variable (may be factor column).
#' @param plot_sf The sf object used for plotting (may have binned factor column).
#' @return Character vector of HTML popup strings (one per feature).
#' @noRd
.build_detail_popup <- function(result, col, display_name, plot_sf) {
  n <- nrow(plot_sf)

  # --- Compute all derived quantities ---
  abs_vals <- .compute_quantity(result, col, "absolute")
  pct_tract <- tryCatch(
    .compute_quantity(result, col, "pct_tract"),
    error = function(e) rep(NA_real_, n)
  )
  pct_muni <- tryCatch(
    .compute_quantity(result, col, "pct_muni"),
    error = function(e) rep(NA_real_, n)
  )
  pct_eligible <- tryCatch(
    .compute_quantity(result, col, "pct_eligible"),
    error = function(e) rep(NA_real_, n)
  )
  density_vals <- tryCatch(
    .compute_quantity(result, col, "density"),
    error = function(e) rep(NA_real_, n)
  )
  rank_vals <- rank(-pct_tract, ties.method = "min", na.last = "keep")
  n_valid <- sum(!is.na(pct_tract))

  # --- Neighborhood labels (spatial join, if available) ---
  nb_names <- rep(NA_character_, n)
  if (!is.null(result$neighborhoods) &&
      requireNamespace("sf", quietly = TRUE)) {
    tryCatch({
      sf::sf_use_s2(FALSE)
      pts <- suppressWarnings(sf::st_point_on_surface(plot_sf))
      idx <- sf::st_intersects(pts, result$neighborhoods)
      nb_names <- vapply(idx, function(x) {
        if (length(x) == 0) NA_character_
        else result$neighborhoods$name_neighborhood[x[1]]
      }, character(1))
    }, error = function(e) NULL)
  }

  # --- Age pyramid data (census) ---
  gender_age <- .extract_gender_age(plot_sf, result$calib_cols)

  # --- Fitted age data (from interpolated matrix, always available) ---
  fitted_gender_age <- NULL
  if (!is.null(result$calib_cols) &&
      length(result$calib_cols$sources) > 0 &&
      all(result$calib_cols$sources %in% colnames(result$interpolated))) {
    fitted_calib <- result$interpolated[, result$calib_cols$sources, drop = FALSE]
    colnames(fitted_calib) <- result$calib_cols$tracts
    fitted_gender_age <- .extract_gender_age(fitted_calib, result$calib_cols)
  }

  # --- Residuals ---
  tract_resid <- .compute_tract_residuals(result)

  # --- Format helper ---
  .fmt <- function(x, digits = 1) {
    ifelse(is.na(x), "&mdash;",
           format(round(x, digits), big.mark = ",",
                  scientific = FALSE, trim = TRUE))
  }
  .pct <- function(x, digits = 2) {
    ifelse(is.na(x), "&mdash;",
           paste0(format(round(x, digits), nsmall = digits,
                         scientific = FALSE, trim = TRUE), "%"))
  }

  # --- Variable display name ---
  var_label <- .auto_title(col, result)

  # --- CSS ---
  css <- paste0(
    '<style>',
    '.ip-popup { font-family: sans-serif; font-size: 12px; ',
    'line-height: 1.4; max-width: 320px; }',
    '.ip-popup h4 { margin: 0 0 4px 0; font-size: 13px; ',
    'border-bottom: 1px solid #ccc; padding-bottom: 3px; }',
    '.ip-popup .ip-section { margin: 6px 0; }',
    '.ip-popup .ip-row { display: flex; justify-content: space-between; }',
    '.ip-popup .ip-label { color: #666; }',
    '.ip-popup .ip-val { font-weight: 600; }',
    '.ip-popup .ip-resid { font-size: 11px; color: #888; ',
    'border-top: 1px solid #eee; padding-top: 3px; margin-top: 4px; }',
    '</style>'
  )

  # --- Build per-tract HTML ---
  tract_id_col <- result$tract_id
  popups <- vapply(seq_len(n), function(i) {
    html <- css
    html <- paste0(html, '<div class="ip-popup">')

    # Header: tract ID + neighborhood
    tract_id_val <- if (!is.null(tract_id_col) && tract_id_col %in% names(plot_sf)) {
      as.character(plot_sf[[tract_id_col]][i])
    } else ""
    html <- paste0(html, '<h4>Tract: ', tract_id_val, '</h4>')

    if (!is.na(nb_names[i])) {
      html <- paste0(html, '<div><b>Neighborhood:</b> ', nb_names[i], '</div>')
    }

    # Population line
    pop_total <- if ("pop_total" %in% names(plot_sf)) {
      round(plot_sf[["pop_total"]][i])
    } else NA
    # Voting-age = sum of calibration brackets
    voting_age <- NA
    if (!is.null(gender_age)) {
      if (gender_age$has_gender) {
        voting_age <- round(sum(gender_age$male[i, ]) + sum(gender_age$female[i, ]))
      } else if (!is.null(gender_age$age_totals)) {
        voting_age <- round(sum(gender_age$age_totals[i, ]))
      }
    }

    pop_line <- '<div class="ip-row">'
    if (!is.na(pop_total)) {
      pop_line <- paste0(pop_line,
        '<span><span class="ip-label">Total pop:</span> ',
        '<span class="ip-val">', .fmt(pop_total, 0), '</span></span>')
    }
    if (!is.na(voting_age)) {
      pop_line <- paste0(pop_line,
        '<span><span class="ip-label"> Voting age:</span> ',
        '<span class="ip-val">', .fmt(voting_age, 0), '</span></span>')
    }
    pop_line <- paste0(pop_line, '</div>')
    html <- paste0(html, pop_line)

    # Turnout
    if ("QT_COMPARECIMENTO" %in% names(plot_sf)) {
      html <- paste0(html,
        '<div><span class="ip-label">Turnout:</span> ',
        '<span class="ip-val">', .fmt(plot_sf[["QT_COMPARECIMENTO"]][i], 0),
        '</span></div>')
    }

    # Age pyramid
    if (!is.null(gender_age)) {
      html <- paste0(html, '<div class="ip-section">')
      has_fitted <- !is.null(fitted_gender_age)
      pyr_subtitle <- if (has_fitted) {
        "Compulsory voters by age"
      } else {
        "Compulsory voters by age (census)"
      }
      html <- paste0(html,
        '<div style="text-align:center;font-size:11px;color:#666;',
        'margin-bottom:2px">', pyr_subtitle, '</div>')
      if (gender_age$has_gender) {
        fm <- if (has_fitted && !is.null(fitted_gender_age$male)) {
          fitted_gender_age$male[i, ]
        }
        ff <- if (has_fitted && !is.null(fitted_gender_age$female)) {
          fitted_gender_age$female[i, ]
        }
        html <- paste0(html,
          .build_pyramid_svg(gender_age$male[i, ], gender_age$female[i, ],
                              gender_age$age_labels,
                              fitted_male = fm, fitted_female = ff))
      } else {
        fv <- if (has_fitted && !is.null(fitted_gender_age$age_totals)) {
          fitted_gender_age$age_totals[i, ]
        }
        html <- paste0(html,
          .build_age_bars_svg(gender_age$age_totals[i, ],
                               gender_age$age_labels,
                               fitted_vals = fv))
      }
      html <- paste0(html, '</div>')
    }

    # Variable statistics
    html <- paste0(html, '<div class="ip-section">')
    html <- paste0(html, '<h4>', var_label, '</h4>')
    html <- paste0(html,
      '<div class="ip-row"><span class="ip-label">Votes:</span>',
      '<span class="ip-val">', .fmt(abs_vals[i], 0), '</span></div>')
    html <- paste0(html,
      '<div class="ip-row"><span class="ip-label">% of tract:</span>',
      '<span class="ip-val">', .pct(pct_tract[i]), '</span></div>')
    html <- paste0(html,
      '<div class="ip-row"><span class="ip-label">% of municipality:</span>',
      '<span class="ip-val">', .pct(pct_muni[i]), '</span></div>')
    html <- paste0(html,
      '<div class="ip-row"><span class="ip-label">% of eligible:</span>',
      '<span class="ip-val">', .pct(pct_eligible[i]), '</span></div>')
    html <- paste0(html,
      '<div class="ip-row"><span class="ip-label">Rank:</span>',
      '<span class="ip-val">',
      ifelse(is.na(rank_vals[i]), "&mdash;",
             paste0(rank_vals[i], " / ", n_valid)),
      '</span></div>')
    if (!all(is.na(density_vals))) {
      html <- paste0(html,
        '<div class="ip-row"><span class="ip-label">Density:</span>',
        '<span class="ip-val">', .fmt(density_vals[i]),
        ' / km&sup2;</span></div>')
    }

    # Category (if binned)
    if (is.factor(plot_sf[[display_name]])) {
      html <- paste0(html,
        '<div class="ip-row"><span class="ip-label">Category:</span>',
        '<span class="ip-val">', as.character(plot_sf[[display_name]][i]),
        '</span></div>')
    }
    html <- paste0(html, '</div>')

    # Residual indicator
    if (!is.null(tract_resid) && !is.na(tract_resid[i])) {
      resid_val <- tract_resid[i]
      html <- paste0(html,
        '<div class="ip-resid">Avg |residual|: ',
        .fmt(resid_val, 1), '</div>')
    }

    html <- paste0(html, '</div>')
    html
  }, character(1))

  popups
}


#' Build popup from selected columns (table-based, for popup_vars override)
#' @noRd
.build_popup <- function(sf_data, popup_cols) {
  if (length(popup_cols) == 0) return(TRUE)  # mapview default
  if (requireNamespace("leafpop", quietly = TRUE)) {
    leafpop::popupTable(sf_data, zcol = popup_cols, row.numbers = FALSE)
  } else {
    popup_cols
  }
}


# --- Age pyramid and visual popup helpers ---

#' Extract male/female population matrices from calibration columns
#'
#' Detects full (gender x age) vs age-only calibration and returns
#' aggregated matrices suitable for age pyramid visualization.
#'
#' @param tracts_sf sf object with population columns.
#' @param calib_cols List with `$tracts` character vector of column names.
#' @return List with `male` (matrix), `female` (matrix), `age_labels`
#'   (character), `has_gender` (logical). For age-only, male/female are NULL
#'   and `age_totals` is provided instead.
#' @noRd
.extract_gender_age <- function(data, calib_cols) {
  tract_cols <- calib_cols$tracts
  if (is.null(tract_cols) || length(tract_cols) == 0) return(NULL)

  if (inherits(data, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
    df <- sf::st_drop_geometry(data)
  } else {
    df <- as.data.frame(data)
  }
  n <- nrow(df)

  # Detect full mode: columns match pop_hom_alf_* pattern
  hom_alf <- grep("^pop_hom_alf_", tract_cols, value = TRUE)
  has_gender <- length(hom_alf) > 0

  if (has_gender) {
    # Extract age suffixes from hom_alf columns
    ages <- sub("^pop_hom_alf_", "", hom_alf)

    male_mat <- matrix(0, n, length(ages))
    female_mat <- matrix(0, n, length(ages))
    colnames(male_mat) <- colnames(female_mat) <- ages

    for (i in seq_along(ages)) {
      ag <- ages[i]
      hom_a <- paste0("pop_hom_alf_", ag)
      hom_n <- paste0("pop_hom_nalf_", ag)
      mul_a <- paste0("pop_mul_alf_", ag)
      mul_n <- paste0("pop_mul_nalf_", ag)

      male_mat[, i] <- .col_or_zero(df, hom_a) + .col_or_zero(df, hom_n)
      female_mat[, i] <- .col_or_zero(df, mul_a) + .col_or_zero(df, mul_n)
    }

    # Human-readable labels
    age_labels <- gsub("_", "-", ages)
    age_labels <- sub("mais$", "+", age_labels)

    list(male = male_mat, female = female_mat,
         age_labels = age_labels, has_gender = TRUE)
  } else {
    # Age-only mode: pop_18_19, pop_20_24, etc.
    pop_cols <- grep("^pop_", tract_cols, value = TRUE)
    if (length(pop_cols) == 0) return(NULL)
    ages <- sub("^pop_", "", pop_cols)

    age_mat <- matrix(0, n, length(ages))
    colnames(age_mat) <- ages
    for (i in seq_along(pop_cols)) {
      age_mat[, i] <- .col_or_zero(df, pop_cols[i])
    }

    age_labels <- gsub("_", "-", ages)
    age_labels <- sub("mais$", "+", age_labels)

    list(male = NULL, female = NULL, age_totals = age_mat,
         age_labels = age_labels, has_gender = FALSE)
  }
}

# Safely extract a column or return zeros
.col_or_zero <- function(df, col) {
  if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
}


#' Build an inline SVG age pyramid for a single tract
#'
#' Generates a compact horizontal bar chart with males (left, blue)
#' and females (right, orange). Pure string construction, no rendering.
#'
#' @param male_vals Numeric vector (one per age bracket).
#' @param female_vals Numeric vector (one per age bracket).
#' @param age_labels Character vector of age bracket labels.
#' @param width Total SVG width in pixels. Default: 280.
#' @param height Total SVG height in pixels. Default: 120.
#' @return Character string containing an inline `<svg>` element.
#' @noRd
.build_pyramid_svg <- function(male_vals, female_vals, age_labels,
                                fitted_male = NULL, fitted_female = NULL,
                                width = 280, height = NULL) {
  k <- length(age_labels)
  has_fitted <- !is.null(fitted_male) && !is.null(fitted_female)
  legend_h <- if (has_fitted) 16 else 0
  if (is.null(height)) height <- max(90, k * 18) + legend_h
  bar_h <- floor((height - legend_h) / k) - 2

  # Scale to max across census AND fitted
  all_vals <- c(male_vals, female_vals)
  if (has_fitted) all_vals <- c(all_vals, fitted_male, fitted_female)
  max_val <- max(all_vals, na.rm = TRUE)
  if (max_val == 0) max_val <- 1

  mid_x <- width / 2
  bar_max_w <- mid_x - 35  # Leave room for labels

  bars <- vapply(seq_len(k), function(i) {
    y <- (i - 1) * (bar_h + 2)
    m_w <- round(male_vals[i] / max_val * bar_max_w)
    f_w <- round(female_vals[i] / max_val * bar_max_w)
    m_x <- mid_x - 30 - m_w
    f_x <- mid_x + 30

    s <- sprintf(paste0(
      '<rect x="%d" y="%d" width="%d" height="%d" fill="#4575b4" />',
      '<rect x="%d" y="%d" width="%d" height="%d" fill="#fdae61" />'
    ),
    m_x, y, m_w, bar_h,
    f_x, y, f_w, bar_h)

    # Fitted overlay (outlined rectangles)
    if (has_fitted) {
      fm_w <- max(1, round(fitted_male[i] / max_val * bar_max_w))
      ff_w <- max(1, round(fitted_female[i] / max_val * bar_max_w))
      fm_x <- mid_x - 30 - fm_w
      ff_x <- mid_x + 30
      s <- paste0(s, sprintf(paste0(
        '<rect x="%d" y="%d" width="%d" height="%d" ',
        'fill="none" stroke="#1a3a5c" stroke-width="1.5" />',
        '<rect x="%d" y="%d" width="%d" height="%d" ',
        'fill="none" stroke="#b8651e" stroke-width="1.5" />'
      ),
      fm_x, y, fm_w, bar_h,
      ff_x, y, ff_w, bar_h))
    }

    # Age label
    s <- paste0(s, sprintf(
      '<text x="%d" y="%d" text-anchor="middle" font-size="10" font-family="sans-serif" fill="#333">%s</text>',
      round(mid_x), y + bar_h - 2, age_labels[i]))
    s
  }, character(1))

  # Header labels
  header <- sprintf(paste0(
    '<text x="%d" y="%d" text-anchor="end" font-size="9" ',
    'font-family="sans-serif" fill="#4575b4" font-weight="bold">Male</text>',
    '<text x="%d" y="%d" text-anchor="start" font-size="9" ',
    'font-family="sans-serif" fill="#fdae61" font-weight="bold">Female</text>'
  ), round(mid_x) - 32, -3, round(mid_x) + 32, -3)

  # Legend (only when fitted available)
  legend_svg <- ""
  if (has_fitted) {
    ly <- k * (bar_h + 2) + 4
    legend_svg <- sprintf(paste0(
      '<rect x="%d" y="%d" width="10" height="10" fill="#4575b4" opacity="0.7" />',
      '<text x="%d" y="%d" font-size="9" font-family="sans-serif" fill="#666">Census</text>',
      '<rect x="%d" y="%d" width="10" height="10" fill="none" stroke="#1a3a5c" stroke-width="1.5" />',
      '<text x="%d" y="%d" font-size="9" font-family="sans-serif" fill="#666">Interpolated</text>'
    ),
    round(mid_x) - 80, ly,
    round(mid_x) - 67, ly + 9,
    round(mid_x) + 10, ly,
    round(mid_x) + 23, ly + 9)
  }

  svg_body <- paste(c(header, bars, legend_svg), collapse = "\n")
  sprintf(
    '<svg width="%d" height="%d" viewBox="0 -10 %d %d" ',
    width, height + 10, width, height + 10
  ) |>
    paste0('xmlns="http://www.w3.org/2000/svg">\n', svg_body, '\n</svg>')
}


#' Build a single-sided age bar chart (for age-only calibration)
#' @noRd
.build_age_bars_svg <- function(age_vals, age_labels,
                                 fitted_vals = NULL,
                                 width = 280, height = NULL) {
  k <- length(age_labels)
  has_fitted <- !is.null(fitted_vals)
  legend_h <- if (has_fitted) 16 else 0
  if (is.null(height)) height <- max(90, k * 18) + legend_h
  bar_h <- floor((height - legend_h) / k) - 2

  all_vals <- age_vals
  if (has_fitted) all_vals <- c(all_vals, fitted_vals)
  max_val <- max(all_vals, na.rm = TRUE)
  if (max_val == 0) max_val <- 1

  label_w <- 45
  bar_max_w <- width - label_w - 10

  bars <- vapply(seq_len(k), function(i) {
    y <- (i - 1) * (bar_h + 2)
    bw <- round(age_vals[i] / max_val * bar_max_w)
    s <- sprintf(paste0(
      '<text x="%d" y="%d" text-anchor="end" ',
      'font-size="10" font-family="sans-serif" fill="#333">%s</text>',
      '<rect x="%d" y="%d" width="%d" height="%d" fill="#4575b4" />'
    ),
    label_w - 4, y + bar_h - 2, age_labels[i],
    label_w, y, bw, bar_h)

    if (has_fitted) {
      fw <- max(1, round(fitted_vals[i] / max_val * bar_max_w))
      s <- paste0(s, sprintf(
        '<rect x="%d" y="%d" width="%d" height="%d" fill="none" stroke="#1a3a5c" stroke-width="1.5" />',
        label_w, y, fw, bar_h))
    }
    s
  }, character(1))

  legend_svg <- ""
  if (has_fitted) {
    ly <- k * (bar_h + 2) + 4
    legend_svg <- sprintf(paste0(
      '<rect x="%d" y="%d" width="10" height="10" fill="#4575b4" opacity="0.7" />',
      '<text x="%d" y="%d" font-size="9" font-family="sans-serif" fill="#666">Census</text>',
      '<rect x="%d" y="%d" width="10" height="10" fill="none" stroke="#1a3a5c" stroke-width="1.5" />',
      '<text x="%d" y="%d" font-size="9" font-family="sans-serif" fill="#666">Interpolated</text>'
    ),
    label_w, ly,
    label_w + 13, ly + 9,
    label_w + 80, ly,
    label_w + 93, ly + 9)
  }

  sprintf('<svg width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">\n%s\n</svg>',
          width, height, paste(c(bars, legend_svg), collapse = "\n"))
}


#' Compute per-tract residual summary
#'
#' @param result interpElections_result object.
#' @return Numeric vector of mean |residual| per tract, or NULL if
#'   residuals cannot be computed.
#' @noRd
.compute_tract_residuals <- function(result) {
  resid <- tryCatch(residuals(result), error = function(e) NULL)
  if (is.null(resid)) return(NULL)
  rowMeans(abs(resid))
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

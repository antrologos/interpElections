#' One-step IDW interpolation from source points to census tracts
#'
#' High-level wrapper that combines travel-time computation (optional),
#' alpha optimization, and interpolation into a single call. If no travel
#' time matrix is provided, OSM road network data is automatically
#' downloaded and travel times are computed via r5r.
#'
#' @param tracts_sf An `sf` polygon object. Target census tracts.
#' @param electoral_sf An `sf` point object. Source points (e.g., voting
#'   locations).
#' @param tract_id Character. Name of the ID column in `tracts_sf`.
#' @param point_id Character. Name of the ID column in `electoral_sf`.
#' @param calib_tracts Character vector. Column names in `tracts_sf` to use
#'   as the calibration population matrix. Must match `calib_sources` in
#'   length.
#' @param calib_sources Character vector. Column names in `electoral_sf` to
#'   use as the source calibration matrix. Must match `calib_tracts` in
#'   length.
#' @param interp_sources Character vector or NULL. Column names in
#'   `electoral_sf` to interpolate. Default NULL means all numeric columns
#'   not in `calib_sources`.
#' @param time_matrix Numeric matrix \[n x m\] or NULL. Pre-computed travel
#'   times. If provided, skips all travel time computation.
#' @param network_path Character or NULL. Path to directory with an OSM
#'   `.pbf` file. If provided (and `time_matrix` is NULL), travel times
#'   are computed directly without downloading OSM data.
#' @param elevation_path Character or NULL. Path to elevation `.tif` file
#'   for r5r routing.
#' @param osm_buffer_km Numeric. Buffer in kilometers to expand the
#'   bounding box when auto-downloading OSM data. Default: 10.
#' @param min_pop Numeric. Minimum total population in `calib_tracts` for a
#'   census tract to be included. Default: 1.
#' @param alpha Numeric vector of length n, or NULL. Pre-computed decay
#'   parameters. If provided, optimization is skipped.
#' @param offset Numeric. Travel time offset. Default: 1.
#' @param keep Character vector or NULL. Names of heavy intermediate
#'   objects to include in the result. Default NULL (lightweight).
#'   Options: `"weights"` (column-standardized weight matrix \[n x m\]),
#'   `"time_matrix"` (travel time matrix \[n x m\]),
#'   `"sources_sf"` (source points as `sf` object with geometry).
#'   These can be large for big municipalities. Travel times are
#'   cached on disk and can be reloaded without keeping them in memory.
#' @param use_gpu Logical or NULL. Passed to [optimize_alpha()].
#' @param cpu_parallel Logical. Use `optimParallel` for parallel CPU
#'   optimization? Default: `FALSE`. **Not recommended:** ~10x slower
#'   than serial L-BFGS-B in practice because `optimParallel` cannot
#'   parallelize the analytical gradient. For large municipalities,
#'   use GPU (`use_gpu = TRUE`). Passed to [optimize_alpha()].
#' @param verbose Logical. Print progress. Default: TRUE.
#' @param ... Additional arguments forwarded to [optimize_alpha()],
#'   [compute_travel_times()], and/or [download_r5r_data()].
#' @param .step_offset Integer. Internal: offset added to step numbers
#'   when called from [interpolate_election_br()]. Do not set manually.
#' @param .step_total Integer or NULL. Internal: total step count for
#'   unified progress display. Do not set manually.
#'
#' @return A list of class `"interpElections_result"` with components:
#' \describe{
#'   \item{interpolated}{Numeric matrix \[n x p\]. Interpolated values.}
#'   \item{alpha}{Numeric vector of length n. Decay parameters used.}
#'   \item{tracts_sf}{`sf` object with interpolated columns joined to census tracts.}
#'   \item{sources}{Data frame (no geometry) of source point data.}
#'   \item{optimization}{`interpElections_optim` object, or NULL if `alpha` was
#'     pre-supplied.}
#'   \item{offset}{Numeric. Offset value used.}
#'   \item{call}{The matched call.}
#'   \item{tract_id}{Character. Name of the census tract ID column.}
#'   \item{point_id}{Character. Name of the ID column in sources.}
#'   \item{interp_cols}{Character vector. Names of interpolated columns.}
#'   \item{calib_cols}{List with `$tracts` and `$sources` calibration columns.}
#'   \item{weights}{Numeric matrix \[n x m\] or NULL. Present only when
#'     `keep` includes `"weights"`.}
#'   \item{time_matrix}{Numeric matrix \[n x m\] or NULL. Present only when
#'     `keep` includes `"time_matrix"`.}
#'   \item{sources_sf}{`sf` point object or NULL. Source points with
#'     geometry. Present only when `keep` includes `"sources_sf"`.}
#' }
#'
#' @examples
#' \dontrun{
#' # Minimal: sf objects + column names (auto-downloads OSM)
#' result <- interpolate_election(
#'   tracts_sf    = census_tracts,
#'   electoral_sf = voting_stations,
#'   tract_id     = "code_tract",
#'   point_id     = "id",
#'   calib_tracts  = c("pop_18_24", "pop_25_34"),
#'   calib_sources = c("voters_18_24", "voters_25_34")
#' )
#'
#' # With pre-computed travel times (skip r5r)
#' result <- interpolate_election(
#'   tracts_sf    = census_tracts,
#'   electoral_sf = voting_stations,
#'   tract_id     = "code_tract",
#'   point_id     = "id",
#'   calib_tracts  = c("pop_young", "pop_old"),
#'   calib_sources = c("voters_young", "voters_old"),
#'   time_matrix  = my_tt_matrix
#' )
#' }
#'
#' @family wrappers
#'
#' @seealso [optimize_alpha()], [idw_interpolate()],
#'   [interpolate_election_br()] for the Brazilian-specific wrapper.
#'
#' @export
interpolate_election <- function(
    tracts_sf,
    electoral_sf,
    tract_id,
    point_id,
    calib_tracts,
    calib_sources,
    interp_sources = NULL,
    time_matrix    = NULL,
    network_path   = NULL,
    elevation_path = NULL,
    osm_buffer_km  = 10,
    min_pop        = 1,
    alpha          = NULL,
    offset         = 1,
    keep           = NULL,
    use_gpu        = NULL,
    cpu_parallel   = FALSE,
    verbose        = TRUE,
    ...,
    .step_offset   = 0L,
    .step_total    = NULL
) {
  cl <- match.call()
  dots <- list(...)

  # --- Check all dependencies upfront ---
  missing_pkgs <- character(0)
  if (!requireNamespace("sf", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "sf")
  }
  if (is.null(time_matrix) && is.null(network_path)) {
    # Will need to download OSM and compute travel times
    for (pkg in c("osmextract", "r5r")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        missing_pkgs <- c(missing_pkgs, pkg)
      }
    }
  } else if (is.null(time_matrix)) {
    # Have network_path but need r5r to compute travel times
    if (!requireNamespace("r5r", quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, "r5r")
    }
  }
  if (length(missing_pkgs) > 0) {
    stop(
      "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
      "\nInstall with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  # Check for OSM clipping tools if travel times will be computed
  if (is.null(time_matrix) && is.null(network_path) && !.has_clip_tool()) {
    .offer_osmium_install(verbose = verbose)
    if (!.has_clip_tool()) {
      stop(
        "A clipping tool (osmium or osmconvert) is required ",
        "to clip state-level OSM files for r5r routing.\n\n",
        "Run interpElections::setup_osmium() to install, ",
        "or install manually:\n",
        "  Windows: conda install -c conda-forge osmium-tool\n",
        "  macOS:   brew install osmium-tool\n",
        "  Linux:   sudo apt install osmium-tool",
        call. = FALSE
      )
    }
  }

  # --- Validate inputs ---
  if (!inherits(tracts_sf, "sf")) {
    stop("'tracts_sf' must be an sf object", call. = FALSE)
  }
  if (!inherits(electoral_sf, "sf")) {
    stop("'electoral_sf' must be an sf object", call. = FALSE)
  }
  if (length(calib_tracts) != length(calib_sources)) {
    stop(sprintf(
      "calib_tracts (%d) and calib_sources (%d) must have the same length",
      length(calib_tracts), length(calib_sources)
    ), call. = FALSE)
  }

  # Check columns exist
  tracts_df <- sf::st_drop_geometry(tracts_sf)
  elec_df <- sf::st_drop_geometry(electoral_sf)

  missing_z <- setdiff(calib_tracts, names(tracts_df))
  if (length(missing_z) > 0) {
    stop("calib_tracts columns not found in tracts_sf: ",
         paste(missing_z, collapse = ", "), call. = FALSE)
  }
  missing_s <- setdiff(calib_sources, names(elec_df))
  if (length(missing_s) > 0) {
    stop("calib_sources columns not found in electoral_sf: ",
         paste(missing_s, collapse = ", "), call. = FALSE)
  }

  # --- Spatial overlap sanity check ---
  # Disable S2 for this check to ensure consistent planar geometry
  # across platforms (s2 default differs across sf versions/OS).
  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))

  # The convex hulls of tracts and source points must intersect,
  # otherwise the data refers to different geographic areas.
  # Transform to common CRS (tracts' CRS) before comparing.
  tracts_hull <- sf::st_convex_hull(sf::st_union(tracts_sf))
  electoral_check <- sf::st_transform(electoral_sf, sf::st_crs(tracts_sf))
  points_hull <- sf::st_convex_hull(sf::st_union(electoral_check))
  if (!sf::st_intersects(tracts_hull, points_hull, sparse = FALSE)[1, 1]) {
    stop(
      "Spatial mismatch: the convex hulls of tracts_sf and electoral_sf ",
      "do not overlap. Check that both datasets refer to the same ",
      "geographic area.",
      call. = FALSE
    )
  }

  # --- Filter census tracts by min_pop ---
  if (min_pop > 0) {
    pop_total <- rowSums(tracts_df[, calib_tracts, drop = FALSE], na.rm = TRUE)
    keep_rows <- pop_total >= min_pop
    if (sum(keep_rows) < nrow(tracts_sf)) {
      n_removed <- sum(!keep_rows)
      tracts_sf <- tracts_sf[keep_rows, ]
      tracts_df <- sf::st_drop_geometry(tracts_sf)
      if (verbose) {
        message(sprintf("  Filtered %d census tracts with pop < %g (%d remaining)",
                        n_removed, min_pop, nrow(tracts_sf)))
      }
    }
  }

  # --- Step 1: Resolve travel time matrix ---
  # Step count depends on path taken:
  #   Pre-computed matrix: 3 steps (matrix, optimize, interpolate)
  #   User-provided network: 4 steps (+ compute travel times)
  #   Auto-download OSM:    5 steps (+ download OSM + compute travel times)
  step_num <- 1L + .step_offset
  total_steps <- if (!is.null(.step_total)) .step_total else 3L

  if (is.null(time_matrix)) {
    # Check for cached travel time matrix
    # Use actual (unsorted) IDs — the matrix is positional, so row/col order matters
    zone_ids <- as.character(tracts_df[[tract_id]])
    point_ids <- as.character(elec_df[[point_id]])
    # Include routing params in cache key so different modes
    # don't collide
    tt_mode <- dots$mode %||% "WALK"
    tt_dur <- dots$max_trip_duration %||% 300L
    cache_input <- paste(
      c(zone_ids, "||", point_ids, "||",
        tt_mode, tt_dur),
      collapse = ","
    )
    tt_hash <- substr(.digest_simple(cache_input), 1, 16)
    tt_cache_name <- sprintf("tt_%s.rds", tt_hash)
    cached_tt <- .load_from_cache(tt_cache_name, .cache_subdirs()$travel_times)

    # Validate cache: check dimensions AND that stored IDs match exactly
    tt_cache_valid <- !is.null(cached_tt) &&
      is.matrix(cached_tt) &&
      nrow(cached_tt) == length(zone_ids) &&
      ncol(cached_tt) == length(point_ids) &&
      identical(attr(cached_tt, "zone_ids"), zone_ids) &&
      identical(attr(cached_tt, "point_ids"), point_ids)

    if (tt_cache_valid) {
      time_matrix <- cached_tt
    }

    # Determine step counts based on which path we'll take
    if (!is.null(time_matrix)) {
      # Cached travel times — keep original total, skip step numbers
      if (!is.null(.step_total) && is.null(network_path)) {
        # Cache replaces 2 planned steps (download OSM + compute TT)
        if (verbose) message(sprintf(
          "[%d-%d/%d] Using cached travel time matrix (%dx%d)",
          step_num, step_num + 1L, total_steps,
          nrow(time_matrix), ncol(time_matrix)
        ))
        step_num <- step_num + 2L
      } else {
        # Cache replaces 1 step (compute TT) — 1-for-1, or standalone call
        if (verbose) message(sprintf(
          "[%d/%d] Using cached travel time matrix (%dx%d)",
          step_num, total_steps, nrow(time_matrix), ncol(time_matrix)
        ))
        step_num <- step_num + 1L
      }
    } else if (is.null(network_path)) {
      # Need to download OSM + compute — 5 steps total (when standalone)
      if (is.null(.step_total)) total_steps <- 5L + .step_offset
      if (verbose) message(sprintf("[%d/%d] Downloading OSM road network...",
                                   step_num, total_steps))

      expanded_area <- .expand_bbox(tracts_sf, electoral_sf,
                                    buffer_km = osm_buffer_km)
      # Each municipality bbox gets its own isolated r5r directory
      bb <- sf::st_bbox(sf::st_transform(expanded_area, 4326))
      bbox_str <- sprintf("%.4f_%.4f_%.4f_%.4f",
                          bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      bbox_hash <- substr(.digest_simple(bbox_str), 1, 12)
      r5r_dir <- file.path(get_interpElections_cache_dir(),
                           .cache_subdirs()$r5r, bbox_hash)
      if (!dir.exists(r5r_dir)) dir.create(r5r_dir, recursive = TRUE)
      dl_args <- .extract_args(dots, download_r5r_data)
      r5r_data <- do.call(download_r5r_data, c(
        list(area_sf = expanded_area, output_dir = r5r_dir,
             elevation = FALSE, verbose = verbose),
        dl_args
      ))
      network_path <- r5r_data$output_dir

      # If user provided elevation file, copy it to the network dir
      if (!is.null(elevation_path) && file.exists(elevation_path)) {
        file.copy(elevation_path,
                  file.path(network_path, basename(elevation_path)),
                  overwrite = TRUE)
      }

      step_num <- step_num + 1L
    } else {
      # User provided network_path — 4 steps total (when standalone)
      if (is.null(.step_total)) total_steps <- 4L + .step_offset
    }

    # Compute travel times if not cached
    if (is.null(time_matrix)) {
      if (verbose) message(sprintf("[%d/%d] Computing travel times...",
                                   step_num, total_steps))

      tt_args <- .extract_args(dots, compute_travel_times)
      time_matrix <- do.call(compute_travel_times, c(
        list(tracts_sf = tracts_sf, points_sf = electoral_sf,
             network_path = network_path,
             tract_id = tract_id, point_id = point_id,
             verbose = verbose),
        tt_args
      ))

      # Cache the computed travel time matrix with ID attributes
      attr(time_matrix, "zone_ids") <- zone_ids
      attr(time_matrix, "point_ids") <- point_ids
      tryCatch(
        .save_to_cache(time_matrix, tt_cache_name,
                       .cache_subdirs()$travel_times),
        error = function(e) {
          if (verbose) {
            warning("Failed to cache travel time matrix: ",
                    conditionMessage(e), call. = FALSE)
          }
        }
      )
      step_num <- step_num + 1L
    }
  } else {
    if (!is.matrix(time_matrix)) time_matrix <- as.matrix(time_matrix)
    if (verbose) message(sprintf("[%d/%d] Using pre-computed travel time matrix",
                                 step_num, total_steps))
    step_num <- step_num + 1L
  }

  # --- Step 2: Build calibration matrices ---
  pop_matrix <- as.matrix(tracts_df[, calib_tracts, drop = FALSE])
  storage.mode(pop_matrix) <- "double"

  source_matrix <- as.matrix(elec_df[, calib_sources, drop = FALSE])
  storage.mode(source_matrix) <- "double"

  # --- Step 3: Determine interpolation columns ---
  if (is.null(interp_sources)) {
    num_cols <- names(elec_df)[vapply(elec_df, is.numeric, logical(1))]
    interp_sources <- setdiff(num_cols, calib_sources)
    if (length(interp_sources) == 0) {
      interp_sources <- calib_sources
    }
  } else {
    missing_i <- setdiff(interp_sources, names(elec_df))
    if (length(missing_i) > 0) {
      stop("interp_sources columns not found in electoral_sf: ",
           paste(missing_i, collapse = ", "), call. = FALSE)
    }
  }

  interp_data <- as.matrix(elec_df[, interp_sources, drop = FALSE])
  storage.mode(interp_data) <- "double"

  # --- Validate dimensions ---
  if (nrow(pop_matrix) != nrow(time_matrix)) {
    stop(sprintf("tracts_sf has %d rows but time_matrix has %d rows",
                 nrow(pop_matrix), nrow(time_matrix)), call. = FALSE)
  }
  if (nrow(source_matrix) != ncol(time_matrix)) {
    stop(sprintf("electoral_sf has %d rows but time_matrix has %d columns",
                 nrow(source_matrix), ncol(time_matrix)), call. = FALSE)
  }

  # --- Step 4: Optimize alpha ---
  optim_result <- NULL
  if (is.null(alpha)) {
    if (verbose) message(sprintf("[%d/%d] Optimizing alpha...",
                                 step_num, total_steps))
    opt_args <- .extract_args(dots, optimize_alpha)
    optim_result <- do.call(optimize_alpha, c(
      list(time_matrix = time_matrix, pop_matrix = pop_matrix,
           source_matrix = source_matrix, offset = offset,
           use_gpu = use_gpu, cpu_parallel = cpu_parallel,
           verbose = verbose),
      opt_args
    ))
    alpha <- optim_result$alpha
  } else {
    if (verbose) message(sprintf("[%d/%d] Using pre-supplied alpha (skipping optimization)",
                                 step_num, total_steps))
  }
  step_num <- step_num + 1L

  # --- Step 5: Interpolate ---
  if (verbose) message(sprintf("[%d/%d] Interpolating...",
                               step_num, total_steps))
  W_std <- idw_weights(time_matrix, alpha, offset = offset)
  interpolated <- W_std %*% interp_data
  if (!is.null(colnames(interp_data))) {
    colnames(interpolated) <- colnames(interp_data)
  }

  # --- Build tracts_sf with interpolated columns joined ---
  tracts_result <- tracts_sf
  for (col in colnames(interpolated)) {
    tracts_result[[col]] <- interpolated[, col]
  }

  result <- list(
    interpolated  = interpolated,
    alpha         = alpha,
    tracts_sf     = tracts_result,
    sources       = elec_df,
    optimization  = optim_result,
    offset        = offset,
    call          = cl,
    tract_id      = tract_id,
    point_id      = point_id,
    interp_cols   = colnames(interpolated),
    calib_cols    = list(tracts = calib_tracts, sources = calib_sources),
    # Heavy objects (opt-in)
    weights       = if ("weights" %in% keep) W_std else NULL,
    time_matrix   = if ("time_matrix" %in% keep) time_matrix else NULL,
    sources_sf    = if ("sources_sf" %in% keep) electoral_sf else NULL,
    # Brazilian metadata (NULL when called generically)
    code_muni      = NULL,
    nome_municipio = NULL,
    code_muni_tse  = NULL,
    uf             = NULL,
    year           = NULL,
    census_year    = NULL,
    what           = NULL,
    pop_data       = NULL,
    dictionary     = NULL
  )
  class(result) <- "interpElections_result"

  if (verbose) {
    message(sprintf("  Interpolated %d variables into %d census tracts",
                    ncol(interpolated), nrow(interpolated)))
  }

  result
}

#' @export
print.interpElections_result <- function(x, ...) {
  # Header
  if (!is.null(x$code_muni)) {
    cat("interpElections result -- Brazilian election\n")
    if (!is.null(x$nome_municipio)) {
      cat(sprintf("  Municipality: %s (%s)\n", x$nome_municipio, x$uf))
      cat(sprintf("  IBGE: %s | TSE: %s | Election: %d | Census: %d\n",
                  x$code_muni, x$code_muni_tse, x$year, x$census_year))
    } else {
      cat(sprintf("  Municipality: %s (election %d, census %d)\n",
                  x$code_muni, x$year, x$census_year))
    }
  } else {
    cat("interpElections result\n")
  }

  # Dimensions
  n <- nrow(x$interpolated)
  m <- nrow(x$sources)
  p <- ncol(x$interpolated)
  cat(sprintf("  Census tracts: %d | Sources: %d\n", n, m))

  # Variable summary by type (from dictionary) or plain column names
  cat(sprintf("\n  Variables: %d\n", p))
  if (!is.null(x$dictionary) && nrow(x$dictionary) > 0) {
    .print_var_summary(x$dictionary)
  } else if (p <= 8 && !is.null(x$interp_cols)) {
    cat(sprintf("    %s\n", paste(x$interp_cols, collapse = ", ")))
  } else if (!is.null(x$interp_cols)) {
    first4 <- x$interp_cols[seq_len(min(4, p))]
    cat(sprintf("    %s", paste(first4, collapse = ", ")))
    if (p > 4) cat(sprintf(", ... and %d more", p - 4))
    cat("\n")
  }

  # Optimization
  cat("\n")
  if (!is.null(x$optimization)) {
    cat(sprintf("  Optimizer: %s (obj = %.2f)\n",
                x$optimization$method, x$optimization$value))
  } else {
    cat("  Optimizer: skipped (alpha provided)\n")
  }
  cat(sprintf("  Alpha:     [%.3f, %.3f] (mean %.3f)\n",
              min(x$alpha), max(x$alpha), mean(x$alpha)))

  # Contents
  cat("\n  Contents:\n")
  cat(sprintf("    result$tracts_sf       sf with census tracts + interpolated columns\n"))
  cat(sprintf("    result$interpolated    numeric matrix [%d x %d]\n", n, p))
  cat(sprintf("    result$alpha           decay parameters (length %d)\n", n))
  if (!is.null(x$dictionary)) {
    cat(sprintf("    result$dictionary      column metadata (%d rows) -- View(result$dictionary)\n",
                nrow(x$dictionary)))
  }
  cat(sprintf("    result$sources         source data frame [%d rows]\n", m))
  # Heavy objects
  has_w <- !is.null(x$weights)
  has_t <- !is.null(x$time_matrix)
  has_s <- !is.null(x$sources_sf)
  if (has_w) cat("    result$weights         weight matrix\n")
  if (has_t) cat("    result$time_matrix     travel time matrix\n")
  if (has_s) cat("    result$sources_sf      source locations (sf)\n")

  cat("  Methods: summary(), as.data.frame(), coef(), residuals()\n")
  cat("  Plotting: plot(result, variable = ..., type = ..., breaks = ...)\n")
  cat("            plot_interactive(result, variable = ...) -- mapview\n")

  invisible(x)
}

# Print variable summary grouped by type from dictionary
.print_var_summary <- function(dict) {
  # Fixed display order
  type_order <- c("candidate", "party", "turnout", "demographics", "calibration")
  type_labels <- c(candidate = "Candidates", party = "Parties",
                   turnout = "Turnout", demographics = "Demographics",
                   calibration = "Calibration")
  # Max names to show inline (calibration/turnout/demographics always full)
  max_abbrev <- 4L

  types_present <- intersect(type_order, unique(dict$type))
  for (tp in types_present) {
    cols <- dict$column[dict$type == tp]
    label <- type_labels[tp]
    nc <- length(cols)

    # Calibration, turnout, demographics: always show all
    if (tp %in% c("calibration", "turnout", "demographics")) {
      col_str <- paste(cols, collapse = ", ")
      cat(sprintf("    %-14s %d (%s)\n", paste0(label, ":"), nc, col_str))
    } else {
      # Candidates, parties: abbreviate if too many
      if (nc <= max_abbrev) {
        col_str <- paste(cols, collapse = ", ")
      } else {
        col_str <- paste0(
          paste(cols[seq_len(max_abbrev)], collapse = ", "),
          ", ..."
        )
      }
      cat(sprintf("    %-14s %d (%s)\n", paste0(label, ":"), nc, col_str))
    }
  }
}

# Format a single dictionary row into a compact label for printing
.format_dict_label <- function(row) {
  if (row$type == "candidate") {
    name_part <- if (!is.na(row$candidate_name)) row$candidate_name else ""
    party_part <- if (!is.na(row$party)) paste0(" (", row$party, ")") else ""
    paste0(name_part, party_part)
  } else if (row$type == "party") {
    if (!is.na(row$party)) row$party else ""
  } else {
    row$type
  }
}

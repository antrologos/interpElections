#' One-step IDW interpolation from source points to census tracts
#'
#' General-purpose wrapper that combines travel-time computation (optional),
#' alpha optimization, and interpolation into a single call. Accepts R
#' objects (sf data frames) directly. If no travel time matrix is provided,
#' OSM road network data is automatically downloaded and travel times are
#' computed via r5r.
#'
#' For Brazilian elections, use [interpolate_election_br()] which
#' auto-downloads all required data.
#'
#' @param tracts_sf An `sf` polygon object. Target census tracts.
#' @param electoral_sf An `sf` point object. Source points (e.g., voting
#'   locations).
#' @param calib_tracts Character vector. Column names in `tracts_sf` to use
#'   as the calibration population matrix. Must match `calib_sources` in
#'   length.
#' @param calib_sources Character vector. Column names in `electoral_sf` to
#'   use as the source calibration matrix. Must match `calib_tracts` in
#'   length.
#' @param interp_sources Character vector or NULL. Column names in
#'   `electoral_sf` to interpolate. Default NULL means all numeric columns
#'   not in `calib_sources`.
#' @param tract_id Character. Name of the ID column in `tracts_sf`.
#'   Default: `"id"`.
#' @param point_id Character. Name of the ID column in `electoral_sf`.
#'   Default: `"id"`.
#' @param time_matrix Numeric matrix \[n x m\] or NULL. Pre-computed travel
#'   times. If provided, skips all travel time computation.
#' @param optim An [optim_control()] object with optimization parameters.
#'   Default: `optim_control()`.
#' @param routing A [routing_control()] object with routing parameters.
#'   Default: `routing_control()`.
#' @param min_tract_pop Numeric. Minimum total population in
#'   `calib_tracts` for a census tract to be included. Default: 1.
#' @param offset Numeric. Travel time offset. Default: 1.
#' @param keep Character vector or NULL. Names of extra intermediate
#'   objects to include in the result. `weights` and `time_matrix` are
#'   always kept. Options: `"electoral_sf"`, `"pop_raster"`,
#'   `"rep_points"`.
#' @param verbose Logical. Print progress. Default: TRUE.
#' @param ... Advanced arguments. `network_path` (character) for
#'   pre-downloaded OSM data, `elevation_path` (character) for
#'   elevation GeoTIFF. Also accepts **deprecated** old-style
#'   parameters (`max_epochs`, `alpha_min`, `use_gpu`, `mode`,
#'   `gtfs_zip_path`, etc.) for backward compatibility.
#'
#' @return A list of class `"interpElections_result"` with components:
#' \describe{
#'   \item{interpolated}{Numeric matrix \[n x p\]. Interpolated values.}
#'   \item{alpha}{Decay parameters used.}
#'   \item{tracts_sf}{`sf` object with interpolated columns joined.}
#'   \item{sources}{Data frame (no geometry) of source point data.}
#'   \item{optimization}{`interpElections_optim` object.}
#'   \item{offset}{Numeric. Offset value used.}
#'   \item{call}{The matched call.}
#'   \item{tract_id, point_id}{ID column names.}
#'   \item{interp_cols}{Character vector. Interpolated column names.}
#'   \item{calib_cols}{List with `$tracts` and `$sources`.}
#'   \item{weights}{Weight matrix or NULL (opt-in via `keep`).}
#'   \item{time_matrix}{Travel time matrix or NULL (opt-in via `keep`).}
#'   \item{electoral_sf}{`sf` point object or NULL (opt-in via `keep`).}
#'   \item{pop_raster}{Population raster or NULL (opt-in via `keep`).}
#'   \item{rep_points}{Representative points or NULL (opt-in via `keep`).}
#' }
#'
#' @examples
#' \dontrun{
#' # Minimal: sf objects + column names (auto-downloads OSM)
#' result <- interpolate_election(
#'   tracts_sf    = census_tracts,
#'   electoral_sf = voting_stations,
#'   calib_tracts  = c("pop_18_24", "pop_25_34"),
#'   calib_sources = c("voters_18_24", "voters_25_34")
#' )
#'
#' # With pre-computed travel times (skip r5r)
#' result <- interpolate_election(
#'   tracts_sf    = census_tracts,
#'   electoral_sf = voting_stations,
#'   calib_tracts  = c("pop_young", "pop_old"),
#'   calib_sources = c("voters_young", "voters_old"),
#'   time_matrix  = my_tt_matrix
#' )
#'
#' # GPU optimization with custom routing
#' result <- interpolate_election(
#'   tracts_sf    = census_tracts,
#'   electoral_sf = voting_stations,
#'   calib_tracts  = c("pop_young", "pop_old"),
#'   calib_sources = c("voters_young", "voters_old"),
#'   optim   = optim_control(use_gpu = TRUE),
#'   routing = routing_control(mode = c("WALK", "TRANSIT"))
#' )
#' }
#'
#' @family wrappers
#'
#' @seealso [optim_control()], [routing_control()],
#'   [optimize_alpha()], [compute_weight_matrix()],
#'   [interpolate_election_br()] for the Brazilian-specific wrapper.
#'
#' @export
interpolate_election <- function(
    tracts_sf,
    electoral_sf,
    # -- Calibration --
    calib_tracts,
    calib_sources,
    interp_sources     = NULL,
    # -- ID columns --
    tract_id           = "id",
    point_id           = "id",
    # -- Pre-computed input --
    time_matrix        = NULL,
    # -- Control --
    optim              = optim_control(),
    routing            = routing_control(),
    # -- Filtering --
    min_tract_pop      = 1,
    # -- Output --
    offset             = 1,
    keep               = NULL,
    verbose            = TRUE,
    ...
) {
  cl <- match.call()
  dots <- list(...)

  # --- Backward compatibility: detect old-style params in ... ---
  optim_param_names <- c("alpha_init", "max_epochs", "lr_init", "use_gpu",
                         "device", "dtype", "convergence_tol", "patience",
                         "barrier_mu", "alpha_min")
  routing_param_names <- c("point_method", "pop_raster",
                           "min_area_for_pop_weight", "mode",
                           "max_trip_duration", "fill_missing",
                           "n_threads", "departure_datetime",
                           "gtfs_zip_path", "osm_buffer_km")
  old_optim <- intersect(names(dots), optim_param_names)
  old_routing <- intersect(names(dots), routing_param_names)
  if (length(old_optim) > 0) {
    warning(
      "Passing optimization parameters directly is deprecated.\n",
      "Use optim = optim_control(...) instead.\n",
      "Deprecated parameters: ", paste(old_optim, collapse = ", "),
      call. = FALSE
    )
    ctrl_list <- unclass(optim)
    ctrl_list[old_optim] <- dots[old_optim]
    optim <- do.call(optim_control, ctrl_list)
  }
  if (length(old_routing) > 0) {
    warning(
      "Passing routing parameters directly is deprecated.\n",
      "Use routing = routing_control(...) instead.\n",
      "Deprecated parameters: ", paste(old_routing, collapse = ", "),
      call. = FALSE
    )
    ctrl_list <- unclass(routing)
    ctrl_list[old_routing] <- dots[old_routing]
    routing <- do.call(routing_control, ctrl_list)
  }

  # Extract from control objects
  use_gpu             <- optim$use_gpu
  mode                <- routing$mode
  max_trip_duration   <- routing$max_trip_duration
  point_method        <- routing$point_method
  osm_buffer_km       <- routing$osm_buffer_km

  # Extract internal progress tracking from dots
  .progress <- dots$.progress %||% list(offset = 0L, total = NULL)
  .step_offset <- .progress$offset
  .step_total <- .progress$total

  # Extract advanced args from dots
  network_path   <- dots$network_path
  elevation_path <- dots$elevation_path

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

  # --- Filter census tracts by min_tract_pop ---
  if (min_tract_pop > 0) {
    pop_total <- rowSums(tracts_df[, calib_tracts, drop = FALSE], na.rm = TRUE)
    keep_rows <- pop_total >= min_tract_pop
    if (sum(keep_rows) < nrow(tracts_sf)) {
      n_removed <- sum(!keep_rows)
      tracts_sf <- tracts_sf[keep_rows, ]
      tracts_df <- sf::st_drop_geometry(tracts_sf)
      if (verbose) {
        message(sprintf("  Filtered %d census tracts with pop < %g (%d remaining)",
                        n_removed, min_tract_pop, nrow(tracts_sf)))
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
  pop_raster_obj <- NULL   # populated on cache miss when pop_weighted
  rep_points_obj <- NULL   # populated on cache miss

  if (is.null(time_matrix)) {
    # Check for cached travel time matrix
    # Use actual (unsorted) IDs — the matrix is positional, so row/col order matters
    zone_ids <- as.character(tracts_df[[tract_id]])
    point_ids <- as.character(elec_df[[point_id]])
    # Include routing params in cache key so different modes/methods
    # don't collide
    cache_input <- paste(
      c(zone_ids, "||", point_ids, "||",
        mode, max_trip_duration, point_method),
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
      r5r_data <- download_r5r_data(
        area_sf = expanded_area, output_dir = r5r_dir,
        elevation = FALSE, verbose = verbose
      )
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

      time_matrix <- compute_travel_times(
        tracts_sf = tracts_sf, points_sf = electoral_sf,
        network_path = network_path,
        tract_id = tract_id, point_id = point_id,
        routing = routing,
        verbose = verbose
      )

      # Extract heavy/non-serializable attributes before caching
      # (SpatRaster and sf objects should not be serialized inside the
      # travel time RDS cache)
      pop_raster_obj <- attr(time_matrix, "pop_raster")
      attr(time_matrix, "pop_raster") <- NULL
      rep_points_obj <- attr(time_matrix, "rep_points")
      attr(time_matrix, "rep_points") <- NULL

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
  # Compute row_targets for population-proportional allocation
  pop_total <- rowSums(pop_matrix)
  row_targets <- pop_total / sum(pop_total) * ncol(time_matrix)

  optim_result <- NULL
  if (verbose) message(sprintf("[%d/%d] Optimizing alpha...",
                               step_num, total_steps))
  optim_result <- optimize_alpha(
    time_matrix = time_matrix, pop_matrix = pop_matrix,
    source_matrix = source_matrix, row_targets = row_targets,
    optim = optim,
    offset = offset, verbose = verbose
  )
  alpha <- optim_result$alpha
  step_num <- step_num + 1L

  # --- Step 5: Interpolate ---
  if (verbose) message(sprintf("[%d/%d] Interpolating...",
                               step_num, total_steps))
  if (!is.null(optim_result) && !is.null(optim_result$W)) {
    W <- optim_result$W
  } else {
    W <- compute_weight_matrix(time_matrix, alpha, pop_matrix, source_matrix,
                                offset = offset, use_gpu = optim$use_gpu)
  }
  interpolated <- W %*% interp_data
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
    row_targets   = row_targets,
    # Always kept (needed for reinterpolate, residuals, etc.)
    weights       = W,
    time_matrix   = time_matrix,
    # Heavy objects (opt-in)
    electoral_sf  = if ("electoral_sf" %in% keep) electoral_sf else NULL,
    pop_raster    = if ("pop_raster" %in% keep) pop_raster_obj else NULL,
    rep_points    = if ("rep_points" %in% keep) rep_points_obj else NULL,
    # Brazilian metadata (NULL when called generically)
    code_muni      = NULL,
    nome_municipio = NULL,
    code_muni_tse  = NULL,
    uf             = NULL,
    turno          = NULL,
    cargo          = NULL,
    year           = NULL,
    census_year    = NULL,
    what           = NULL,
    pop_data       = NULL,
    dictionary     = NULL,
    # Spatial data for plotting (set by interpolate_election_br)
    muni_boundary  = NULL,
    neighborhoods  = NULL
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
      info_line <- sprintf("  IBGE: %s | TSE: %s | Election: %d | Census: %d",
                           x$code_muni, x$code_muni_tse, x$year, x$census_year)
      if (!is.null(x$turno))
        info_line <- paste0(info_line, sprintf(" | Turno: %d", x$turno))
      if (!is.null(x$cargo)) {
        cargo_str <- paste(
          vapply(x$cargo, .br_cargo_label, character(1)),
          collapse = ", ")
        info_line <- paste0(info_line, sprintf(" | Cargo: %s", cargo_str))
      }
      cat(info_line, "\n")
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
  has_s <- !is.null(x$electoral_sf)
  has_p <- !is.null(x$pop_raster)
  has_r <- !is.null(x$rep_points)
  if (has_w) cat("    result$weights         weight matrix\n")
  if (has_t) cat("    result$time_matrix     travel time matrix\n")
  if (has_s) cat("    result$electoral_sf    electoral locations (sf)\n")
  if (has_p) cat("    result$pop_raster      population density raster\n")
  if (has_r) cat("    result$rep_points      representative points (sf)\n")

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

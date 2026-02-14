#' One-step IDW interpolation from source points to target zones
#'
#' High-level wrapper that combines travel-time computation (optional),
#' alpha optimization, and interpolation into a single call. If no travel
#' time matrix is provided, OSM road network data is automatically
#' downloaded and travel times are computed via r5r.
#'
#' @param tracts_sf An `sf` polygon object. Target zones (e.g., census tracts).
#' @param electoral_sf An `sf` point object. Source points (e.g., voting
#'   locations).
#' @param zone_id Character. Name of the ID column in `tracts_sf`.
#' @param point_id Character. Name of the ID column in `electoral_sf`.
#' @param calib_zones Character vector. Column names in `tracts_sf` to use
#'   as the calibration population matrix. Must match `calib_sources` in
#'   length.
#' @param calib_sources Character vector. Column names in `electoral_sf` to
#'   use as the source calibration matrix. Must match `calib_zones` in
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
#' @param min_pop Numeric. Minimum total population in `calib_zones` for a
#'   zone to be included. Default: 1.
#' @param alpha Numeric vector of length n, or NULL. Pre-computed decay
#'   parameters. If provided, optimization is skipped.
#' @param offset Numeric. Travel time offset. Default: 1.
#' @param use_gpu Logical or NULL. Passed to [optimize_alpha()].
#' @param verbose Logical. Print progress. Default: TRUE.
#' @param ... Additional arguments forwarded to [optimize_alpha()],
#'   [compute_travel_times()], and/or [download_r5r_data()].
#'
#' @return A list of class `"interpElections_result"` with components:
#' \describe{
#'   \item{interpolated}{Numeric matrix \[n x p\]. Interpolated values.}
#'   \item{alpha}{Numeric vector of length n. Decay parameters used.}
#'   \item{weights}{Numeric matrix \[n x m\]. Column-standardized weight
#'     matrix.}
#'   \item{optimization}{`interpElections_optim` object, or NULL if `alpha` was
#'     pre-supplied.}
#'   \item{time_matrix}{Numeric matrix \[n x m\]. Travel times used.}
#'   \item{offset}{Numeric. Offset value used.}
#'   \item{call}{The matched call.}
#' }
#'
#' @examples
#' \dontrun{
#' # Minimal: sf objects + column names (auto-downloads OSM)
#' result <- interpolate_election(
#'   tracts_sf    = census_tracts,
#'   electoral_sf = voting_stations,
#'   zone_id      = "code_tract",
#'   point_id     = "id",
#'   calib_zones  = c("pop_18_24", "pop_25_34"),
#'   calib_sources = c("voters_18_24", "voters_25_34")
#' )
#'
#' # With pre-computed travel times (skip r5r)
#' result <- interpolate_election(
#'   tracts_sf    = census_tracts,
#'   electoral_sf = voting_stations,
#'   zone_id      = "code_tract",
#'   point_id     = "id",
#'   calib_zones  = c("pop_young", "pop_old"),
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
    zone_id,
    point_id,
    calib_zones,
    calib_sources,
    interp_sources = NULL,
    time_matrix    = NULL,
    network_path   = NULL,
    elevation_path = NULL,
    osm_buffer_km  = 10,
    min_pop        = 1,
    alpha          = NULL,
    offset         = 1,
    use_gpu        = NULL,
    verbose        = TRUE,
    ...
) {
  cl <- match.call()
  dots <- list(...)

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required", call. = FALSE)
  }

  # --- Validate inputs ---
  if (!inherits(tracts_sf, "sf")) {
    stop("'tracts_sf' must be an sf object", call. = FALSE)
  }
  if (!inherits(electoral_sf, "sf")) {
    stop("'electoral_sf' must be an sf object", call. = FALSE)
  }
  if (length(calib_zones) != length(calib_sources)) {
    stop(sprintf(
      "calib_zones (%d) and calib_sources (%d) must have the same length",
      length(calib_zones), length(calib_sources)
    ), call. = FALSE)
  }

  # Check columns exist
  tracts_df <- sf::st_drop_geometry(tracts_sf)
  elec_df <- sf::st_drop_geometry(electoral_sf)

  missing_z <- setdiff(calib_zones, names(tracts_df))
  if (length(missing_z) > 0) {
    stop("calib_zones columns not found in tracts_sf: ",
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
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(FALSE)

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

  # --- Filter zones by min_pop ---
  if (min_pop > 0) {
    pop_total <- rowSums(tracts_df[, calib_zones, drop = FALSE], na.rm = TRUE)
    keep <- pop_total >= min_pop
    if (sum(keep) < nrow(tracts_sf)) {
      n_removed <- sum(!keep)
      tracts_sf <- tracts_sf[keep, ]
      tracts_df <- sf::st_drop_geometry(tracts_sf)
      if (verbose) {
        message(sprintf("Filtered %d zones with pop < %g (%d remaining)",
                        n_removed, min_pop, nrow(tracts_sf)))
      }
    }
  }

  # --- Step 1: Resolve travel time matrix ---
  # Step count depends on path taken:
  #   Pre-computed matrix: 3 steps (matrix, optimize, interpolate)
  #   User-provided network: 4 steps (+ compute travel times)
  #   Auto-download OSM:    5 steps (+ download OSM + compute travel times)
  step_num <- 1L
  total_steps <- 3L

  if (is.null(time_matrix)) {
    # Check for cached travel time matrix
    # Use actual (unsorted) IDs — the matrix is positional, so row/col order matters
    zone_ids <- as.character(tracts_df[[zone_id]])
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
      # Cached travel times — 3 steps total
      if (verbose) message(sprintf(
        "[%d/%d] Using cached travel time matrix (%dx%d)",
        step_num, total_steps, nrow(time_matrix), ncol(time_matrix)
      ))
      step_num <- step_num + 1L
    } else if (is.null(network_path)) {
      # Need to download OSM + compute — 5 steps total
      total_steps <- 5L
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
      # User provided network_path — 4 steps total
      total_steps <- 4L
    }

    # Compute travel times if not cached
    if (is.null(time_matrix)) {
      if (verbose) message(sprintf("[%d/%d] Computing travel times...",
                                   step_num, total_steps))

      tt_args <- .extract_args(dots, compute_travel_times)
      time_matrix <- do.call(compute_travel_times, c(
        list(zones_sf = tracts_sf, points_sf = electoral_sf,
             network_path = network_path,
             zone_id = zone_id, point_id = point_id,
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
  pop_matrix <- as.matrix(tracts_df[, calib_zones, drop = FALSE])
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
           use_gpu = use_gpu, verbose = verbose),
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

  result <- list(
    interpolated = interpolated,
    alpha = alpha,
    weights = W_std,
    optimization = optim_result,
    time_matrix = time_matrix,
    offset = offset,
    call = cl
  )
  class(result) <- "interpElections_result"

  if (verbose) {
    message(sprintf("Done. %d variables interpolated into %d zones.",
                    ncol(interpolated), nrow(interpolated)))
  }

  result
}

#' @export
print.interpElections_result <- function(x, ...) {
  cat("interpElections interpolation result\n")
  cat(sprintf("  Zones:     %d\n", nrow(x$interpolated)))
  cat(sprintf("  Sources:   %d\n", ncol(x$time_matrix)))
  cat(sprintf("  Variables: %d\n", ncol(x$interpolated)))
  if (!is.null(x$optimization)) {
    cat(sprintf("  Optimizer: %s (obj = %.2f)\n",
                x$optimization$method, x$optimization$value))
  } else {
    cat("  Optimizer: skipped (alpha provided)\n")
  }
  cat(sprintf("  Alpha:     [%.3f, %.3f] (mean %.3f)\n",
              min(x$alpha), max(x$alpha), mean(x$alpha)))
  invisible(x)
}

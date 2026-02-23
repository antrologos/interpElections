#' Compute a travel-time matrix from census tract representative points to source points
#'
#' Builds a travel-time matrix using the r5r routing engine. Computes
#' travel times from representative points of target census tracts
#' to geolocated source points (e.g., polling locations).
#'
#' @param tracts_sf An `sf` object with polygon geometries. Target census
#'   tracts.
#' @param points_sf An `sf` object with point geometries. Source points.
#' @param network_path Character. Path to the directory containing the
#'   OSM `.pbf` file for building the r5r network.
#' @param tract_id Character. Name of the ID column in `tracts_sf`.
#'   Default: `"id"`.
#' @param point_id Character. Name of the ID column in `points_sf`.
#'   Default: `"id"`.
#' @param point_method Character. Method for computing representative points
#'   for census tracts. One of `"point_on_surface"` (default),
#'   `"centroid"`, or `"pop_weighted"`. See [compute_representative_points()]
#'   for details.
#' @param pop_raster A [terra::SpatRaster] object, a file path to a GeoTIFF,
#'   or `NULL`. Population density raster for `point_method = "pop_weighted"`.
#'   If `NULL`, WorldPop data is auto-downloaded. Ignored for other methods.
#' @param min_area_for_pop_weight Numeric. Minimum tract area in km² for applying the
#'   population-weighted method. Default: 1.
#' @param mode Character. Routing mode. Default: `"WALK"`.
#' @param max_trip_duration Integer. Maximum trip duration in minutes.
#'   Default: 300.
#' @param fill_missing Numeric. Value to fill for unreachable
#'   origin-destination pairs. Default: same as `max_trip_duration`.
#' @param n_threads Integer. Number of r5r routing threads. Default: 4.
#' @param departure_datetime POSIXct or NULL. Departure time for
#'   transit-based routing. Required when `mode` includes transit
#'   components. Default: NULL (ignored for WALK/BICYCLE modes).
#' @param gtfs_zip_path Character or NULL. Path to a GTFS `.zip` file for
#'   transit routing. When provided, the file is copied into `network_path`
#'   so that r5r can auto-detect it. Default: NULL.
#' @param verbose Logical. Default: TRUE.
#'
#' @return A numeric matrix \[n_tracts x n_points\]. Travel times in minutes.
#'   Row names = census tract IDs, column names = point IDs. Unreachable
#'   pairs are filled with `fill_missing`.
#'
#' @details
#' Requires the `r5r` and `sf` packages. r5r requires exactly Java/JDK 21.
#' Use [download_r5r_data()] to obtain the OSM data needed for `network_path`.
#'
#' @examples
#' \dontrun{
#' tt <- compute_travel_times(
#'   tracts_sf = tracts, points_sf = stations,
#'   network_path = "path/to/osm_data",
#'   tract_id = "code_tract", point_id = "id"
#' )
#' }
#'
#' @family spatial
#'
#' @seealso [download_r5r_data()] to download OSM and elevation data.
#'
#' @export
compute_travel_times <- function(
    tracts_sf,
    points_sf,
    network_path,
    tract_id = "id",
    point_id = "id",
    point_method = "point_on_surface",
    pop_raster = NULL,
    min_area_for_pop_weight = 1,
    mode = "WALK",
    max_trip_duration = 300L,
    fill_missing = max_trip_duration,
    n_threads = 4L,
    departure_datetime = NULL,
    gtfs_zip_path = NULL,
    verbose = TRUE
) {
  if (!requireNamespace("r5r", quietly = TRUE)) {
    stop("The 'r5r' package is required for compute_travel_times()",
         call. = FALSE)
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for compute_travel_times()",
         call. = FALSE)
  }

  # Java pre-flight check: verify Java is present and version == 21
  java_check <- tryCatch(
    system2("java", "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(java_check)) {
    stop(
      "Java not found. r5r requires exactly Java/JDK 21.\n",
      "Run interpElections::check_r5r() for diagnostics, or\n",
      "Run interpElections::setup_java() to download and install Java 21.",
      call. = FALSE
    )
  }
  java_ver <- .parse_java_version(java_check)
  if (!is.na(java_ver) && java_ver != 21L) {
    stop(
      sprintf("Java %d found, but r5r requires exactly Java 21.\n", java_ver),
      "Run interpElections::setup_java() to download and install Java 21.",
      call. = FALSE
    )
  }

  # Validate inputs
  if (!inherits(tracts_sf, "sf")) {
    stop("'tracts_sf' must be an sf object", call. = FALSE)
  }
  if (!inherits(points_sf, "sf")) {
    stop("'points_sf' must be an sf object", call. = FALSE)
  }
  if (nrow(tracts_sf) == 0) {
    stop("tracts_sf must not be empty", call. = FALSE)
  }
  if (nrow(points_sf) == 0) {
    stop("points_sf must not be empty", call. = FALSE)
  }
  if (!tract_id %in% names(tracts_sf)) {
    stop(sprintf("tract_id column '%s' not found in tracts_sf", tract_id),
         call. = FALSE)
  }
  if (!point_id %in% names(points_sf)) {
    stop(sprintf("point_id column '%s' not found in points_sf", point_id),
         call. = FALSE)
  }

  # Check for duplicate IDs
  z_ids <- as.character(tracts_sf[[tract_id]])
  p_ids <- as.character(points_sf[[point_id]])
  if (anyDuplicated(z_ids)) {
    stop("Duplicate IDs found in tracts_sf[[tract_id]]", call. = FALSE)
  }
  if (anyDuplicated(p_ids)) {
    stop("Duplicate IDs found in points_sf[[point_id]]", call. = FALSE)
  }

  # Compute representative points for tracts
  rep_points <- compute_representative_points(
    tracts_sf = tracts_sf,
    method = point_method,
    pop_raster = pop_raster,
    min_area_for_pop_weight = min_area_for_pop_weight,
    tract_id = tract_id,
    verbose = verbose
  )
  # rep_points already in WGS84 (guaranteed by compute_representative_points)
  points_sf <- sf::st_transform(points_sf, 4326)

  # Prepare origin/destination data for r5r
  origins <- data.frame(
    id = as.character(rep_points[[tract_id]]),
    lon = sf::st_coordinates(rep_points)[, 1],
    lat = sf::st_coordinates(rep_points)[, 2]
  )
  destinations <- data.frame(
    id = as.character(points_sf[[point_id]]),
    lon = sf::st_coordinates(points_sf)[, 1],
    lat = sf::st_coordinates(points_sf)[, 2]
  )

  # Copy GTFS zip into network directory if provided
  if (!is.null(gtfs_zip_path)) {
    if (!file.exists(gtfs_zip_path)) {
      stop(sprintf("GTFS file not found: %s", gtfs_zip_path), call. = FALSE)
    }
    file.copy(gtfs_zip_path,
              file.path(network_path, basename(gtfs_zip_path)),
              overwrite = TRUE)
    if (verbose) message("  Copied GTFS file to network directory")
  }

  # Build r5r network
  if (verbose) message("  Building r5r network...")
  r5r_core <- suppressMessages(r5r::setup_r5(
    data_path = network_path,
    verbose = FALSE
  ))
  on.exit(suppressMessages(r5r::stop_r5(r5r_core)), add = TRUE)

  # Build travel time call args
  tt_args <- list(
    r5r_core,
    origins = origins,
    destinations = destinations,
    mode = mode,
    max_trip_duration = max_trip_duration,
    verbose = FALSE,
    max_rides = 1L
  )

  # Pass departure_datetime if provided (needed for transit modes)
  if (!is.null(departure_datetime)) {
    tt_args$departure_datetime <- departure_datetime
  }

  # Calculate travel times
  if (verbose) message("  Computing travel times...")
  tt <- suppressMessages(do.call(r5r::travel_time_matrix, tt_args))

  # Vectorized pivot to wide matrix
  tract_ids <- origins$id
  point_ids <- destinations$id

  mat <- matrix(fill_missing,
                nrow = length(tract_ids),
                ncol = length(point_ids),
                dimnames = list(tract_ids, point_ids))

  # Detect whether r5r swapped origins and destinations.
  # r5r may internally route from the smaller set for efficiency,
  # returning from_id = original destinations and to_id = original origins.
  from_ids <- as.character(tt$from_id)
  to_ids   <- as.character(tt$to_id)
  fwd_tract <- sum(from_ids %in% tract_ids)
  rev_tract <- sum(to_ids   %in% tract_ids)

  if (fwd_tract >= rev_tract) {
    # Normal: from_id = tracts, to_id = points
    row_idx <- match(from_ids, tract_ids)
    col_idx <- match(to_ids, point_ids)
  } else {
    # Swapped: from_id = points, to_id = tracts
    if (verbose) {
      message("  Note: r5r swapped origins/destinations; adjusting ID mapping")
    }
    row_idx <- match(to_ids, tract_ids)
    col_idx <- match(from_ids, point_ids)
  }
  valid <- !is.na(row_idx) & !is.na(col_idx)

  # r5r column name varies by version
  tt_col <- intersect(
    c("travel_time_p50", "travel_time"),
    names(tt)
  )
  if (length(tt_col) == 0) {
    stop("r5r output missing travel time column. ",
         "Found: ", paste(names(tt), collapse = ", "),
         call. = FALSE)
  }
  tt_values <- tt[[tt_col[1]]]

  n_valid <- sum(valid)
  n_total <- length(tract_ids) * length(point_ids)

  if (n_valid == 0) {
    warning(
      "r5r returned no usable travel times. ",
      "The entire matrix is filled with fill_missing (", fill_missing, ").\n",
      "This will produce constant interpolation weights. ",
      "Check that the OSM network covers the study area and that ",
      "origins/destinations snap to the street network.",
      call. = FALSE
    )
  } else if (n_valid < n_total * 0.1) {
    warning(
      sprintf("r5r returned travel times for only %.1f%% of OD pairs (%d/%d). ",
              100 * n_valid / n_total, n_valid, n_total),
      "Most entries are fill_missing. Check network coverage.",
      call. = FALSE
    )
  }

  if (any(valid)) {
    mat[cbind(row_idx[valid], col_idx[valid])] <-
      tt_values[valid]
  }

  # Replace any remaining NA with fill_missing
  mat[is.na(mat)] <- fill_missing

  # Diagnostic: detect tracts where ALL travel times equal fill_missing
  all_filled <- rowSums(mat == fill_missing) == ncol(mat)
  n_unreachable <- sum(all_filled)
  if (n_unreachable > 0) {
    unreachable_ids <- tract_ids[all_filled]
    warning(
      sprintf(
        "%d tract(s) have ALL travel times equal to fill_missing (%g min). ",
        n_unreachable, fill_missing
      ),
      "These tracts will receive near-zero interpolation weight. ",
      "This usually means their representative point is not routable ",
      "(e.g., falls in a park, river, or area without OSM road coverage). ",
      "Consider using point_method = 'pop_weighted' or 'point_on_surface'.",
      call. = FALSE
    )
    attr(mat, "unreachable_tracts") <- unreachable_ids
  }

  # Propagate attributes from representative points
  rp_raster <- attr(rep_points, "pop_raster")
  if (!is.null(rp_raster)) {
    attr(mat, "pop_raster") <- rp_raster
  }
  no_pop <- attr(rep_points, "no_pop_tracts")
  if (!is.null(no_pop)) {
    attr(mat, "no_pop_tracts") <- no_pop
  }
  attr(mat, "rep_points") <- rep_points

  if (verbose) {
    pct <- 100 * n_valid / n_total
    message(sprintf("  Travel time matrix: %d x %d (%d/%d = %.1f%% actual values)",
                    nrow(mat), ncol(mat), n_valid, n_total, pct))
  }

  mat
}

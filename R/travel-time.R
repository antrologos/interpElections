#' Compute a travel-time matrix from zone centroids to source points
#'
#' Builds a travel-time matrix using the r5r routing engine. Computes
#' travel times from the centroids of target zones (e.g., census tracts)
#' to geolocated source points (e.g., polling locations).
#'
#' @param zones_sf An `sf` object with polygon geometries. Target zones.
#' @param points_sf An `sf` object with point geometries. Source points.
#' @param network_path Character. Path to the directory containing the
#'   OSM `.pbf` file for building the r5r network.
#' @param zone_id Character. Name of the ID column in `zones_sf`.
#'   Default: `"id"`.
#' @param point_id Character. Name of the ID column in `points_sf`.
#'   Default: `"id"`.
#' @param mode Character. Routing mode. Default: `"WALK"`.
#' @param max_trip_duration Integer. Maximum trip duration in minutes.
#'   Default: 300.
#' @param fill_missing Numeric. Value to fill for unreachable
#'   origin-destination pairs. Default: same as `max_trip_duration`.
#' @param n_threads Integer. Number of r5r routing threads. Default: 4.
#' @param departure_datetime POSIXct or NULL. Departure time for
#'   transit-based routing. Required when `mode` includes transit
#'   components. Default: NULL (ignored for WALK/BICYCLE modes).
#' @param verbose Logical. Default: TRUE.
#'
#' @return A numeric matrix \[n_zones x n_points\]. Travel times in minutes.
#'   Row names = zone IDs, column names = point IDs. Unreachable pairs
#'   are filled with `fill_missing`.
#'
#' @details
#' Requires the `r5r` and `sf` packages. r5r requires Java/JDK 21+.
#' Use [download_r5r_data()] to obtain the OSM data needed for `network_path`.
#'
#' @examples
#' \dontrun{
#' tt <- compute_travel_times(
#'   zones_sf = tracts, points_sf = stations,
#'   network_path = "path/to/osm_data",
#'   zone_id = "code_tract", point_id = "id"
#' )
#' }
#'
#' @family spatial
#'
#' @seealso [download_r5r_data()] to download OSM and elevation data.
#'
#' @export
compute_travel_times <- function(
    zones_sf,
    points_sf,
    network_path,
    zone_id = "id",
    point_id = "id",
    mode = "WALK",
    max_trip_duration = 300L,
    fill_missing = max_trip_duration,
    n_threads = 4L,
    departure_datetime = NULL,
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

  # Java pre-flight check: verify Java is present and version >= 21
  java_check <- tryCatch(
    system2("java", "-version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(java_check)) {
    stop(
      "Java not found. r5r requires Java/JDK 21+.\n",
      "Run interpElections::check_r5r() for diagnostics, or\n",
      "Run interpElections::setup_java() to download and install Java 21.",
      call. = FALSE
    )
  }
  java_ver <- .parse_java_version(java_check)
  if (!is.na(java_ver) && java_ver < 21L) {
    stop(
      sprintf("Java %d found, but r5r requires Java 21+.\n", java_ver),
      "Run interpElections::setup_java() to download and install Java 21.",
      call. = FALSE
    )
  }

  # Validate inputs
  if (!inherits(zones_sf, "sf")) {
    stop("'zones_sf' must be an sf object", call. = FALSE)
  }
  if (!inherits(points_sf, "sf")) {
    stop("'points_sf' must be an sf object", call. = FALSE)
  }
  if (nrow(zones_sf) == 0) {
    stop("zones_sf must not be empty", call. = FALSE)
  }
  if (nrow(points_sf) == 0) {
    stop("points_sf must not be empty", call. = FALSE)
  }
  if (!zone_id %in% names(zones_sf)) {
    stop(sprintf("zone_id column '%s' not found in zones_sf", zone_id),
         call. = FALSE)
  }
  if (!point_id %in% names(points_sf)) {
    stop(sprintf("point_id column '%s' not found in points_sf", point_id),
         call. = FALSE)
  }

  # Check for duplicate IDs
  z_ids <- as.character(zones_sf[[zone_id]])
  p_ids <- as.character(points_sf[[point_id]])
  if (anyDuplicated(z_ids)) {
    stop("Duplicate IDs found in zones_sf[[zone_id]]", call. = FALSE)
  }
  if (anyDuplicated(p_ids)) {
    stop("Duplicate IDs found in points_sf[[point_id]]", call. = FALSE)
  }

  # Project to equal-area CRS for centroid computation, then to WGS84
  if (verbose) message("  Computing zone centroids...")
  zones_proj <- sf::st_transform(zones_sf, "EPSG:5880")
  centroids_proj <- suppressWarnings(sf::st_centroid(zones_proj))
  centroids <- sf::st_transform(centroids_proj, 4326)
  points_sf <- sf::st_transform(points_sf, 4326)

  # Prepare origin/destination data for r5r
  origins <- data.frame(
    id = as.character(centroids[[zone_id]]),
    lon = sf::st_coordinates(centroids)[, 1],
    lat = sf::st_coordinates(centroids)[, 2]
  )
  destinations <- data.frame(
    id = as.character(points_sf[[point_id]]),
    lon = sf::st_coordinates(points_sf)[, 1],
    lat = sf::st_coordinates(points_sf)[, 2]
  )

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
  zone_ids <- origins$id
  point_ids <- destinations$id

  mat <- matrix(fill_missing,
                nrow = length(zone_ids),
                ncol = length(point_ids),
                dimnames = list(zone_ids, point_ids))

  # Detect whether r5r swapped origins and destinations.
  # r5r may internally route from the smaller set for efficiency,
  # returning from_id = original destinations and to_id = original origins.
  from_ids <- as.character(tt$from_id)
  to_ids   <- as.character(tt$to_id)
  fwd_zone <- sum(from_ids %in% zone_ids)
  rev_zone <- sum(to_ids   %in% zone_ids)

  if (fwd_zone >= rev_zone) {
    # Normal: from_id = zones, to_id = points
    row_idx <- match(from_ids, zone_ids)
    col_idx <- match(to_ids, point_ids)
  } else {
    # Swapped: from_id = points, to_id = zones
    if (verbose) {
      message("  Note: r5r swapped origins/destinations; adjusting ID mapping")
    }
    row_idx <- match(to_ids, zone_ids)
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
  n_total <- length(zone_ids) * length(point_ids)

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

  if (verbose) {
    pct <- 100 * n_valid / n_total
    message(sprintf("  Travel time matrix: %d x %d (%d/%d = %.1f%% actual values)",
                    nrow(mat), ncol(mat), n_valid, n_total, pct))
  }

  mat
}

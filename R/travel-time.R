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
#' @param routing A [routing_control()] object with routing parameters
#'   (mode, point_method, max_trip_duration, n_threads, gtfs_zip_path,
#'   departure_datetime, pop_raster, min_area_for_pop_weight,
#'   osm_buffer_km, fill_missing). Default: `routing_control()`.
#' @param verbose Logical. Default: TRUE.
#' @param ... **Deprecated**. Old-style individual parameters
#'   (`point_method`, `pop_raster`, `min_area_for_pop_weight`, `mode`,
#'   `max_trip_duration`, `fill_missing`, `n_threads`,
#'   `departure_datetime`, `gtfs_zip_path`) are still accepted via
#'   `...` for backward compatibility but will be removed in a future
#'   release. Use `routing = routing_control(...)` instead.
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
#'
#' # Transit mode with GTFS
#' tt <- compute_travel_times(
#'   tracts_sf = tracts, points_sf = stations,
#'   network_path = "path/to/osm_data",
#'   routing = routing_control(
#'     mode = c("WALK", "TRANSIT"),
#'     gtfs_zip_path = "sptrans.zip",
#'     departure_datetime = as.POSIXct("2022-10-02 10:00:00")
#'   )
#' )
#' }
#'
#' @family spatial
#'
#' @seealso [routing_control()] for routing parameters,
#'   [download_r5r_data()] to download OSM and elevation data.
#'
#' @export
compute_travel_times <- function(
    tracts_sf,
    points_sf,
    network_path,
    tract_id = "id",
    point_id = "id",
    routing = routing_control(),
    verbose = TRUE,
    ...
) {
  # --- Backward compatibility: detect old-style params in ... ---
  dots <- list(...)
  routing_param_names <- c("point_method", "pop_raster",
                           "min_area_for_pop_weight", "mode",
                           "max_trip_duration", "fill_missing",
                           "n_threads", "departure_datetime",
                           "gtfs_zip_path", "osm_buffer_km")
  old_params <- intersect(names(dots), routing_param_names)
  if (length(old_params) > 0) {
    warning(
      "Passing routing parameters directly is deprecated.\n",
      "Use routing = routing_control(...) instead.\n",
      "Deprecated parameters: ", paste(old_params, collapse = ", "),
      call. = FALSE
    )
    ctrl_list <- unclass(routing)
    ctrl_list[old_params] <- dots[old_params]
    routing <- do.call(routing_control, ctrl_list)
  }

  # Extract from control object
  point_method            <- routing$point_method
  pop_raster              <- routing$pop_raster
  min_area_for_pop_weight <- routing$min_area_for_pop_weight
  mode                    <- routing$mode
  max_trip_duration       <- routing$max_trip_duration
  fill_missing            <- routing$fill_missing
  n_threads               <- routing$n_threads
  departure_datetime      <- routing$departure_datetime
  gtfs_zip_path           <- routing$gtfs_zip_path
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
  java_missing <- is.null(java_check)
  java_wrong   <- FALSE
  java_ver     <- NA_integer_

  if (!java_missing) {
    java_ver   <- .parse_java_version(java_check)
    java_wrong <- !is.na(java_ver) && java_ver != 21L
  }

  if (java_missing || java_wrong) {
    if (java_missing) {
      issue_msg <- "Java not found. r5r requires exactly Java/JDK 21."
    } else {
      issue_msg <- sprintf(
        "Java %d found, but r5r requires exactly Java 21.", java_ver
      )
    }

    if (interactive()) {
      message(issue_msg)
      choice <- utils::menu(
        c("Yes, set up Java 21 automatically",
          "No, I will handle it myself"),
        title = "Would you like R to download and configure Java 21 for you?"
      )

      if (choice == 1L) {
        setup_java(.ask_consent = FALSE)

        # Re-verify after setup
        java_recheck <- tryCatch(
          system2("java", "-version", stdout = TRUE, stderr = TRUE),
          error = function(e) NULL, warning = function(w) NULL
        )
        if (is.null(java_recheck)) {
          stop(
            "Java setup completed but Java is still not accessible on PATH.\n",
            "Please restart R and try again.",
            call. = FALSE
          )
        }
        java_ver2 <- .parse_java_version(java_recheck)
        if (is.na(java_ver2) || java_ver2 != 21L) {
          stop(
            "Java setup completed but Java 21 is still not the active version.\n",
            "Please restart R and try again.",
            call. = FALSE
          )
        }
        if (verbose) message("Java 21 is now configured. Continuing...")
      } else {
        stop(
          issue_msg, "\n",
          "Run interpElections::setup_java() when ready.",
          call. = FALSE
        )
      }
    } else {
      stop(
        issue_msg, "\n",
        "Run interpElections::setup_java() to download and install Java 21.",
        call. = FALSE
      )
    }
  }

  # Auto-configure Java heap if not set (must happen before r5r loads the JVM)
  if (!"rJava" %in% loadedNamespaces()) {
    java_mem <- .get_java_memory()
    if (is.null(java_mem$configured)) {
      auto_size <- .recommend_heap_size()
      if (!is.null(auto_size)) {
        param <- paste0("-Xmx", auto_size)
        options(java.parameters = param)
        # Persist so future sessions use this setting automatically
        tryCatch(
          .persist_renviron_var("_JAVA_OPTIONS", param),
          error = function(e) NULL
        )
        if (verbose) {
          message("  Java heap auto-configured to ", auto_size,
                  " (based on ", java_mem$system_ram %||% "system", " RAM)")
        }
      }
    }
  } else {
    # JVM already running — check if heap is dangerously low
    java_mem <- .get_java_memory()
    if (is.null(java_mem$configured)) {
      warning(
        "Java heap is at JVM default (~256-512 MB), which may be too small for r5r.\n",
        "If R crashes, restart R and run:\n",
        "  interpElections::set_java_memory(\"4g\")\n",
        "before loading any packages.",
        call. = FALSE
      )
    }
  }

  # Now safe to load r5r (JVM will start with configured heap)
  if (!requireNamespace("r5r", quietly = TRUE)) {
    stop("The 'r5r' package is required for compute_travel_times()",
         call. = FALSE)
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
      "The entire matrix is NA (unreachable). ",
      "All interpolation weights will be zero. ",
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

  # Replace any remaining NA with fill_missing (if fill_missing is not NA)
  if (!is.na(fill_missing)) {
    mat[is.na(mat)] <- fill_missing
  }

  # Convert any Inf to NA (supports legacy fill_missing = Inf)
  if (any(is.infinite(mat))) {
    mat[is.infinite(mat)] <- NA_real_
  }

  # Diagnostic: detect tracts where ALL travel times are NA
  all_na <- rowSums(!is.na(mat)) == 0L
  n_unreachable <- sum(all_na)
  if (n_unreachable > 0) {
    unreachable_ids <- tract_ids[all_na]
    warning(
      sprintf(
        "%d tract(s) have no reachable stations (all travel times are NA). ",
        n_unreachable
      ),
      "These tracts will receive zero interpolation weight and will be ",
      "excluded from optimization. ",
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
    n_na <- sum(is.na(mat))
    msg <- sprintf(
      "  Travel time matrix: %d x %d (%d/%d = %.1f%% actual values)",
      nrow(mat), ncol(mat), n_valid, n_total, pct)
    if (n_na > 0) {
      msg <- sprintf("%s, %d NA (unreachable)", msg, n_na)
    }
    message(msg)
  }

  mat
}

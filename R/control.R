#' Optimization control parameters
#'
#' Creates a control object for the SGD optimizer in [optimize_alpha()].
#' All parameters have sensible defaults; override only what you need.
#'
#' @param max_epochs Integer. Maximum number of epochs (full passes through
#'   all tracts). The optimizer may stop earlier if convergence is detected.
#'   Default: 2000.
#' @param lr_init Numeric. Initial ADAM learning rate. Reduced automatically
#'   via ReduceLROnPlateau when the epoch loss plateaus. Default: 0.05.
#' @param convergence_tol Numeric. Relative change in epoch loss below which
#'   the optimizer considers the solution converged. Default: 1e-4.
#' @param patience Integer. Number of consecutive epochs with no improvement
#'   (at minimum learning rate) before early stopping. The LR scheduler
#'   uses `2 * patience` as its own patience. Default: 50.
#' @param barrier_mu Numeric. Strength of the log-barrier penalty that
#'   prevents any census tract from receiving zero predicted voters.
#'   Set to 0 to disable. Default: 10.
#' @param alpha_init Numeric scalar, vector of length n, or matrix \[n x k\].
#'   Initial guess for alpha. A scalar is recycled to all tracts and
#'   brackets. Default: 2.
#' @param alpha_min Numeric or NULL. Lower bound for alpha values. The
#'   reparameterization becomes `alpha = alpha_min + softplus(theta)`.
#'   If NULL (default), the bound is set based on `kernel`:
#'   1 for `"power"` (linear-or-steeper decay), 0 for `"exponential"`.
#' @param kernel Character. Kernel function for spatial decay.
#'   `"power"` (default): \eqn{K(t) = (t + \text{offset})^{-\alpha}},
#'   the classic inverse distance weighting kernel.
#'   `"exponential"`: \eqn{K(t) = \exp(-\alpha \cdot t)}, which has a
#'   light tail (relative decay increases with distance). The exponential
#'   kernel does not need an offset and allows `alpha_min = 0`.
#' @param use_gpu Logical or NULL. If `TRUE`, use GPU (CUDA or MPS). If
#'   `FALSE`, use CPU. If `NULL` (default), reads the package option
#'   `interpElections.use_gpu` (set via [use_gpu()]).
#' @param device Character or NULL. Torch device: `"cuda"`, `"mps"`, or
#'   `"cpu"`. Only used when GPU is enabled. Default: NULL (auto-detect).
#' @param dtype Character. Torch dtype: `"float32"` or `"float64"`. Default:
#'   `"float32"`. Float32 halves memory usage with negligible precision loss.
#'
#' @return A list of class `"interpElections_optim_control"` with one element
#'   per parameter.
#'
#' @examples
#' # Default settings
#' optim_control()
#'
#' # Use GPU with more epochs
#' optim_control(use_gpu = TRUE, max_epochs = 5000)
#'
#' # Stricter convergence
#' optim_control(convergence_tol = 1e-6, patience = 100)
#'
#' @seealso [optimize_alpha()], [routing_control()]
#'
#' @export
optim_control <- function(
    max_epochs      = 2000L,
    lr_init         = 0.05,
    convergence_tol = 1e-4,
    patience        = 50L,
    barrier_mu      = 10,
    alpha_init      = 2,
    alpha_min       = NULL,
    kernel          = "power",
    use_gpu         = NULL,
    device          = NULL,
    dtype           = "float32"
) {
  # --- Validation ---
  if (!is.numeric(max_epochs) || length(max_epochs) != 1 || max_epochs < 1) {
    stop("`max_epochs` must be a positive integer", call. = FALSE)
  }
  if (!is.numeric(lr_init) || length(lr_init) != 1 || lr_init <= 0) {
    stop("`lr_init` must be a positive number", call. = FALSE)
  }
  if (!is.numeric(convergence_tol) || length(convergence_tol) != 1 ||
      convergence_tol <= 0) {
    stop("`convergence_tol` must be a positive number", call. = FALSE)
  }
  if (!is.numeric(patience) || length(patience) != 1 || patience < 1) {
    stop("`patience` must be a positive integer", call. = FALSE)
  }
  if (!is.numeric(barrier_mu) || length(barrier_mu) != 1 || barrier_mu < 0) {
    stop("`barrier_mu` must be a non-negative number", call. = FALSE)
  }
  if (!is.numeric(alpha_init)) {
    stop("`alpha_init` must be numeric", call. = FALSE)
  }
  kernel <- match.arg(kernel, c("power", "exponential"))
  # Resolve kernel-dependent default for alpha_min
  if (is.null(alpha_min)) {
    alpha_min <- switch(kernel, power = 1, exponential = 0)
  }
  if (!is.numeric(alpha_min) || length(alpha_min) != 1) {
    stop("`alpha_min` must be a single number", call. = FALSE)
  }
  if (!is.null(use_gpu) && (!is.logical(use_gpu) || length(use_gpu) != 1)) {
    stop("`use_gpu` must be TRUE, FALSE, or NULL", call. = FALSE)
  }
  if (!is.null(device) && (!is.character(device) || length(device) != 1)) {
    stop("`device` must be a single string or NULL", call. = FALSE)
  }
  if (!is.character(dtype) || length(dtype) != 1 ||
      !dtype %in% c("float32", "float64")) {
    stop('`dtype` must be "float32" or "float64"', call. = FALSE)
  }

  structure(
    list(
      max_epochs      = as.integer(max_epochs),
      lr_init         = lr_init,
      convergence_tol = convergence_tol,
      patience        = as.integer(patience),
      barrier_mu      = barrier_mu,
      alpha_init      = alpha_init,
      alpha_min       = alpha_min,
      kernel          = kernel,
      use_gpu         = use_gpu,
      device          = device,
      dtype           = dtype
    ),
    class = "interpElections_optim_control"
  )
}


#' Routing control parameters
#'
#' Creates a control object for travel-time computation in
#' [compute_travel_times()]. All parameters have sensible defaults;
#' override only what you need.
#'
#' @param mode Character. Routing mode, using r5r convention:
#'   `"WALK"` (default), `"BICYCLE"`, `"CAR"`, `"TRANSIT"`, or
#'   combinations like `c("WALK", "TRANSIT")`. Passed directly to
#'   [r5r::travel_time_matrix()].
#' @param point_method Character. Method for computing representative points
#'   of census tracts. One of `"pop_weighted"` (default, uses WorldPop
#'   raster), `"point_on_surface"`, or `"centroid"`.
#'   See [compute_representative_points()].
#' @param min_area_for_pop_weight Numeric. Minimum tract area in km2 for
#'   applying the pop_weighted method. Smaller tracts fall back to
#'   point_on_surface. Default: 1.
#' @param max_trip_duration Integer. Maximum trip duration in minutes.
#'   Pairs beyond this threshold are not routed and receive zero weight.
#'   Default: 180 (3 hours walking).
#' @param n_threads Integer. Number of parallel threads for the r5r routing
#'   engine. Default: 4.
#' @param gtfs_zip_path Character or NULL. Path to a GTFS `.zip` file for
#'   transit routing. Copied into the network directory so r5r can
#'   auto-detect it. Default: NULL.
#' @param departure_datetime POSIXct or NULL. Departure time for
#'   transit-based routing. Required when `mode` includes transit.
#'   Default: NULL.
#' @param pop_raster A [terra::SpatRaster], file path to GeoTIFF, or NULL.
#'   Population raster for `point_method = "pop_weighted"`. If NULL,
#'   WorldPop data is auto-downloaded. Default: NULL.
#' @param osm_buffer_km Numeric. Buffer in km around the combined bounding
#'   box of tracts and points when clipping OSM data. Default: 10.
#' @param fill_missing Numeric, `NA`, or NULL. Value for unreachable OD pairs.
#'   `NA` (default when NULL) leaves unreachable pairs as `NA` in the travel
#'   time matrix, which maps to exactly zero weight in the weight matrix.
#'   These pairs are immune to alpha optimization. Set to a numeric value
#'   (e.g., `max_trip_duration`) to impute a travel time instead.
#'
#' @return A list of class `"interpElections_routing_control"` with one element
#'   per parameter.
#'
#' @examples
#' # Default settings (walking, 3h max)
#' routing_control()
#'
#' # Transit mode with GTFS
#' routing_control(
#'   mode = c("WALK", "TRANSIT"),
#'   gtfs_zip_path = "sptrans.zip",
#'   departure_datetime = as.POSIXct("2022-10-02 10:00:00")
#' )
#'
#' # Bicycle with centroid-based points
#' routing_control(mode = "BICYCLE", point_method = "centroid")
#'
#' @seealso [compute_travel_times()], [optim_control()]
#'
#' @export
routing_control <- function(
    mode                    = "WALK",
    point_method            = "pop_weighted",
    min_area_for_pop_weight = 1,
    max_trip_duration       = 180L,
    n_threads               = 4L,
    gtfs_zip_path           = NULL,
    departure_datetime      = NULL,
    pop_raster              = NULL,
    osm_buffer_km           = 10,
    fill_missing            = NULL
) {
  # --- Validation ---
  if (!is.character(mode) || length(mode) == 0) {
    stop("`mode` must be a character vector (e.g., \"WALK\", c(\"WALK\", \"TRANSIT\"))",
         call. = FALSE)
  }
  valid_modes <- c("WALK", "BICYCLE", "CAR", "TRANSIT", "BUS", "RAIL",
                    "FERRY", "CABLE_CAR", "GONDOLA", "FUNICULAR")
  bad <- setdiff(toupper(mode), valid_modes)
  if (length(bad) > 0) {
    stop("Unknown routing mode(s): ", paste(bad, collapse = ", "),
         "\nValid modes: ", paste(valid_modes, collapse = ", "),
         call. = FALSE)
  }
  mode <- toupper(mode)

  valid_methods <- c("pop_weighted", "point_on_surface", "centroid")
  point_method <- match.arg(point_method, valid_methods)

  if (!is.numeric(min_area_for_pop_weight) ||
      length(min_area_for_pop_weight) != 1 ||
      min_area_for_pop_weight < 0) {
    stop("`min_area_for_pop_weight` must be a non-negative number", call. = FALSE)
  }
  if (!is.numeric(max_trip_duration) || length(max_trip_duration) != 1 ||
      max_trip_duration <= 0) {
    stop("`max_trip_duration` must be a positive number", call. = FALSE)
  }
  if (!is.numeric(n_threads) || length(n_threads) != 1 || n_threads < 1) {
    stop("`n_threads` must be a positive integer", call. = FALSE)
  }
  if (!is.null(gtfs_zip_path) &&
      (!is.character(gtfs_zip_path) || length(gtfs_zip_path) != 1)) {
    stop("`gtfs_zip_path` must be a single file path or NULL", call. = FALSE)
  }
  if (!is.null(departure_datetime) && !inherits(departure_datetime, "POSIXct")) {
    stop("`departure_datetime` must be a POSIXct object or NULL", call. = FALSE)
  }
  if (!is.numeric(osm_buffer_km) || length(osm_buffer_km) != 1 ||
      osm_buffer_km < 0) {
    stop("`osm_buffer_km` must be a non-negative number", call. = FALSE)
  }
  if (!is.null(fill_missing)) {
    if (length(fill_missing) != 1)
      stop("`fill_missing` must be a single value or NULL", call. = FALSE)
    if (!is.na(fill_missing) && !is.numeric(fill_missing))
      stop("`fill_missing` must be a single number, NA, or NULL", call. = FALSE)
  }

  # Resolve fill_missing default
  fill_resolved <- if (is.null(fill_missing)) {
    NA_real_
  } else {
    fill_missing
  }

  structure(
    list(
      mode                    = mode,
      point_method            = point_method,
      min_area_for_pop_weight = min_area_for_pop_weight,
      max_trip_duration       = as.integer(max_trip_duration),
      n_threads               = as.integer(n_threads),
      gtfs_zip_path           = gtfs_zip_path,
      departure_datetime      = departure_datetime,
      pop_raster              = pop_raster,
      osm_buffer_km           = osm_buffer_km,
      fill_missing            = fill_resolved
    ),
    class = "interpElections_routing_control"
  )
}


#' @export
print.interpElections_optim_control <- function(x, ...) {
  cat("interpElections optimization control:\n")
  cat("  max_epochs:", x$max_epochs, "\n")
  cat("  lr_init:", x$lr_init, "\n")
  cat("  convergence_tol:", x$convergence_tol, "\n")
  cat("  patience:", x$patience, "\n")
  cat("  barrier_mu:", x$barrier_mu, "\n")
  cat("  alpha_init:", if (length(x$alpha_init) == 1) x$alpha_init
      else paste0("[", length(x$alpha_init), " values]"), "\n")
  cat("  alpha_min:", x$alpha_min, "\n")
  cat("  kernel:", x$kernel, "\n")
  gpu_str <- if (is.null(x$use_gpu)) "NULL (auto)" else x$use_gpu
  cat("  use_gpu:", gpu_str, "\n")
  cat("  device:", x$device %||% "auto", "\n")
  cat("  dtype:", x$dtype, "\n")
  invisible(x)
}


#' @export
print.interpElections_routing_control <- function(x, ...) {
  cat("interpElections routing control:\n")
  cat("  mode:", paste(x$mode, collapse = ";"), "\n")
  cat("  point_method:", x$point_method, "\n")
  cat("  min_area_for_pop_weight:", x$min_area_for_pop_weight, "\n")
  cat("  max_trip_duration:", x$max_trip_duration, "min\n")
  cat("  n_threads:", x$n_threads, "\n")
  cat("  gtfs_zip_path:", x$gtfs_zip_path %||% "NULL", "\n")
  cat("  departure_datetime:",
      if (is.null(x$departure_datetime)) "NULL"
      else format(x$departure_datetime), "\n")
  cat("  pop_raster:",
      if (is.null(x$pop_raster)) "NULL (auto-download)"
      else if (is.character(x$pop_raster)) x$pop_raster
      else "SpatRaster", "\n")
  cat("  osm_buffer_km:", x$osm_buffer_km, "\n")
  cat("  fill_missing:", x$fill_missing, "\n")
  invisible(x)
}

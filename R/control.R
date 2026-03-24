#' Optimization control parameters
#'
#' Creates a control object for the SGD optimizer in [optimize_alpha()].
#' All parameters have sensible defaults; override only what you need.
#'
#' @param max_epochs Integer. Maximum number of epochs (full passes through
#'   all tracts). The optimizer may stop earlier if convergence is detected.
#'   Default: 50000.
#' @param lr_init Numeric. Initial ADAM learning rate. In adaptive mode
#'   (`target_eff_src` set), uses constant LR throughout (the dual update
#'   changes the loss landscape every epoch, making monotone LR decay
#'   counterproductive). In non-adaptive mode, follows SGDR cosine annealing
#'   with warm restarts. Default: 0.05.
#' @param convergence_tol Numeric. Minimum relative improvement over the
#'   patience window for the optimizer to keep running. If the deviance
#'   (or total loss in non-adaptive mode) improves by less than
#'   `convergence_tol` fraction over the last `patience` epochs, the
#'   optimizer converges. Default: 1e-5.
#' @param patience Integer. Lookback window (in epochs) for the window-based
#'   convergence criterion. The optimizer compares the current deviance
#'   against the deviance `patience` epochs ago and converges when the
#'   relative improvement is less than `convergence_tol`. Works alongside a
#'   gradient-based criterion that triggers when the relative gradient norm
#'   is small for several consecutive epochs. Convergence is never declared
#'   before `max(3 * patience, 500)` epochs to allow sufficient exploration.
#'   Default: 100.
#' @param barrier_mu Numeric. Strength of the log-barrier penalty that
#'   prevents any census tract from receiving zero predicted voters.
#'   Set to 0 to disable. Default: 1.
#' @param entropy_mu Numeric. Strength of the Shannon entropy penalty that
#'   discourages diffuse weight distributions (many effective sources per
#'   tract). Higher values push the optimizer to concentrate weights on
#'   fewer nearby stations, reducing effective sources at the cost of
#'   higher Poisson deviance. The penalty uses the sum of per-tract
#'   entropies, consistent with the deviance and barrier terms.
#'   Set to 0 to disable. Default: 0.
#'   Ignored when `target_eff_src` is set (dual ascent mode).
#' @param target_eff_src Numeric or NULL. Target number of effective sources
#'   per tract. When set (not NULL), enables dual ascent: the optimizer
#'   automatically adapts `entropy_mu` during training to reach this target.
#'   Must be > 1. Mutually exclusive with manual `entropy_mu` tuning.
#'   Default: NULL (disabled).
#' @param dual_eta Numeric. Scaling factor for the per-epoch log-space dual
#'   update of `entropy_mu` (augmented Lagrangian). Each epoch:
#'   `log_mu += decay * dual_eta / T_damp * (mean_H - h_target_mean)`,
#'   then `entropy_mu = exp(log_mu)` (always positive). `T_damp = 500` is a
#'   dampening constant and `decay = 1/sqrt(1 + epoch/5000)` is a gentle
#'   schedule. The per-tract quadratic penalty `(rho/2)*sum((H_i - h_target_i)^2)`
#'   does the heavy lifting; this dual update ensures exactness. Per-tract
#'   targets are adaptively set using a feasibility floor from the travel time
#'   structure at alpha_ref=7 (power) or 50 (exponential).
#'   Default: 1.0.
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
#' @param report_every Integer or NULL. How often to print epoch progress.
#'   `NULL` (default) prints ~20 lines automatically (`max_epochs %/% 20`).
#'   Set to a smaller value (e.g., `10`) for detailed per-batch reporting with
#'   timing. With `report_every = 1` every epoch is printed.
#' @param perf_log Character or NULL. Path to a CSV file for saving per-epoch
#'   performance metrics (loss components, gradient norm, lr, alpha stats,
#'   effective sources, wall-clock time per epoch). Useful for benchmarking
#'   and studying convergence. `NULL` (default) disables the log. The file is
#'   written once after the epoch loop completes (or at convergence).
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
#' optim_control(convergence_tol = 1e-6, patience = 200)
#'
#' @seealso [optimize_alpha()], [routing_control()]
#'
#' @export
optim_control <- function(
    max_epochs      = 50000L,
    lr_init         = 0.05,
    convergence_tol = 1e-5,
    patience        = 100L,
    barrier_mu      = 1,
    entropy_mu      = 0,
    target_eff_src  = NULL,
    dual_eta        = 1.0,
    alpha_init      = 2,
    alpha_min       = NULL,
    kernel          = "power",
    use_gpu         = NULL,
    device          = NULL,
    dtype           = "float32",
    force_chunked   = NULL,  # internal: override automatic gate (TRUE/FALSE/NULL)
    chunk_size      = NULL,  # internal: override VRAM-based m_chunk (positive integer)
    report_every    = NULL,
    perf_log        = NULL
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
  if (!is.numeric(entropy_mu) || length(entropy_mu) != 1 || entropy_mu < 0) {
    stop("`entropy_mu` must be a non-negative number", call. = FALSE)
  }
  if (!is.null(target_eff_src)) {
    if (!is.numeric(target_eff_src) || length(target_eff_src) != 1 ||
        target_eff_src <= 1) {
      stop("`target_eff_src` must be a number > 1", call. = FALSE)
    }
  }
  if (!is.numeric(dual_eta) || length(dual_eta) != 1 ||
      dual_eta <= 0 || dual_eta > 5) {
    stop("`dual_eta` must be a number in (0, 5]", call. = FALSE)
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
  if (!is.null(report_every)) {
    if (!is.numeric(report_every) || length(report_every) != 1 ||
        report_every < 1 || !is.finite(report_every)) {
      stop("`report_every` must be a positive integer or NULL", call. = FALSE)
    }
  }
  if (!is.null(perf_log)) {
    if (!is.character(perf_log) || length(perf_log) != 1) {
      stop("`perf_log` must be a file path string or NULL", call. = FALSE)
    }
  }

  structure(
    list(
      max_epochs      = as.integer(max_epochs),
      lr_init         = lr_init,
      convergence_tol = convergence_tol,
      patience        = as.integer(patience),
      barrier_mu      = barrier_mu,
      entropy_mu      = entropy_mu,
      target_eff_src  = target_eff_src,
      dual_eta        = dual_eta,
      alpha_init      = alpha_init,
      alpha_min       = alpha_min,
      kernel          = kernel,
      use_gpu         = use_gpu,
      device          = device,
      dtype           = dtype,
      force_chunked   = force_chunked,
      chunk_size      = chunk_size,
      report_every    = if (!is.null(report_every)) as.integer(report_every) else NULL,
      perf_log        = perf_log
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
#'   `"AUTO"` (default), `"WALK"`, `"BICYCLE"`, `"CAR"`, `"TRANSIT"`, or
#'   combinations like `c("WALK", "TRANSIT")`. Passed directly to
#'   [r5r::travel_time_matrix()].
#'
#'   When `mode = "AUTO"`, routing first tries `"WALK"` with
#'   `max_trip_duration`. If the fraction of tracts with no reachable
#'   station exceeds `unreachable_threshold`, the **entire** matrix is
#'   re-computed with `"CAR"` and `fallback_max_trip_duration`. Travel
#'   times in the returned matrix are always from a single mode — never
#'   mixed. For municipalities with large rural areas or complex geography
#'   (rivers, forests), CAR mode may provide better coverage; AUTO handles
#'   this automatically.
#' @param point_method Character. Method for computing representative points
#'   of census tracts. One of `"pop_weighted"` (default, uses WorldPop
#'   raster), `"point_on_surface"`, or `"centroid"`.
#'   See [compute_representative_points()].
#' @param min_area_for_pop_weight Numeric. Minimum tract area in km2 for
#'   applying the pop_weighted method. Smaller tracts fall back to
#'   point_on_surface. Default: 1.
#' @param max_trip_duration Integer. Maximum trip duration in minutes.
#'   Pairs beyond this threshold are not routed and receive zero weight.
#'   Default: 120 (2 hours walking). In AUTO mode, this is used for the
#'   initial WALK attempt.
#' @param fallback_max_trip_duration Integer. Maximum trip duration in minutes
#'   for the CAR fallback when `mode = "AUTO"`. Ignored for other modes.
#'   Default: 90 (1.5 hours driving).
#' @param unreachable_threshold Numeric between 0 and 1. Fraction of tracts
#'   that must be unreachable (zero reachable stations) to trigger the
#'   WALK-to-CAR switch in AUTO mode. Ignored for other modes.
#'   Default: 0.01 (1%).
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
#' # Default settings (AUTO mode: walk first, car fallback if needed)
#' routing_control()
#'
#' # Explicit walking mode (no fallback)
#' routing_control(mode = "WALK")
#'
#' # Transit mode with GTFS
#' routing_control(
#'   mode = c("WALK", "TRANSIT"),
#'   gtfs_zip_path = "sptrans.zip",
#'   departure_datetime = as.POSIXct("2022-10-02 10:00:00")
#' )
#'
#' # AUTO mode with more lenient threshold (10% unreachable to trigger CAR)
#' routing_control(unreachable_threshold = 0.10)
#'
#' # Bicycle with centroid-based points
#' routing_control(mode = "BICYCLE", point_method = "centroid")
#'
#' @seealso [compute_travel_times()], [optim_control()]
#'
#' @export
routing_control <- function(
    mode                       = "AUTO",
    point_method               = "pop_weighted",
    min_area_for_pop_weight    = 1,
    max_trip_duration          = 120L,
    fallback_max_trip_duration = 90L,
    unreachable_threshold      = 0.01,
    n_threads                  = 4L,
    gtfs_zip_path              = NULL,
    departure_datetime         = NULL,
    pop_raster                 = NULL,
    osm_buffer_km              = 10,
    fill_missing               = NULL
) {
  # --- Validation ---
  if (!is.character(mode) || length(mode) == 0) {
    stop("`mode` must be a character vector (e.g., \"AUTO\", \"WALK\", c(\"WALK\", \"TRANSIT\"))",
         call. = FALSE)
  }
  valid_modes <- c("AUTO", "WALK", "BICYCLE", "CAR", "TRANSIT", "BUS", "RAIL",
                    "FERRY", "CABLE_CAR", "GONDOLA", "FUNICULAR")
  bad <- setdiff(toupper(mode), valid_modes)
  if (length(bad) > 0) {
    stop("Unknown routing mode(s): ", paste(bad, collapse = ", "),
         "\nValid modes: ", paste(valid_modes, collapse = ", "),
         call. = FALSE)
  }
  mode <- toupper(mode)
  if ("AUTO" %in% mode && length(mode) > 1) {
    stop("\"AUTO\" cannot be combined with other modes", call. = FALSE)
  }
  if (!is.numeric(fallback_max_trip_duration) ||
      length(fallback_max_trip_duration) != 1 ||
      fallback_max_trip_duration <= 0) {
    stop("`fallback_max_trip_duration` must be a positive number", call. = FALSE)
  }
  if (!is.numeric(unreachable_threshold) ||
      length(unreachable_threshold) != 1 ||
      unreachable_threshold < 0 || unreachable_threshold > 1) {
    stop("`unreachable_threshold` must be a number between 0 and 1", call. = FALSE)
  }

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
      mode                       = mode,
      point_method               = point_method,
      min_area_for_pop_weight    = min_area_for_pop_weight,
      max_trip_duration          = as.integer(max_trip_duration),
      fallback_max_trip_duration = as.integer(fallback_max_trip_duration),
      unreachable_threshold      = unreachable_threshold,
      n_threads                  = as.integer(n_threads),
      gtfs_zip_path              = gtfs_zip_path,
      departure_datetime         = departure_datetime,
      pop_raster                 = pop_raster,
      osm_buffer_km              = osm_buffer_km,
      fill_missing               = fill_resolved
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
  if (!is.null(x$target_eff_src)) {
    cat("  target_eff_src:", x$target_eff_src, "(dual ascent)\n")
    cat("  dual_eta:", x$dual_eta, "\n")
  } else if (x$entropy_mu > 0) {
    cat("  entropy_mu:", x$entropy_mu, "\n")
  }
  cat("  alpha_init:", if (length(x$alpha_init) == 1) x$alpha_init
      else paste0("[", length(x$alpha_init), " values]"), "\n")
  cat("  alpha_min:", x$alpha_min, "\n")
  cat("  kernel:", x$kernel, "\n")
  gpu_str <- if (is.null(x$use_gpu)) "NULL (auto)" else x$use_gpu
  cat("  use_gpu:", gpu_str, "\n")
  cat("  device:", x$device %||% "auto", "\n")
  cat("  dtype:", x$dtype, "\n")
  if (!is.null(x$report_every)) cat("  report_every:", x$report_every, "\n")
  if (!is.null(x$perf_log))    cat("  perf_log:", x$perf_log, "\n")
  invisible(x)
}


#' @export
print.interpElections_routing_control <- function(x, ...) {
  cat("interpElections routing control:\n")
  cat("  mode:", paste(x$mode, collapse = ";"), "\n")
  if (identical(x$mode, "AUTO")) {
    cat("  fallback_max_trip_duration:", x$fallback_max_trip_duration, "min\n")
    cat("  unreachable_threshold:", sprintf("%.0f%%", x$unreachable_threshold * 100), "\n")
  }
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

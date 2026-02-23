# Internal utility functions

# Null-coalescing operator backport for R < 4.4.0.
# R 4.4.0+ has base::`%||%`; we define it for R 4.1.0-4.3.x compatibility.
if (!exists("%||%", baseenv())) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

# Column-standardize a matrix so each column sums to 1
# Maps to: t(t(inv_W_alpha)/colSums(inv_W_alpha)) pattern in the original code
.col_standardize <- function(mat) {
  cs <- colSums(mat)
  cs[cs == 0] <- 1 # avoid division by zero for empty columns
  result <- t(t(mat) / cs)
  result[!is.finite(result)] <- 0
  result
}

# Apply travel-time offset to avoid 0^(-alpha) singularity
.apply_offset <- function(time_matrix, offset = 1) {
  if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset) || offset < 0) {
    stop("offset must be a single non-negative finite number", call. = FALSE)
  }
  time_matrix + offset
}

# Combine bboxes of two sf objects, buffer by km, return sf polygon
.expand_bbox <- function(sf1, sf2, buffer_km = 10) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required", call. = FALSE)
  }

  # Transform both to WGS84 for consistent bbox
  sf1_wgs <- sf::st_transform(sf1, 4326)
  sf2_wgs <- sf::st_transform(sf2, 4326)

  # Combine bounding boxes
  bb1 <- sf::st_bbox(sf1_wgs)
  bb2 <- sf::st_bbox(sf2_wgs)
  combined_bb <- c(
    xmin = min(bb1["xmin"], bb2["xmin"]),
    ymin = min(bb1["ymin"], bb2["ymin"]),
    xmax = max(bb1["xmax"], bb2["xmax"]),
    ymax = max(bb1["ymax"], bb2["ymax"])
  )

  # Convert km to approximate degrees.
  # 1 degree latitude ~ 111 km everywhere; longitude shrinks by cos(lat).
  KM_PER_DEG <- 111.0
  mid_lat <- unname((combined_bb["ymin"] + combined_bb["ymax"]) / 2)
  lat_buffer <- buffer_km / KM_PER_DEG
  lon_buffer <- buffer_km / (KM_PER_DEG * cos(mid_lat * pi / 180))

  expanded_bb <- c(
    xmin = unname(combined_bb["xmin"]) - lon_buffer,
    ymin = unname(combined_bb["ymin"]) - lat_buffer,
    xmax = unname(combined_bb["xmax"]) + lon_buffer,
    ymax = unname(combined_bb["ymax"]) + lat_buffer
  )

  # Create sf polygon from expanded bbox
  bbox_poly <- sf::st_as_sfc(sf::st_bbox(
    expanded_bb,
    crs = sf::st_crs(4326)
  ))

  sf::st_sf(geometry = bbox_poly)
}

# Compute an MD5 hash via base R (serialize to tempfile, then md5sum).
# No external dependencies required.
.digest_simple <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  con <- file(tmp, "wb")
  serialize(x, connection = con)
  close(con)
  unname(tools::md5sum(tmp))
}

# Input validation helpers

# Check a single matrix is numeric, non-empty, and contains only finite values
.check_matrix <- function(mat, name) {
  if (!is.matrix(mat) || !is.numeric(mat)) {
    stop(name, " must be a numeric matrix", call. = FALSE)
  }
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    stop(name, " must not be empty", call. = FALSE)
  }
  if (anyNA(mat) || any(!is.finite(mat))) {
    stop(name, " must not contain NA, NaN, or Inf values", call. = FALSE)
  }
}

.validate_matrices <- function(time_matrix, pop_matrix, source_matrix,
                               alpha = NULL) {
  .check_matrix(time_matrix, "time_matrix")
  .check_matrix(pop_matrix, "pop_matrix")
  .check_matrix(source_matrix, "source_matrix")

  n <- nrow(time_matrix)
  m <- ncol(time_matrix)
  k <- ncol(pop_matrix)

  if (nrow(pop_matrix) != n) {
    stop(sprintf(
      "pop_matrix has %d rows but time_matrix has %d rows (must match)",
      nrow(pop_matrix), n
    ), call. = FALSE)
  }
  if (nrow(source_matrix) != m) {
    stop(sprintf(
      "source_matrix has %d rows but time_matrix has %d columns (must match)",
      nrow(source_matrix), m
    ), call. = FALSE)
  }
  if (ncol(pop_matrix) != ncol(source_matrix)) {
    stop(sprintf(
      "pop_matrix has %d columns but source_matrix has %d columns (must match)",
      ncol(pop_matrix), ncol(source_matrix)
    ), call. = FALSE)
  }

  # time_matrix must be strictly positive (required for t^(-alpha))
  if (any(time_matrix <= 0)) {
    stop(
      "time_matrix must contain only positive values ",
      "(apply offset before validation if needed)",
      call. = FALSE
    )
  }

  if (!is.null(alpha)) {
    .validate_alpha(alpha, n, k = k)
  }

  invisible(TRUE)
}

.validate_alpha <- function(alpha, n, m = NULL, k = NULL) {
  if (is.matrix(alpha)) {
    # Per-tract-bracket matrix: shape (n_tracts, k_brackets)
    if (!is.numeric(alpha))
      stop("alpha matrix must be numeric", call. = FALSE)
    if (nrow(alpha) != n)
      stop(sprintf(
        "alpha matrix must have %d rows (one per tract), got %d",
        n, nrow(alpha)), call. = FALSE)
    if (!is.null(k) && ncol(alpha) != k)
      stop(sprintf(
        "alpha matrix must have %d columns (one per bracket), got %d",
        k, ncol(alpha)), call. = FALSE)
    if (anyNA(alpha) || any(!is.finite(alpha)))
      stop("alpha must not contain NA, NaN, or Inf values", call. = FALSE)
    if (any(alpha < 0))
      stop("alpha values must be non-negative", call. = FALSE)
  } else {
    # Per-tract vector: original behaviour
    if (!is.numeric(alpha))
      stop("alpha must be a numeric vector", call. = FALSE)
    if (length(alpha) != n)
      stop(sprintf(
        "alpha has length %d but expected length %d",
        length(alpha), n), call. = FALSE)
    if (anyNA(alpha) || any(!is.finite(alpha)))
      stop("alpha must not contain NA, NaN, or Inf values", call. = FALSE)
    if (any(alpha < 0))
      stop("alpha values must be non-negative", call. = FALSE)
  }
  invisible(TRUE)
}

# --- Population density raster utilities ---

#' @noRd
.download_worldpop_raster <- function(tracts_sf, verbose = TRUE) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "The 'terra' package is required for population-weighted points.\n",
      "Install with: install.packages('terra')",
      call. = FALSE
    )
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required", call. = FALSE)
  }

  # WorldPop Constrained 2020 for Brazil (~48 MB, 100m resolution)
  # Constrained = population allocated only to satellite-detected built-up areas
  url <- paste0(
    "https://data.worldpop.org/GIS/Population/",
    "Global_2000_2020_Constrained/2020/BSGM/BRA/",
    "bra_ppp_2020_UNadj_constrained.tif"
  )
  filename <- "worldpop_bra_2020_constrained.tif"

  if (verbose) message("  Loading WorldPop constrained raster (Brazil 2020)...")

  local_path <- .interpElections_download(
    url = url,
    filename = filename,
    subdir = .cache_subdirs()$pop_raster,
    cache = TRUE,
    force = FALSE,
    verbose = verbose
  )

  # Load raster (terra uses lazy loading, minimal memory)
  rast <- terra::rast(local_path)

  # Crop to municipality bounding box with small buffer
  tracts_wgs <- sf::st_transform(tracts_sf, 4326)
  bb <- sf::st_bbox(tracts_wgs)
  crop_ext <- terra::ext(
    bb[["xmin"]] - 0.01,
    bb[["xmax"]] + 0.01,
    bb[["ymin"]] - 0.01,
    bb[["ymax"]] + 0.01
  )

  if (verbose) message("  Cropping raster to municipality extent...")
  terra::crop(rast, crop_ext)
}

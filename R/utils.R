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

# Extract named arguments from a dots list that match a target function's formals
.extract_args <- function(dots, target_fn) {
  valid <- names(formals(target_fn))
  dots[names(dots) %in% valid]
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

# Save and load interpElections_result objects

#' Save an interpolation result to disk
#'
#' Saves an `interpElections_result` object to an RDS file, handling
#' non-serializable components (e.g., `SpatRaster` from `terra`).
#'
#' @param result An `interpElections_result` object.
#' @param path Character. File path to save to (typically `.rds`).
#' @param compress Logical or character. Compression argument passed to
#'   [saveRDS()]. Default: `TRUE`.
#'
#' @return The file path (invisibly).
#'
#' @seealso [load_interpolation()]
#'
#' @export
save_interpolation <- function(result, path, compress = TRUE) {
  if (!inherits(result, "interpElections_result")) {
    stop("`result` must be an interpElections_result object.", call. = FALSE)
  }

  # Wrap SpatRaster objects (they use external pointers, not serializable)
  if (!is.null(result$pop_raster) &&
      inherits(result$pop_raster, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(
        "The 'terra' package is required to save results with pop_raster.\n",
        "Install with: install.packages(\"terra\")",
        call. = FALSE
      )
    }
    result$pop_raster <- terra::wrap(result$pop_raster)
  }

  saveRDS(result, file = path, compress = compress)
  invisible(path)
}


#' Load an interpolation result from disk
#'
#' Reads an `interpElections_result` object from an RDS file, restoring
#' any wrapped non-serializable components (e.g., `SpatRaster`).
#'
#' @param path Character. File path to an `.rds` file created by
#'   [save_interpolation()] or `saveRDS()`.
#'
#' @return An `interpElections_result` object.
#'
#' @seealso [save_interpolation()]
#'
#' @export
load_interpolation <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }

  result <- readRDS(path)

  if (!inherits(result, "interpElections_result")) {
    stop(
      "The loaded object is not an interpElections_result.\n",
      "Expected class 'interpElections_result'.",
      call. = FALSE
    )
  }

  # Unwrap PackedSpatRaster objects
  if (!is.null(result$pop_raster) &&
      inherits(result$pop_raster, "PackedSpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      warning(
        "The 'terra' package is needed to unwrap pop_raster.\n",
        "Install with: install.packages(\"terra\")\n",
        "pop_raster will remain in packed form.",
        call. = FALSE
      )
    } else {
      result$pop_raster <- terra::unwrap(result$pop_raster)
    }
  }

  result
}

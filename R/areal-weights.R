#' Compute area-weighted intersection matrix between two polygon layers
#'
#' Builds a weight matrix that maps values from source polygons (e.g., census
#' tracts) to target polygons (e.g., custom analysis zones) based on the
#' fraction of area shared between them.
#'
#' @param target_sf An `sf` object with polygon geometries. The target zones
#'   to aggregate into.
#' @param source_sf An `sf` object with polygon geometries. The source zones
#'   (e.g., census tracts with interpolated data).
#' @param target_id Character. Name of the ID column in `target_sf`.
#' @param source_id Character. Name of the ID column in `source_sf`.
#'
#' @return A numeric matrix \[n_target x n_source\]. Each column is
#'   standardized so that the weights from all target zones sum to 1 (or 0
#'   if the source zone has no overlap). Row names = target IDs, column
#'   names = source IDs.
#'
#' @details
#' Requires the `sf` package. Geometries are made valid with
#' [sf::st_make_valid()] before computing intersections.
#'
#' @family spatial
#'
#' @seealso [areal_interpolate()] to apply the weights.
#'
#' @export
areal_weights <- function(target_sf, source_sf,
                          target_id = "id", source_id = "id") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required for areal_weights()", call. = FALSE)
  }

  # Validate inputs
  if (!inherits(target_sf, "sf")) {
    stop("'target_sf' must be an sf object", call. = FALSE)
  }
  if (!inherits(source_sf, "sf")) {
    stop("'source_sf' must be an sf object", call. = FALSE)
  }
  if (!target_id %in% names(target_sf)) {
    stop(sprintf("target_id column '%s' not found in target_sf",
                 target_id), call. = FALSE)
  }
  if (!source_id %in% names(source_sf)) {
    stop(sprintf("source_id column '%s' not found in source_sf",
                 source_id), call. = FALSE)
  }

  # Compute source areas
  source_sf$`.area` <- sf::st_area(source_sf)

  # Compute intersections
  target_valid <- sf::st_make_valid(target_sf[, target_id])
  source_valid <- sf::st_make_valid(source_sf[, c(source_id, ".area")])

  intersections <- sf::st_intersection(target_valid, source_valid)
  intersections$`.mini_area` <- sf::st_area(intersections)
  intersections$`.frac_area` <- as.numeric(
    intersections$`.mini_area` / intersections$`.area`
  )

  # Drop geometry for pivoting
  int_df <- sf::st_drop_geometry(intersections)
  int_df <- int_df[, c(target_id, source_id, ".frac_area")]

  # Get all IDs
  target_ids <- as.character(target_sf[[target_id]])
  source_ids <- as.character(source_sf[[source_id]])
  int_df[[target_id]] <- as.character(int_df[[target_id]])
  int_df[[source_id]] <- as.character(int_df[[source_id]])

  # Build matrix using vectorized match() lookup instead of row-by-row loop
  mat <- matrix(0, nrow = length(target_ids), ncol = length(source_ids),
                dimnames = list(target_ids, source_ids))
  row_idx <- match(int_df[[target_id]], target_ids)
  col_idx <- match(int_df[[source_id]], source_ids)
  valid <- !is.na(row_idx) & !is.na(col_idx)
  if (any(valid)) {
    # Vectorized aggregation: convert to linear indices and use tapply
    ri <- row_idx[valid]
    ci <- col_idx[valid]
    vals <- int_df$.frac_area[valid]
    lin_idx <- (ci - 1L) * nrow(mat) + ri
    agg <- tapply(vals, lin_idx, sum)
    mat[as.integer(names(agg))] <- as.numeric(agg)
  }

  .col_standardize(mat)
}


#' Aggregate data from source zones to target polygons using areal weights
#'
#' Applies a precomputed areal weight matrix (from [areal_weights()]) to
#' transfer data from source zones to target polygons.
#'
#' @param data Numeric matrix or data.frame \[n_source x p\]. Data from
#'   source zones. Rows must correspond to the columns of `weights`.
#' @param weights Numeric matrix \[n_target x n_source\]. Output of
#'   [areal_weights()].
#'
#' @return Numeric matrix \[n_target x p\]. Aggregated values in target
#'   polygons.
#'
#' @examples
#' W <- matrix(c(0.7, 0.3, 0.2, 0.8), nrow = 2)  # 2 targets x 2 sources
#' src_data <- matrix(c(100, 200), nrow = 2)        # 2 sources x 1 variable
#' areal_interpolate(src_data, W)
#'
#' @family spatial
#'
#' @export
areal_interpolate <- function(data, weights) {
  if (is.data.frame(data)) {
    col_names <- names(data)
    data <- as.matrix(data)
  } else {
    col_names <- colnames(data)
  }

  if (!is.numeric(data)) {
    stop("data must be numeric", call. = FALSE)
  }
  if (!is.matrix(weights) || !is.numeric(weights)) {
    stop("weights must be a numeric matrix", call. = FALSE)
  }
  if (ncol(weights) != nrow(data)) {
    stop(sprintf(
      "weights has %d columns but data has %d rows (must match)",
      ncol(weights), nrow(data)
    ), call. = FALSE)
  }

  result <- weights %*% data
  if (!is.null(col_names)) colnames(result) <- col_names
  result
}

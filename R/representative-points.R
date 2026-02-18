# --- Census tract representative points ---

#' Compute representative points for census tracts
#'
#' Computes a single representative point for each census tract polygon.
#' Three methods are available: geometric point-on-surface (default),
#' centroid, or population-density-weighted using WorldPop raster data.
#'
#' @param tracts_sf An `sf` polygon object containing census tract geometries.
#' @param method Character. Method for computing representative points:
#'   \describe{
#'     \item{`"point_on_surface"`}{(Default) Uses [sf::st_point_on_surface()].
#'       Guarantees the point falls inside the polygon, unlike centroids
#'       which can fall outside concave shapes.}
#'     \item{`"centroid"`}{Uses [sf::st_centroid()]. Classic geometric centroid.
#'       May fall outside concave polygons.}
#'     \item{`"pop_weighted"`}{Uses a population density raster (WorldPop
#'       Constrained 2020 by default) to find the most populated cell within
#'       each tract. Only applied to tracts with area >= `pop_min_area`;
#'       smaller tracts use `point_on_surface`. Requires the `terra` package.}
#'   }
#' @param pop_raster A [terra::SpatRaster] object, a file path to a GeoTIFF,
#'   or `NULL`. Population density raster for `method = "pop_weighted"`.
#'   If `NULL` (default), the WorldPop Brazil Constrained 2020 raster (~48 MB)
#'   is downloaded automatically and cached. Ignored for other methods.
#' @param pop_min_area Numeric. Minimum tract area in km² for applying the
#'   population-weighted method. Tracts smaller than this threshold use
#'   `point_on_surface` instead. Default: 1 (km²). Only used when
#'   `method = "pop_weighted"`.
#' @param tract_id Character. Name of the ID column in `tracts_sf`.
#'   Default: `"id"`.
#' @param verbose Logical. Print progress messages? Default: `TRUE`.
#'
#' @return An `sf` POINT object in WGS84 (EPSG:4326) with one row per tract,
#'   preserving the `tract_id` column. Carries an attribute `"point_method"`
#'   recording which method was used.
#'
#' @examples
#' \dontrun{
#' tracts <- br_prepare_tracts(code_muni = 3170701)
#' tracts$id <- tracts$code_tract
#'
#' # Default: point on surface
#' pts <- compute_representative_points(tracts)
#'
#' # Population-weighted for large tracts
#' pts_pop <- compute_representative_points(tracts, method = "pop_weighted")
#' }
#'
#' @export
compute_representative_points <- function(
    tracts_sf,
    method = c("point_on_surface", "centroid", "pop_weighted"),
    pop_raster = NULL,
    pop_min_area = 1,
    tract_id = "id",
    verbose = TRUE
) {
  method <- match.arg(method)

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required", call. = FALSE)
  }
  if (!tract_id %in% names(tracts_sf)) {
    stop(sprintf("tract_id column '%s' not found in tracts_sf", tract_id),
         call. = FALSE)
  }

  # Project to equal-area CRS for geometric operations
  tracts_proj <- sf::st_transform(tracts_sf, "EPSG:5880")

  if (method == "centroid") {
    if (verbose) message("  Computing census tract centroids...")
    pts_proj <- suppressWarnings(sf::st_centroid(tracts_proj))

  } else if (method == "point_on_surface") {
    if (verbose) message("  Computing point-on-surface for census tracts...")
    pts_proj <- suppressWarnings(sf::st_point_on_surface(tracts_proj))

  } else if (method == "pop_weighted") {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(
        "The 'terra' package is required for pop_weighted method.\n",
        "Install with: install.packages('terra')",
        call. = FALSE
      )
    }

    # Load or download raster
    if (is.null(pop_raster)) {
      pop_raster <- .download_worldpop_raster(
        tracts_sf = tracts_sf,
        verbose = verbose
      )
    } else if (is.character(pop_raster)) {
      pop_raster <- terra::rast(pop_raster)
    }

    if (verbose) message("  Computing population-weighted representative points...")
    pts_proj <- .pop_weighted_points(
      tracts_proj = tracts_proj,
      pop_raster = pop_raster,
      pop_min_area = pop_min_area,
      tract_id = tract_id,
      verbose = verbose
    )
  }

  # Transform to WGS84 and keep only ID + geometry
  pts_wgs <- sf::st_transform(pts_proj, 4326)
  result <- pts_wgs[, tract_id]
  attr(result, "point_method") <- method
  if (method == "pop_weighted" && inherits(pop_raster, "SpatRaster")) {
    attr(result, "pop_raster") <- pop_raster
  }
  # Propagate no_pop_tracts diagnostic attribute
  no_pop <- attr(pts_proj, "no_pop_tracts")
  if (!is.null(no_pop)) {
    attr(result, "no_pop_tracts") <- no_pop
  }
  result
}


#' @noRd
.pop_weighted_points <- function(
    tracts_proj,
    pop_raster,
    pop_min_area = 1,
    tract_id = "id",
    verbose = TRUE
) {
  # Compute tract areas in km²
  areas_m2 <- as.numeric(sf::st_area(tracts_proj))
  areas_km2 <- areas_m2 / 1e6

  large_idx <- which(areas_km2 >= pop_min_area)
  small_idx <- which(areas_km2 < pop_min_area)

  if (verbose) {
    message(sprintf(
      "    %d tracts >= %.1f km\u00b2 (pop-weighted), %d tracts < %.1f km\u00b2 (point-on-surface)",
      length(large_idx), pop_min_area, length(small_idx), pop_min_area
    ))
  }

  # Initialize all points with point_on_surface
  all_pts <- suppressWarnings(sf::st_point_on_surface(tracts_proj))

  # If no large tracts, we're done

  if (length(large_idx) == 0) {
    return(all_pts)
  }

  # Process large tracts with raster extraction
  large_tracts <- tracts_proj[large_idx, ]

  # Transform large tracts to raster CRS for extraction
  raster_crs <- terra::crs(pop_raster)
  large_raster_crs <- sf::st_transform(large_tracts, raster_crs)

  # Extract cell values with coordinates
  vect_large <- terra::vect(large_raster_crs)
  extracted <- terra::extract(pop_raster, vect_large, xy = TRUE, ID = TRUE)

  # Get the value column name (first non-ID, non-coordinate column)
  val_col <- setdiff(names(extracted), c("ID", "x", "y"))[1]

  # For each large tract, find the cell with maximum population
  n_fallback <- 0L
  fallback_ids <- character(0)
  for (i in seq_along(large_idx)) {
    rows_i <- extracted[extracted$ID == i, , drop = FALSE]
    vals <- rows_i[[val_col]]
    valid <- !is.na(vals) & is.finite(vals) & vals > 0

    if (any(valid)) {
      best <- which.max(vals[valid])
      best_row <- rows_i[valid, ][best, ]
      # Create point in raster CRS, transform to EPSG:5880
      pt_rcrs <- sf::st_sfc(
        sf::st_point(c(best_row$x, best_row$y)),
        crs = raster_crs
      )
      pt_proj <- sf::st_transform(pt_rcrs, sf::st_crs(tracts_proj))
      sf::st_geometry(all_pts)[large_idx[i]] <- sf::st_geometry(pt_proj)[[1]]
    } else {
      # Fallback: keep point_on_surface (already set)
      n_fallback <- n_fallback + 1L
      fallback_ids <- c(fallback_ids,
                        as.character(tracts_proj[[tract_id]][large_idx[i]]))
    }
  }

  if (n_fallback > 0) {
    warning(
      sprintf(
        "%d tract(s) >= %.1f km\u00b2 have no population in the WorldPop raster ",
        n_fallback, pop_min_area
      ),
      "(all cells are NA or zero). Using point-on-surface fallback.\n",
      "Tract IDs: ", paste(fallback_ids, collapse = ", "), "\n",
      "These tracts may be unpopulated areas (parks, industrial zones, water bodies) ",
      "or may fall outside the raster coverage.",
      call. = FALSE
    )
    attr(all_pts, "no_pop_tracts") <- fallback_ids
  }

  all_pts
}

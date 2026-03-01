# --- Census tract representative points ---

#' Compute representative points for census tracts
#'
#' Computes a single representative point for each census tract polygon.
#' Three methods are available: geometric point-on-surface (default),
#' centroid, or population-density-weighted using WorldPop raster data.
#'
#' The `pop_weighted` method uses cluster-based selection: it identifies
#' connected clusters of populated cells via [terra::patches()] and selects
#' the largest cluster by total population. If OSM road data is available
#' (via `osm_roads`), it applies **tiered road proximity refinement**:
#' cells within 200m of well-connected road types (primary, secondary,
#' tertiary, residential) are preferred over cells near isolated rural
#' roads (tracks, paths). This reduces the chance of placing the
#' representative point near a disconnected road fragment that r5r
#' cannot route from.
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
#'       Constrained 2020 by default) to find the best representative point
#'       within each tract via cluster analysis and optional road proximity.
#'       Only applied to tracts with area >= `min_area_for_pop_weight`;
#'       smaller tracts use `point_on_surface`. Requires the `terra` package.}
#'   }
#' @param pop_raster A [terra::SpatRaster] object, a file path to a GeoTIFF,
#'   or `NULL`. Population density raster for `method = "pop_weighted"`.
#'   If `NULL` (default), the WorldPop Brazil Constrained 2020 raster (~48 MB)
#'   is downloaded automatically and cached. Ignored for other methods.
#' @param min_area_for_pop_weight Numeric. Minimum tract area in km² for applying the
#'   population-weighted method. Tracts smaller than this threshold use
#'   `point_on_surface` instead. Default: 1 (km²). Only used when
#'   `method = "pop_weighted"`.
#' @param tract_id Character. Name of the ID column in `tracts_sf`.
#'   Default: `"id"`.
#' @param osm_roads An `sf` object with OSM road geometries, or `NULL`.
#'   When provided and `method = "pop_weighted"`, cells within 200m of
#'   roads are preferred, using a tiered hierarchy: Tier 1 (primary,
#'   secondary, tertiary, residential) > Tier 2 (unclassified, service) >
#'   Tier 3 (track, path, footway). The algorithm tries the highest tier
#'   first and falls back only if no cells are within 200m of that tier's
#'   roads. Typically read from the clipped PBF file by
#'   [compute_travel_times()].
#' @param verbose Logical. Print progress messages? Default: `TRUE`.
#'
#' @return An `sf` POINT object in WGS84 (EPSG:4326) with one row per tract,
#'   preserving the `tract_id` column. Carries attributes:
#'   \describe{
#'     \item{`"point_method"`}{Character. Which method was used.}
#'     \item{`"pop_raster"`}{The SpatRaster object (if `pop_weighted`).}
#'     \item{`"no_pop_tracts"`}{Character vector of tract IDs that fell back
#'       to `point_on_surface` due to zero population.}
#'     \item{`"pop_weighted_diagnostics"`}{Named list of per-tract diagnostic
#'       records (cluster counts, patch populations, selected point, road
#'       proximity info, road tier used). See [plot_representative_points()].}
#'   }
#'
#' @seealso [plot_representative_points()] to visualize the selection process.
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
#' @keywords internal
compute_representative_points <- function(
    tracts_sf,
    method = c("point_on_surface", "centroid", "pop_weighted"),
    pop_raster = NULL,
    min_area_for_pop_weight = 1,
    tract_id = "id",
    osm_roads = NULL,
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
      min_area_for_pop_weight = min_area_for_pop_weight,
      tract_id = tract_id,
      osm_roads = osm_roads,
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
  # Propagate diagnostic attributes
  no_pop <- attr(pts_proj, "no_pop_tracts")
  if (!is.null(no_pop)) {
    attr(result, "no_pop_tracts") <- no_pop
  }
  pop_diag <- attr(pts_proj, "pop_weighted_diagnostics")
  if (!is.null(pop_diag)) {
    attr(result, "pop_weighted_diagnostics") <- pop_diag
  }
  result
}


#' Classify OSM highway types into connectivity tiers
#'
#' Tier 1: main roads always connected to municipal network.
#' Tier 2: usually connected but sometimes isolated.
#' Tier 3: often disconnected (tracks, paths, footways).
#' @noRd
.road_tier <- function(highway) {
  tier1 <- c("primary", "primary_link", "secondary", "secondary_link",
             "tertiary", "tertiary_link", "residential", "living_street")
  tier2 <- c("unclassified", "service")
  ifelse(highway %in% tier1, 1L,
         ifelse(highway %in% tier2, 2L, 3L))
}


#' @noRd
.pop_weighted_points <- function(
    tracts_proj,
    pop_raster,
    min_area_for_pop_weight = 1,
    tract_id = "id",
    osm_roads = NULL,
    verbose = TRUE
) {
  # Compute tract areas in km²
  areas_m2 <- as.numeric(sf::st_area(tracts_proj))
  areas_km2 <- areas_m2 / 1e6

  large_idx <- which(areas_km2 >= min_area_for_pop_weight)
  small_idx <- which(areas_km2 < min_area_for_pop_weight)

  if (verbose) {
    message(sprintf(
      "    %d tracts >= %.1f km\u00b2 (pop-weighted), %d tracts < %.1f km\u00b2 (point-on-surface)",
      length(large_idx), min_area_for_pop_weight, length(small_idx), min_area_for_pop_weight
    ))
  }

  # Initialize all points with point_on_surface
  all_pts <- suppressWarnings(sf::st_point_on_surface(tracts_proj))

  # If no large tracts, we're done
  if (length(large_idx) == 0) {
    return(all_pts)
  }

  # Process large tracts with cluster-based raster selection
  large_tracts <- tracts_proj[large_idx, ]

  # Transform large tracts to raster CRS for extraction
  raster_crs <- terra::crs(pop_raster)
  large_raster_crs <- sf::st_transform(large_tracts, raster_crs)

  # Pre-compute road intersections per tract (one spatial join)
  roads_per_tract <- NULL
  if (!is.null(osm_roads) && nrow(osm_roads) > 0) {
    large_wgs <- sf::st_transform(large_tracts, 4326)
    roads_per_tract <- tryCatch(
      suppressWarnings(sf::st_intersects(large_wgs, osm_roads)),
      error = function(e) NULL
    )
  }

  n_fallback <- 0L
  fallback_ids <- character(0)
  diag_list <- vector("list", length(large_idx))

  for (i in seq_along(large_idx)) {
    tract_id_val <- as.character(tracts_proj[[tract_id]][large_idx[i]])
    tract_vect <- terra::vect(large_raster_crs[i, ])

    # Step 1: Crop + mask raster to tract polygon
    tract_rast <- tryCatch(
      terra::mask(terra::crop(pop_raster, tract_vect), tract_vect),
      error = function(e) NULL
    )

    if (is.null(tract_rast)) {
      n_fallback <- n_fallback + 1L
      fallback_ids <- c(fallback_ids, tract_id_val)
      next
    }

    # Step 2: Check for populated cells
    vals <- terra::values(tract_rast)[, 1]
    valid_mask <- !is.na(vals) & is.finite(vals) & vals > 0

    if (!any(valid_mask)) {
      n_fallback <- n_fallback + 1L
      fallback_ids <- c(fallback_ids, tract_id_val)
      next
    }

    # Step 3: Find connected clusters of populated cells
    binary <- tract_rast > 0
    binary[is.na(tract_rast)] <- NA
    patches_rast <- terra::patches(binary, directions = 8, zeroAsNA = TRUE)

    # Step 4: Total population per patch
    patch_pop <- terra::zonal(tract_rast, patches_rast, fun = "sum",
                               na.rm = TRUE)
    val_col <- setdiff(names(patch_pop), "patches")[1]

    # Step 5: Select patch with highest total population
    best_idx <- which.max(patch_pop[[val_col]])
    best_patch_id <- patch_pop$patches[best_idx]

    # Step 6: Within the best patch, find the best cell
    best_patch_mask <- patches_rast == best_patch_id
    best_rast <- terra::mask(tract_rast, best_patch_mask,
                              maskvalues = c(0, NA))

    # Step 7: Tiered road proximity refinement (if roads available)
    # Prefer cells near well-connected road types (primary, secondary,
    # tertiary, residential) over isolated rural roads (track, path).
    near_road <- FALSE
    has_roads <- FALSE
    road_tier_used <- NA_integer_
    tract_road_idx <- if (!is.null(roads_per_tract)) roads_per_tract[[i]] else integer(0)

    if (length(tract_road_idx) > 0) {
      has_roads <- TRUE
      best_cells <- terra::as.data.frame(best_rast, xy = TRUE, na.rm = TRUE)
      if (nrow(best_cells) > 1) {
        cell_pts <- sf::st_as_sf(best_cells, coords = c("x", "y"),
                                  crs = sf::st_crs(raster_crs))
        cell_pts_wgs <- sf::st_transform(cell_pts, 4326)
        tract_roads <- osm_roads[tract_road_idx, ]

        # Classify roads by connectivity tier
        if ("highway" %in% names(tract_roads)) {
          tract_roads$road_tier <- .road_tier(tract_roads$highway)
        } else {
          tract_roads$road_tier <- 2L
        }

        # Try tiers cumulatively: tier 1, then 1+2, then 1+2+3
        for (tier in 1:3) {
          tier_roads <- tract_roads[tract_roads$road_tier <= tier, ]
          if (nrow(tier_roads) == 0) next

          road_buf <- tryCatch(
            suppressWarnings(sf::st_buffer(tier_roads, dist = 200)),
            error = function(e) NULL
          )
          if (is.null(road_buf)) next

          road_union <- suppressWarnings(sf::st_union(road_buf))
          inside <- suppressWarnings(
            sf::st_intersects(cell_pts_wgs, road_union, sparse = FALSE)[, 1]
          )
          if (any(inside)) {
            near_road <- TRUE
            road_tier_used <- tier
            near_cells <- best_cells[inside, , drop = FALSE]
            pop_col <- setdiff(names(near_cells), c("x", "y"))[1]
            best_near <- which.max(near_cells[[pop_col]])
            xy <- c(near_cells$x[best_near], near_cells$y[best_near])
            best_pop <- near_cells[[pop_col]][best_near]
            break
          }
        }
      }
    }

    # If no road refinement was applied, use where.max on the cluster
    if (!near_road) {
      wm <- terra::where.max(best_rast)
      xy <- terra::xyFromCell(best_rast, wm[1, "cell"])
      xy <- c(xy[1, 1], xy[1, 2])
      best_pop <- wm[1, "value"]
    }

    # Create point in raster CRS, transform to projected CRS
    pt_rcrs <- sf::st_sfc(sf::st_point(xy), crs = raster_crs)
    pt_proj <- sf::st_transform(pt_rcrs, sf::st_crs(tracts_proj))
    sf::st_geometry(all_pts)[large_idx[i]] <- sf::st_geometry(pt_proj)[[1]]

    # Collect lightweight diagnostics
    diag_list[[i]] <- list(
      tract_id       = tract_id_val,
      n_patches      = nrow(patch_pop),
      patch_pop      = patch_pop,
      selected_patch = best_patch_id,
      selected_xy    = c(x = unname(xy[1]), y = unname(xy[2])),
      selected_pop   = best_pop,
      has_roads      = has_roads,
      near_road      = near_road,
      road_tier      = road_tier_used
    )
  }

  # Attach diagnostics
  diag_list <- diag_list[!vapply(diag_list, is.null, logical(1))]
  if (length(diag_list) > 0) {
    names(diag_list) <- vapply(diag_list, `[[`, character(1), "tract_id")
    attr(all_pts, "pop_weighted_diagnostics") <- diag_list
  }

  if (n_fallback > 0) {
    warning(
      sprintf(
        "%d tract(s) >= %.1f km\u00b2 have no population in the WorldPop raster ",
        n_fallback, min_area_for_pop_weight
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


#' Read OSM road geometries from a PBF file
#'
#' Best-effort reader: returns NULL if the PBF cannot be read
#' (e.g., GDAL OSM driver not available).
#'
#' @param network_path Directory containing the clipped `.pbf` file.
#' @param verbose Logical.
#' @return An `sf` LINESTRING object with road features, or `NULL`.
#' @noRd
.read_osm_roads <- function(network_path, verbose = TRUE) {
  pbf_files <- list.files(network_path, pattern = "\\.pbf$",
                           full.names = TRUE)
  if (length(pbf_files) == 0) return(NULL)
  pbf_path <- pbf_files[1]

  roads <- tryCatch({
    suppressWarnings(sf::st_read(pbf_path, layer = "lines", quiet = TRUE))
  }, error = function(e) {
    if (verbose) {
      message("  Note: could not read roads from PBF (",
              conditionMessage(e), ")")
    }
    NULL
  })
  if (is.null(roads) || nrow(roads) == 0) return(NULL)

  # Keep only actual roads (highway features)
  if ("highway" %in% names(roads)) {
    roads <- roads[!is.na(roads$highway), ]
  }
  if (nrow(roads) == 0) return(NULL)

  if (verbose) {
    message(sprintf("  Read %d road segments from PBF", nrow(roads)))
  }
  roads
}


#' Plot representative point diagnostics
#'
#' Visualize the population-weighted representative point selection for
#' one or more census tracts. Shows the population raster, identified
#' clusters, OSM roads, and the selected departure point.
#'
#' @param result An `interpElections_result` object, or an `sf` POINT
#'   object returned by [compute_representative_points()].
#' @param tracts_sf Census tract polygons. Extracted from `result` if
#'   available, otherwise required.
#' @param pop_raster A [terra::SpatRaster] or `NULL`. Extracted from
#'   `result` if available. Required for single-tract detail view.
#' @param tract Character vector. Tract IDs to plot in detail. If `NULL`
#'   (default), shows a summary map of all tracts.
#' @param osm_roads An `sf` object with road geometries, or `NULL`.
#'   Extracted from `result` if available.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly).
#'
#' @examples
#' \dontrun{
#' result <- interpolate_election_br(3170701, 2022,
#'              keep = c("pop_raster", "osm_roads"))
#'
#' # Summary: all tracts with cluster counts
#' plot_representative_points(result)
#'
#' # Detail: single tract showing raster, clusters, roads, point
#' plot_representative_points(result, tract = "317070105000138")
#' }
#'
#' @export
plot_representative_points <- function(result, tracts_sf = NULL,
                                        pop_raster = NULL,
                                        tract = NULL,
                                        osm_roads = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for plot_representative_points()",
         call. = FALSE)
  }

  # Extract components from result object
  if (inherits(result, "interpElections_result")) {
    rep_pts <- result$rep_points
    tracts_sf <- tracts_sf %||% result$tracts_sf
    pop_raster <- pop_raster %||% result$pop_raster
    osm_roads <- osm_roads %||% result$osm_roads
  } else if (inherits(result, "sf")) {
    rep_pts <- result
  } else {
    stop("'result' must be an interpElections_result or sf object",
         call. = FALSE)
  }

  diag <- attr(rep_pts, "pop_weighted_diagnostics")
  if (is.null(diag) || length(diag) == 0) {
    message("No cluster diagnostics available. Was pop_weighted method used?")
    return(invisible(NULL))
  }

  if (is.null(tract)) {
    return(.plot_rep_points_summary(rep_pts, tracts_sf, diag, osm_roads))
  }

  .plot_rep_points_detail(rep_pts, tracts_sf, pop_raster, diag,
                           as.character(tract), osm_roads)
}


#' @noRd
.plot_rep_points_summary <- function(rep_pts, tracts_sf, diag, osm_roads) {
  if (is.null(tracts_sf)) {
    stop("tracts_sf required for summary view", call. = FALSE)
  }

  # Build data frame of diagnostics
  tract_ids <- vapply(diag, `[[`, character(1), "tract_id")
  n_patches <- vapply(diag, `[[`, integer(1), "n_patches")
  has_roads <- vapply(diag, `[[`, logical(1), "has_roads")
  near_road <- vapply(diag, `[[`, logical(1), "near_road")
  road_tiers <- vapply(diag, function(d) {
    rt <- d$road_tier
    if (is.null(rt) || length(rt) == 0) NA_integer_ else rt
  }, integer(1))

  tract_id_col <- names(rep_pts)[1]
  tracts_sf$n_patches <- NA_integer_
  match_idx <- match(tract_ids, as.character(tracts_sf[[tract_id_col]]))
  tracts_sf$n_patches[match_idx[!is.na(match_idx)]] <-
    n_patches[!is.na(match_idx)]

  # Road tier summary for subtitle
  tier_counts <- table(factor(road_tiers[!is.na(road_tiers)], levels = 1:3))

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = tracts_sf,
                      ggplot2::aes(fill = .data$n_patches),
                      color = "grey50", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(option = "D", na.value = "grey90",
                                   name = "Clusters") +
    ggplot2::geom_sf(data = rep_pts, color = "red", size = 1.2,
                      shape = 16) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Representative points (pop_weighted)",
                  subtitle = sprintf(
                    "%d tracts, %d near road (tier1: %d, tier2: %d, tier3: %d)",
                    length(diag), sum(near_road),
                    tier_counts[1], tier_counts[2], tier_counts[3]))

  if (!is.null(osm_roads)) {
    p <- p + ggplot2::geom_sf(data = osm_roads, color = "steelblue",
                               linewidth = 0.3, alpha = 0.4)
  }

  print(p)
  invisible(p)
}


#' @noRd
.plot_rep_points_detail <- function(rep_pts, tracts_sf, pop_raster,
                                     diag, tract_id, osm_roads) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The 'terra' package is required for detail view", call. = FALSE)
  }
  if (is.null(pop_raster)) {
    stop("pop_raster required for detail view. ",
         "Use keep = \"pop_raster\" when interpolating.", call. = FALSE)
  }
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("The 'patchwork' package is required for detail view", call. = FALSE)
  }

  tract_id_col <- names(rep_pts)[1]

  plots <- lapply(tract_id, function(tid) {
    if (!tid %in% names(diag)) {
      message("Tract '", tid, "' not found in diagnostics, skipping")
      return(NULL)
    }

    d <- diag[[tid]]

    # Get tract polygon
    tract_match <- which(as.character(tracts_sf[[tract_id_col]]) == tid)
    if (length(tract_match) == 0) {
      message("Tract '", tid, "' not found in tracts_sf, skipping")
      return(NULL)
    }
    tract_poly <- sf::st_transform(tracts_sf[tract_match, ], 4326)

    # Crop + mask raster to tract
    tract_vect <- terra::vect(tract_poly)
    tract_rast <- terra::mask(terra::crop(pop_raster, tract_vect), tract_vect)

    # Recompute patches for visualization
    binary <- tract_rast > 0
    binary[is.na(tract_rast)] <- NA
    patches_rast <- terra::patches(binary, directions = 8, zeroAsNA = TRUE)

    # Convert rasters to data frames for ggplot
    pop_df <- terra::as.data.frame(tract_rast, xy = TRUE, na.rm = TRUE)
    names(pop_df)[3] <- "pop"
    patch_df <- terra::as.data.frame(patches_rast, xy = TRUE, na.rm = TRUE)
    names(patch_df)[3] <- "patch"

    # Merge
    rast_df <- merge(pop_df, patch_df, by = c("x", "y"))
    rast_df$selected <- rast_df$patch == d$selected_patch
    rast_df$patch <- factor(rast_df$patch)

    # Selected point
    pt_df <- data.frame(x = d$selected_xy[["x"]], y = d$selected_xy[["y"]])

    # Left panel: population raster + patches + selected point
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = rast_df,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                        fill = .data$pop,
                                        alpha = .data$selected)) +
      ggplot2::scale_fill_viridis_c(option = "C", name = "Population") +
      ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3),
                                   guide = "none") +
      ggplot2::geom_sf(data = tract_poly, fill = NA, color = "black",
                        linewidth = 0.5) +
      ggplot2::geom_point(data = pt_df,
                           ggplot2::aes(x = .data$x, y = .data$y),
                           color = "red", size = 3, shape = 4, stroke = 1.5) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("Tract", tid),
                    subtitle = sprintf(
                      "%d cluster(s), selected #%d (pop=%.0f)%s",
                      d$n_patches, d$selected_patch, d$selected_pop,
                      if (d$near_road) {
                        rt <- d$road_tier
                        if (!is.null(rt) && !is.na(rt)) {
                          sprintf(" [road tier %d]", rt)
                        } else {
                          " [near road]"
                        }
                      } else ""))

    # Add roads if available
    if (!is.null(osm_roads)) {
      bb <- sf::st_bbox(tract_poly)
      tract_roads <- tryCatch(
        suppressWarnings(sf::st_crop(osm_roads, bb)),
        error = function(e) NULL
      )
      if (!is.null(tract_roads) && nrow(tract_roads) > 0) {
        p1 <- p1 + ggplot2::geom_sf(data = tract_roads,
                                      color = "steelblue",
                                      linewidth = 0.6, alpha = 0.7)
      }
    }

    # Right panel: bar chart of cluster populations
    pp <- d$patch_pop
    pp_col <- setdiff(names(pp), "patches")[1]
    pp$selected <- pp$patches == d$selected_patch
    pp$patches <- factor(pp$patches)

    p2 <- ggplot2::ggplot(pp,
                           ggplot2::aes(x = .data$patches,
                                         y = .data[[pp_col]],
                                         fill = .data$selected)) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = c("TRUE" = "firebrick",
                                              "FALSE" = "grey60"),
                                  guide = "none") +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Cluster", y = "Total population",
                    title = "Cluster populations")

    p1 + p2 + patchwork::plot_layout(widths = c(2, 1))
  })

  plots <- plots[!vapply(plots, is.null, logical(1))]
  if (length(plots) == 0) return(invisible(NULL))
  if (length(plots) == 1) {
    print(plots[[1]])
    return(invisible(plots[[1]]))
  }
  combined <- patchwork::wrap_plots(plots, ncol = 1)
  print(combined)
  invisible(combined)
}

#' Prepare census tract shapefiles with population data for a Brazilian municipality
#'
#' Downloads census tract geometries via `geobr`, joins population data
#' from [br_prepare_population()], optionally clips to an urban area mask,
#' and removes unpopulated tracts.
#'
#' @param code_muni Numeric or character. IBGE municipality code.
#' @param pop_data Data frame. Output of [br_prepare_population()]. Must
#'   contain `code_tract` and population bracket columns.
#' @param remove_unpopulated Logical. Remove tracts where `pop_total == 0`.
#'   Default: TRUE.
#' @param clip_sf Optional `sf` polygon object. If provided, census tracts
#'   are clipped to this geometry (e.g., to remove non-urban areas like
#'   parks, forests, and water bodies). Population is proportionally
#'   adjusted based on the fraction of area retained.
#' @param year Integer. Census tract geometry year. Default: 2010.
#' @param crs Character or integer. CRS for the output. Default: SIRGAS 2000
#'   / Brazil Polyconic (EPSG:5880).
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return An `sf` object with census tract polygons and columns:
#'   `code_tract`, all `pop_*` bracket columns from `pop_data`, and
#'   `pop_total` (sum of all brackets).
#'
#' @details
#' Requires the `geobr`, `sf`, and `dplyr` packages.
#'
#' @examples
#' \dontrun{
#' pop <- br_prepare_population("3550308", year = 2010)
#' tracts <- br_prepare_tracts("3550308", pop)
#' }
#'
#' @seealso [br_prepare_population()] to generate `pop_data`.
#'
#' @family Brazil helpers
#' @export
br_prepare_tracts <- function(
    code_muni,
    pop_data,
    remove_unpopulated = TRUE,
    clip_sf = NULL,
    year = 2010,
    crs = "EPSG:5880",
    verbose = TRUE
) {
  for (pkg in c("geobr", "sf", "dplyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "The '%s' package is required for br_prepare_tracts()", pkg
      ), call. = FALSE)
    }
  }

  code_muni <- as.character(code_muni)

  # Disable S2 for planar operations
  old_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))

  # Download census tract shapefiles (with persistent cache)
  cache_name <- sprintf("tracts_%s_%d.rds", code_muni, year)
  shape <- .load_from_cache(cache_name, .cache_subdirs()$tracts)

  if (is.null(shape)) {
    if (verbose) message("  Downloading census tract geometries...")
    shape <- tryCatch(
      suppressMessages(geobr::read_census_tract(
        code_tract = as.numeric(code_muni),
        year = year
      )),
      error = function(e) {
        stop(sprintf(
          "Failed to download census tracts from geobr for municipality %s (year %d): %s",
          code_muni, year, e$message
        ), call. = FALSE)
      }
    )
    # Cache the raw geobr result before any transformations
    tryCatch(
      .save_to_cache(shape, cache_name, .cache_subdirs()$tracts),
      error = function(e) {
        if (verbose) {
          warning("Failed to cache tract geometries: ",
                  conditionMessage(e), call. = FALSE)
        }
      }
    )
  } else {
    if (verbose) message("  Using cached geometries")
  }

  shape <- sf::st_transform(shape, crs = crs)
  shape <- sf::st_make_valid(shape)

  # Filter population data for this municipality
  pop_mun <- dplyr::filter(pop_data, .data$code_muni == !!code_muni)

  # Ensure code_tract is character for joining
  shape$code_tract <- as.character(shape$code_tract)
  pop_mun$code_tract <- as.character(pop_mun$code_tract)

  # Join
  pop_cols <- grep("^pop_", names(pop_mun), value = TRUE)
  pop_join <- pop_mun[, c("code_tract", pop_cols)]

  result <- dplyr::left_join(shape, pop_join, by = "code_tract")

  # Select relevant columns
  result <- result[, c("code_tract", pop_cols,
                        attr(result, "sf_column"))]

  # Fill NA with 0
  for (col in pop_cols) {
    result[[col]][is.na(result[[col]])] <- 0
  }

  # Compute total population
  result$pop_total <- rowSums(
    sf::st_drop_geometry(result)[, pop_cols, drop = FALSE]
  )

  # Optionally clip to urban area
  if (!is.null(clip_sf)) {
    if (verbose) message("Clipping tracts to provided geometry...")
    clip_sf <- sf::st_transform(clip_sf, sf::st_crs(result))
    clip_sf <- sf::st_make_valid(clip_sf)

    # Compute original areas for proportional adjustment
    orig_area <- as.numeric(sf::st_area(result))
    orig_tracts <- result$code_tract

    result <- sf::st_intersection(result, sf::st_union(clip_sf))

    # Filter to polygon/multipolygon geometries only (intersection can
    # produce lines/points at shared boundaries)
    geom_type <- sf::st_geometry_type(result)
    n_before_geom <- nrow(result)
    result <- result[geom_type %in% c("POLYGON", "MULTIPOLYGON"), ]
    n_dropped <- n_before_geom - nrow(result)
    if (n_dropped > 0 && verbose) {
      message(sprintf("Dropped %d non-polygon geometries after clipping",
                       n_dropped))
    }

    # Adjust population proportionally by area fraction
    new_area <- as.numeric(sf::st_area(result))
    area_idx <- match(result$code_tract, orig_tracts)
    frac <- ifelse(is.na(area_idx), 1,
                   new_area / pmax(orig_area[area_idx], 1e-10))
    for (col in pop_cols) {
      result[[col]] <- result[[col]] * frac
    }
    result$pop_total <- rowSums(
      sf::st_drop_geometry(result)[, pop_cols, drop = FALSE]
    )
  }

  # Remove unpopulated tracts
  if (remove_unpopulated) {
    n_before <- nrow(result)
    result <- dplyr::filter(result, .data$pop_total > 0)
    n_after <- nrow(result)
    if (n_before > n_after && verbose) {
      message(sprintf("  Removed %d unpopulated tracts (%d remaining)",
                       n_before - n_after, n_after))
    }
  }

  sf::st_sf(result)
}

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

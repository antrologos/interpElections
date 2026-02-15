# Script to generate pre-computed Brazilian example data for vignettes
# Run this once (requires internet access and installed packages)
#
# Generates .rds files for Boa Vista (IBGE 1400100), 2020 municipal election.
# These are loaded by the "brazilian-elections" vignette so it can use
# eval = TRUE without requiring internet at vignette build time.
#
# Usage:
#   Rscript inst/extdata/create_brazil_example_data.R

library(interpElections)

cat("=== Generating Brazilian example data for vignettes ===\n")
cat("Municipality: Boa Vista, RR (IBGE 1400100)\n")
cat("Election: 2020 municipal (Vereador)\n\n")

outdir <- file.path(
  system.file("extdata", package = "interpElections"),
  fsep = "/"
)
if (!dir.exists(outdir)) {
  outdir <- "inst/extdata"
}

# --- Step 1: Population data ---
cat("Step 1/4: Downloading census population data...\n")
pop_data <- br_prepare_population(code_muni = 1400100, year = 2022)
saveRDS(pop_data, file.path(outdir, "example_pop_data.rds"))
cat(sprintf("  -> %d tracts, %d columns\n", nrow(pop_data), ncol(pop_data)))

# --- Step 2: Tract geometries ---
cat("Step 2/4: Downloading census tract geometries...\n")
tracts_sf <- br_prepare_tracts(
  code_muni = 1400100,
  pop_data = pop_data,
  remove_unpopulated = TRUE,
  year = 2022,
  verbose = FALSE
)
saveRDS(tracts_sf, file.path(outdir, "example_tracts_sf.rds"))
cat(sprintf("  -> %d tracts with geometry\n", nrow(tracts_sf)))

# --- Step 3: Electoral data ---
cat("Step 3/4: Downloading electoral data from TSE...\n")
electoral_data <- br_prepare_electoral(
  code_muni_ibge = "1400100",
  code_muni_tse = "03018",
  uf = "RR",
  year = 2020,
  cargo = 13,
  turno = 1L,
  what = "candidates",
  verbose = FALSE
)
saveRDS(electoral_data, file.path(outdir, "example_electoral_data.rds"))
cat(sprintf("  -> %d polling locations, %d columns\n",
            nrow(electoral_data), ncol(electoral_data)))

# --- Step 4: Full result (if travel time data is available) ---
# This step requires r5r + Java + OSM data. If unavailable, create a
# result object from the intermediate data using a synthetic travel time matrix.
cat("Step 4/4: Creating example result object...\n")

tryCatch({
  br_result <- interpolate_election_br(
    code_muni = 1400100,
    year = 2020,
    cargo = 13,
    turno = 1L,
    what = "candidates",
    use_gpu = FALSE,
    verbose = FALSE
  )
  saveRDS(br_result, file.path(outdir, "example_br_result.rds"))
  cat("  -> Full pipeline result saved (with real travel times)\n")
}, error = function(e) {
  cat(sprintf("  -> Full pipeline failed: %s\n", conditionMessage(e)))
  cat("  -> Creating result with synthetic travel time matrix instead\n")

  n <- nrow(tracts_sf)
  m <- nrow(electoral_data)

  # Synthetic travel time matrix based on Euclidean distances
  tract_centroids <- sf::st_coordinates(
    suppressWarnings(sf::st_centroid(tracts_sf))
  )
  poll_coords <- as.matrix(electoral_data[, c("long", "lat")])
  # Euclidean distance, scaled to 0-60 minutes
  tt_matrix <- as.matrix(
    dist(rbind(tract_centroids, poll_coords))
  )[1:n, (n + 1):(n + m)]
  tt_matrix <- tt_matrix / max(tt_matrix) * 60
  rownames(tt_matrix) <- tracts_sf$code_tract
  colnames(tt_matrix) <- electoral_data$id

  # Convert electoral data to sf (required by .br_match_calibration)
  electoral_sf <- sf::st_as_sf(
    electoral_data,
    coords = c("long", "lat"),
    crs = 4326
  )

  # Find calibration columns (correct argument order: census_year first)
  calib <- interpElections:::.br_match_calibration(2022, tracts_sf, electoral_sf)

  # Use the modified sf objects (with aggregated columns)
  tracts_sf_cal <- calib$tracts_sf
  electoral_sf_cal <- calib$electoral_sf

  pop_matrix <- as.matrix(
    sf::st_drop_geometry(tracts_sf_cal[, calib$calib_zones])
  )
  source_matrix <- as.matrix(
    sf::st_drop_geometry(electoral_sf_cal[, calib$calib_sources])
  )

  # Optimize
  opt <- optimize_alpha(tt_matrix, pop_matrix, source_matrix,
                        use_gpu = FALSE, verbose = FALSE)

  # Interpolate: all numeric columns except id, lat, long, and calibration
  elec_df <- sf::st_drop_geometry(electoral_sf_cal)
  interp_cols <- setdiff(
    names(elec_df)[vapply(elec_df, is.numeric, logical(1))],
    c("id", "lat", "long", calib$calib_sources)
  )
  interp_data <- as.matrix(elec_df[, interp_cols, drop = FALSE])
  interpolated <- idw_interpolate(tt_matrix, opt$alpha, interp_data)

  # Build result object
  result_sf <- tracts_sf_cal
  for (col in colnames(interpolated)) {
    result_sf[[col]] <- interpolated[, col]
  }

  br_result <- structure(list(
    tracts_sf = result_sf,
    alpha = opt$alpha,
    optimization = list(
      value = opt$value, method = opt$method,
      convergence = opt$convergence, iterations = opt$iterations,
      elapsed = opt$elapsed, message = opt$message
    ),
    calibration = list(
      zones = calib$calib_zones, sources = calib$calib_sources
    ),
    interpolated_vars = interp_cols,
    weights = idw_weights(tt_matrix, opt$alpha),
    time_matrix = tt_matrix,
    code_muni = 1400100,
    year = 2020,
    census_year = 2022,
    what = "candidates"
  ), class = "interpElections_result")

  saveRDS(br_result, file.path(outdir, "example_br_result.rds"))
  cat("  -> Synthetic result saved\n")
})

cat("\n=== Done! Files saved to:", outdir, "===\n")
cat("Files created:\n")
for (f in c("example_pop_data.rds", "example_tracts_sf.rds",
            "example_electoral_data.rds", "example_br_result.rds")) {
  fp <- file.path(outdir, f)
  if (file.exists(fp)) {
    sz <- file.info(fp)$size
    cat(sprintf("  %s (%.1f KB)\n", f, sz / 1024))
  }
}

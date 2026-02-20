# ── precompute.R ──────────────────────────────────────────────────────────────
#
# Generates all pre-computed artifacts (RDS data + PNG figures) required by
# the interpElections vignettes. Run this script locally BEFORE deploying
# the pkgdown site. It is NOT executed during pkgdown::build_site().
#
# Requirements: torch, r5r, Java 21, osmium-tool, sf, ggplot2, terra,
#   geobr, censobr, network access.
#
# Usage:
#   Rscript vignettes/precompute.R
#
# Output:
#   vignettes/precomputed/*.rds   — intermediate R objects
#   vignettes/figures/*.png       — rendered plot figures
# ─────────────────────────────────────────────────────────────────────────────

library(interpElections)
library(ggplot2)
library(sf)
library(dplyr)
library(maptiles)
library(stars)
library(ggspatial)
library(terra)
library(tidyr)
library(stringr)

sf::sf_use_s2(FALSE)

out_data <- "vignettes/precomputed"
out_fig  <- "vignettes/figures"
dir.create(out_data, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig,  recursive = TRUE, showWarnings = FALSE)

# Helper: save ggplot as PNG with consistent sizing
save_fig <- function(p, filename, width = 8, height = 6, dpi = 200) {
  path <- file.path(out_fig, filename)
  ggsave(path, plot = p, width = width, height = height, dpi = dpi,
         bg = "white")
  message("  Saved: ", path)
}

# Helper: aggregate calibration matrix from 28 cols (gender x literacy x age)
# down to 7 age brackets by summing across gender and literacy categories.
# Returns a matrix with clean age labels as column names.
aggregate_to_age <- function(mat, col_names = colnames(mat)) {
  n_cols <- length(col_names)
  # Extract age suffix: e.g., "pop_hom_alf_18_19" -> "18_19"
  age_suffix <- sapply(strsplit(col_names, "_"), function(parts) {
    paste(parts[4:length(parts)], collapse = "_")
  })
  unique_ages <- unique(age_suffix)
  n_ages <- length(unique_ages)

  agg <- matrix(0, nrow = nrow(mat), ncol = n_ages)
  for (j in seq_len(n_ages)) {
    idx <- which(age_suffix == unique_ages[j])
    agg[, j] <- rowSums(mat[, idx, drop = FALSE])
  }
  # Clean labels: "18_19" -> "18-19", "60_mais" -> "60+"
  labels <- gsub("_", "-", unique_ages)
  labels <- gsub("-mais$", "+", labels)
  colnames(agg) <- labels
  agg
}

# Helper: aggregate column-level sums to age brackets
aggregate_sums_to_age <- function(col_sums, col_names = names(col_sums)) {
  age_suffix <- sapply(strsplit(col_names, "_"), function(parts) {
    paste(parts[4:length(parts)], collapse = "_")
  })
  unique_ages <- unique(age_suffix)
  agg <- vapply(unique_ages, function(a) {
    sum(col_sums[age_suffix == a])
  }, numeric(1))
  labels <- gsub("_", "-", unique_ages)
  labels <- gsub("-mais$", "+", labels)
  names(agg) <- labels
  agg
}

# Utility: normalize 15-digit IBGE tract codes (avoid scientific notation)
normalize_code <- function(x) {
  x <- as.character(x)
  sci_mask <- grepl("[eE]", x)
  if (any(sci_mask)) {
    x[sci_mask] <- format(as.numeric(x[sci_mask]),
                          scientific = FALSE, trim = TRUE)
  }
  x
}

# Helper: extend r5r route to start/end at exact specified coordinates
extend_route <- function(route_sf, origin_lon, origin_lat,
                         dest_lon, dest_lat) {
  route_4326 <- st_transform(route_sf, 4326)
  coords <- st_coordinates(route_4326)
  xy <- coords[, c("X", "Y"), drop = FALSE]
  full_coords <- rbind(
    c(origin_lon, origin_lat), xy, c(dest_lon, dest_lat))
  st_sf(id = 1,
        geometry = st_sfc(st_linestring(full_coords), crs = 4326))
}


# ═══════════════════════════════════════════════════════════════════════════════
# 1. VARGINHA (MG) — methodology + get-started vignettes (CPU)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("VARGINHA (MG) — Full pipeline with intermediate objects")
message(strrep("=", 70))

# ── 1a. Run full pipeline via wrapper (resolves TSE code automatically) ───────

message("\n[Varginha] Running full interpolation pipeline...")
result_vga <- interpolate_election_br(
  "Varginha", year = 2022,
  cargo = "presidente",
  what = c("candidates", "turnout"),
  keep = c("weights", "sources_sf", "time_matrix", "pop_raster", "rep_points")
)

# ── 1b. Extract intermediate objects from result ──────────────────────────────

message("\n[Varginha] Extracting intermediates...")
tracts_sf_vga    <- result_vga$tracts_sf
electoral_sf_vga <- result_vga$sources_sf
time_matrix_vga  <- result_vga$time_matrix
W_vga            <- result_vga$weights
optim_vga        <- result_vga$optimization
interpolated_vga <- result_vga$interpolated
interp_cols_vga  <- result_vga$interp_cols
row_targets_vga  <- result_vga$row_targets
pop_data_vga     <- result_vga$pop_data

# Ensure id columns exist
if (!"id" %in% names(tracts_sf_vga))
  tracts_sf_vga$id <- as.character(tracts_sf_vga$code_tract)
if (!"id" %in% names(electoral_sf_vga))
  electoral_sf_vga$id <- as.character(seq_len(nrow(electoral_sf_vga)))

# ── 1c. Build calibration matrices (for methodology figures) ─────────────────

message("\n[Varginha] Building calibration matrices...")
calib_cols_vga <- result_vga$calib_cols
tracts_df_vga <- st_drop_geometry(tracts_sf_vga)
elec_df_vga   <- st_drop_geometry(electoral_sf_vga)

pop_matrix_vga <- as.matrix(tracts_df_vga[, calib_cols_vga$tracts])
storage.mode(pop_matrix_vga) <- "double"
source_matrix_vga <- as.matrix(elec_df_vga[, calib_cols_vga$sources])
storage.mode(source_matrix_vga) <- "double"
pop_total_vga <- rowSums(pop_matrix_vga)

# ── 1d. Representative points (all 3 methods for comparison) ─────────────────

message("\n[Varginha] Computing representative points (3 methods)...")
pts_centroid <- compute_representative_points(
  tracts_sf_vga, method = "centroid", tract_id = "id"
)
pts_pos <- compute_representative_points(
  tracts_sf_vga, method = "point_on_surface", tract_id = "id"
)
pts_pop <- compute_representative_points(
  tracts_sf_vga, method = "pop_weighted", tract_id = "id"
)
pop_raster_vga <- attr(pts_pop, "pop_raster")

# ── 1e. Sample routes for visualization ───────────────────────────────────────

message("\n[Varginha] Computing sample route geometries...")
routes_vga <- tryCatch({
  tt_min <- apply(time_matrix_vga, 1, min)
  tract_near <- which.min(tt_min)
  tract_far  <- which.max(tt_min)
  tract_mid  <- which.min(abs(tt_min - median(tt_min)))

  sample_pairs <- data.frame(
    tract_idx = c(tract_near, tract_mid, tract_far),
    stringsAsFactors = FALSE
  )
  sample_pairs$station_idx <- sapply(sample_pairs$tract_idx, function(i) {
    which.min(time_matrix_vga[i, ])
  })

  origins <- pts_pos[sample_pairs$tract_idx, ]
  origin_coords <- st_coordinates(origins)
  dest_coords <- st_coordinates(electoral_sf_vga[sample_pairs$station_idx, ])

  origin_df <- data.frame(
    id = paste0("origin_", seq_len(nrow(sample_pairs))),
    lon = origin_coords[, 1], lat = origin_coords[, 2]
  )
  dest_df <- data.frame(
    id = paste0("dest_", seq_len(nrow(sample_pairs))),
    lon = dest_coords[, 1], lat = dest_coords[, 2]
  )

  if (requireNamespace("r5r", quietly = TRUE)) {
    cache_dir <- get_interpElections_cache_dir()
    r5r_dirs <- list.dirs(file.path(cache_dir, "r5r"), recursive = TRUE)
    pbf_files <- list.files(r5r_dirs, pattern = "\\.pbf$",
                            full.names = TRUE, recursive = TRUE)
    if (length(pbf_files) > 0) {
      network_dir <- dirname(pbf_files[1])
      r5r_core <- r5r::setup_r5(network_dir, verbose = FALSE)
      on.exit(r5r::stop_r5(r5r_core), add = TRUE)

      routes <- r5r::detailed_itineraries(
        r5r_core,
        origins = origin_df,
        destinations = dest_df,
        mode = "WALK",
        max_trip_duration = 300L,
        shortest_path = TRUE
      )
      routes
    } else {
      NULL
    }
  } else {
    NULL
  }
}, error = function(e) {
  message("  Could not compute routes: ", conditionMessage(e))
  NULL
})

# ── 1f. Save all Varginha data ────────────────────────────────────────────────

message("\n[Varginha] Saving RDS...")
saveRDS(list(
  pop_data       = pop_data_vga,
  tracts_sf      = tracts_sf_vga,
  electoral_sf   = electoral_sf_vga,
  pts_centroid   = pts_centroid,
  pts_pos        = pts_pos,
  pts_pop        = pts_pop,
  time_matrix    = time_matrix_vga,
  optim_result   = optim_vga,
  W              = W_vga,
  interpolated   = interpolated_vga,
  pop_matrix     = pop_matrix_vga,
  source_matrix  = source_matrix_vga,
  row_targets    = row_targets_vga,
  calib_cols     = calib_cols_vga,
  interp_cols    = interp_cols_vga,
  routes         = routes_vga,
  result         = result_vga
), file.path(out_data, "varginha_2022.rds"))


# ── 1j. Generate Varginha plots ──────────────────────────────────────────────

message("\n[Varginha] Generating figures...")

# --- Methodology vignette figures ---

# 3.2: Population pyramid (percentages, younger at bottom)
# pop_matrix_vga has gender x 7 age columns; aggregate to 7 age brackets
# or 7 columns (age-only). Aggregate to 7 age brackets for the pyramid.
n_ages <- 7L
n_calib_cols <- ncol(pop_matrix_vga)
age_totals <- if (n_calib_cols == n_ages) {
  colSums(pop_matrix_vga)
} else {
  # Full calibration: columns are ordered by category, 7 ages each
  # Sum columns i, i+7, i+14, i+21 for each age bracket i
  sapply(seq_len(n_ages), function(i) {
    cols <- seq(i, n_calib_cols, by = n_ages)
    sum(pop_matrix_vga[, cols])
  })
}
# Determine bracket labels based on election year (2022 uses 18-19 not 18-20)
age_labels <- c("18-19", "20-24", "25-29", "30-39", "40-49", "50-59", "60-69")
pop_brackets_vga <- data.frame(
  bracket = age_labels,
  total = age_totals
)
pop_brackets_vga$pct <- pop_brackets_vga$total / sum(pop_brackets_vga$total) * 100
pop_brackets_vga$bracket <- factor(pop_brackets_vga$bracket,
                                    levels = pop_brackets_vga$bracket)
p <- ggplot(pop_brackets_vga, aes(x = pct, y = bracket)) +
  geom_col(fill = "#4575b4", alpha = 0.85) +
  labs(title = "Census Population by Age Bracket",
       subtitle = sprintf("Varginha (MG) \u2014 %d tracts, total pop: %s",
                           nrow(pop_matrix_vga),
                           format(sum(pop_matrix_vga), big.mark = ",")),
       x = "% of total population", y = "Age bracket") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
save_fig(p, "pop-pyramid.png", width = 7, height = 5)

# 3.3: Tracts population density choropleth
tracts_plot <- tracts_sf_vga
tracts_plot$pop_total <- pop_total_vga
p <- ggplot(tracts_plot) +
  geom_sf(aes(fill = pop_total), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Population") +
  labs(title = "Population per Census Tract",
       subtitle = sprintf("Varginha (MG) — %d tracts", nrow(tracts_plot))) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom")
save_fig(p, "tracts-pop-map.png")

# 3.4: Electoral stations map
p <- ggplot() +
  geom_sf(data = tracts_sf_vga, fill = "grey95", color = "grey70",
          linewidth = 0.15) +
  geom_sf(data = electoral_sf_vga, aes(size = QT_COMPARECIMENTO),
          color = "#d73027", alpha = 0.6) +
  scale_size_continuous(name = "Voters", range = c(0.5, 4)) +
  labs(title = "Polling Stations over Census Tracts",
       subtitle = sprintf("Varginha (MG) — %d stations, %d tracts",
                           nrow(electoral_sf_vga), nrow(tracts_sf_vga))) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom")
save_fig(p, "electoral-stations-map.png")

# 3.5: Age pyramids comparison (census vs electoral, percentages)
# Aggregate source_matrix across gender categories to get 7 age brackets
elec_totals <- if (ncol(source_matrix_vga) == n_ages) {
  colSums(source_matrix_vga)
} else {
  sapply(seq_len(n_ages), function(i) {
    cols <- seq(i, ncol(source_matrix_vga), by = n_ages)
    sum(source_matrix_vga[, cols])
  })
}
census_pcts <- pop_brackets_vga$total / sum(pop_brackets_vga$total) * 100
elec_pcts <- elec_totals / sum(elec_totals) * 100
pyramid_df <- data.frame(
  bracket = rep(pop_brackets_vga$bracket, 2),
  pct = c(census_pcts, elec_pcts),
  source = rep(c("Census (population)", "TSE (voters)"),
               each = nrow(pop_brackets_vga))
)
p <- ggplot(pyramid_df, aes(x = pct, y = bracket, fill = source)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("#4575b4", "#d73027")) +
  labs(title = "Census Population vs. Registered Voters by Age",
       subtitle = "Varginha (MG) \u2014 2022 Census vs. 2022 Election",
       x = "% of group total", y = "Age bracket", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")
save_fig(p, "age-pyramids-comparison.png", width = 8, height = 5)

# 3.7: Travel time matrix heatmap (subset)
n_show <- min(25, nrow(time_matrix_vga))
m_show <- min(15, ncol(time_matrix_vga))
tt_sub <- time_matrix_vga[seq_len(n_show), seq_len(m_show)]
tt_long <- expand.grid(tract = seq_len(n_show), station = seq_len(m_show))
tt_long$time <- as.vector(tt_sub)
p <- ggplot(tt_long, aes(x = station, y = tract, fill = time)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                        name = "Travel time (min)") +
  labs(title = "Travel Time Matrix (subset)",
       subtitle = sprintf("%d tracts x %d stations", n_show, m_show),
       x = "Station index", y = "Tract index") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
save_fig(p, "tt-heatmap.png", width = 7, height = 6)

# 3.7: Accessibility map
tracts_plot$min_tt <- apply(time_matrix_vga, 1, min)
p <- ggplot(tracts_plot) +
  geom_sf(aes(fill = min_tt), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                        name = "Minutes to\nnearest station") +
  labs(title = "Accessibility: Minimum Travel Time",
       subtitle = "Varginha (MG)") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom")
save_fig(p, "tt-accessibility-map.png")

# 3.12: Optimization convergence
if (!is.null(optim_vga$history) && length(optim_vga$history) > 1) {
  conv_df <- data.frame(
    step = seq_along(optim_vga$history),
    objective = optim_vga$history
  )
  p <- ggplot(conv_df, aes(x = step, y = objective)) +
    geom_line(color = "#4575b4", linewidth = 0.8) +
    geom_point(color = "#4575b4", size = 1) +
    scale_y_log10() +
    labs(title = "Optimization Convergence",
         subtitle = sprintf("Varginha — %s, %d steps, %.1fs",
                             optim_vga$method,
                             optim_vga$iterations,
                             as.numeric(optim_vga$elapsed, units = "secs")),
         x = "PB-SGD step", y = "Objective (SSE, log scale)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  save_fig(p, "optim-convergence.png", width = 7, height = 5)
}

# 3.13: Alpha histogram
alpha_df <- data.frame(alpha = optim_vga$alpha)
p <- ggplot(alpha_df, aes(x = alpha)) +
  geom_histogram(bins = 20, fill = "#4575b4", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(optim_vga$alpha), linetype = "dashed",
             color = "#d73027") +
  labs(title = "Distribution of Optimized Alpha Values",
       subtitle = sprintf("Varginha — median: %.2f, range: [%.2f, %.2f]",
                           median(optim_vga$alpha),
                           min(optim_vga$alpha), max(optim_vga$alpha)),
       x = expression(alpha), y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
save_fig(p, "alpha-histogram.png", width = 7, height = 5)

# 3.13: Alpha spatial map
tracts_plot$alpha <- optim_vga$alpha
p <- ggplot(tracts_plot) +
  geom_sf(aes(fill = alpha), color = "white", linewidth = 0.1) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                        name = expression(alpha)) +
  labs(title = "Spatial Distribution of Alpha",
       subtitle = "Varginha — Higher alpha = more concentrated weights") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey40"),
        legend.position = "bottom")
save_fig(p, "alpha-map.png")

# 3.14: Interpolated choropleth (Lula vs Bolsonaro)
cand_13_col <- grep("^CAND_13$", interp_cols_vga, value = TRUE)
cand_22_col <- grep("^CAND_22$", interp_cols_vga, value = TRUE)
qt_comp_col <- "QT_COMPARECIMENTO"

if (length(cand_13_col) > 0 && length(cand_22_col) > 0) {
  tracts_plot$pct_lula <- interpolated_vga[, cand_13_col] /
    interpolated_vga[, qt_comp_col] * 100
  tracts_plot$pct_bolso <- interpolated_vga[, cand_22_col] /
    interpolated_vga[, qt_comp_col] * 100

  p <- ggplot(tracts_plot) +
    geom_sf(aes(fill = pct_lula), color = "white", linewidth = 0.1) +
    scale_fill_distiller(palette = "RdYlBu", direction = -1,
                          name = "% of tract votes") +
    labs(title = "Lula (PT)", subtitle = "Varginha (MG) — 2022") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "grey40"),
          legend.position = "bottom")
  save_fig(p, "interp-lula.png")

  p <- ggplot(tracts_plot) +
    geom_sf(aes(fill = pct_bolso), color = "white", linewidth = 0.1) +
    scale_fill_distiller(palette = "RdYlBu", direction = -1,
                          name = "% of tract votes") +
    labs(title = "Bolsonaro (PL)", subtitle = "Varginha (MG) — 2022") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "grey40"),
          legend.position = "bottom")
  save_fig(p, "interp-bolsonaro.png")
}

# 3.14: Scatter plot — interpolated vs census demographics
# Aggregate 28 columns to 7 age brackets
fitted_demo <- W_vga %*% source_matrix_vga
fitted_agg <- aggregate_to_age(fitted_demo, calib_cols_vga$tracts)
pop_agg <- aggregate_to_age(pop_matrix_vga, calib_cols_vga$tracts)
bracket1_label <- colnames(fitted_agg)[1]
scatter_df <- data.frame(
  census = pop_agg[, 1],
  interpolated = fitted_agg[, 1]
)
r2 <- cor(scatter_df$census, scatter_df$interpolated)^2
p <- ggplot(scatter_df, aes(x = census, y = interpolated)) +
  geom_point(alpha = 0.6, color = "#4575b4") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Interpolated vs. Census Demographics",
       subtitle = sprintf("Varginha — Age %s (R\u00b2 = %.4f)", bracket1_label, r2),
       x = "Census population", y = "Interpolated value") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
save_fig(p, "interp-scatter-calib.png", width = 6, height = 6)

# 3.15: Residuals boxplot (aggregated to 7 age brackets)
resid_agg <- fitted_agg - pop_agg
resid_long <- tidyr::pivot_longer(
  as.data.frame(resid_agg),
  everything(), names_to = "bracket", values_to = "residual"
)
p <- ggplot(resid_long, aes(x = bracket, y = residual)) +
  geom_boxplot(fill = "#4575b4", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Calibration Residuals by Age Bracket",
       subtitle = "Varginha — Fitted minus observed",
       x = "Age bracket", y = "Residual") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
save_fig(p, "residuals-boxplot.png", width = 7, height = 5)

# Get-started vignette figures (using high-level result)
p_gs_lula <- plot(result_vga, variable = "Lula")
save_fig(p_gs_lula, "gs-vga-lula.png")

p_gs_facet <- plot(result_vga, variable = c("Lula", "Bolsonaro"),
                    type = "pct_tract")
save_fig(p_gs_facet, "gs-vga-lula-bolsonaro.png", width = 12, height = 6)


# ═══════════════════════════════════════════════════════════════════════════════
# 2. IGREJINHA (RS) — get-started vignette (CPU)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("IGREJINHA (RS) — Get Started vignette example")
message(strrep("=", 70))

result_igr <- interpolate_election_br(
  "Igrejinha", year = 2022,
  cargo = "presidente",
  what = c("candidates", "turnout"),
  keep = c("weights", "sources_sf")
)

saveRDS(result_igr, file.path(out_data, "igrejinha_2022.rds"))

message("\n[Igrejinha] Generating figures...")
p <- plot(result_igr, variable = "Lula")
save_fig(p, "gs-igr-lula.png")


# ═══════════════════════════════════════════════════════════════════════════════
# 3. NITEROI (RJ) — working-with-results vignette (GPU)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("NITEROI (RJ) — Working with Results vignette (GPU)")
message(strrep("=", 70))

result_nit <- interpolate_election_br(
  "Niteroi", year = 2022,
  cargo = "presidente",
  what = c("candidates", "parties", "turnout", "demographics"),
  keep = c("weights", "sources_sf", "time_matrix"),
  use_gpu = TRUE
)

saveRDS(result_nit, file.path(out_data, "niteroi_2022.rds"))

message("\n[Niteroi] Generating figures...")

# Choropleth - Lula
p <- plot(result_nit, variable = "Lula")
save_fig(p, "wr-nit-lula.png")

# Absolute
p <- plot(result_nit, variable = "Lula", type = "absolute")
save_fig(p, "wr-nit-lula-abs.png")

# Faceted
p <- plot(result_nit, variable = c("Lula", "Bolsonaro"), type = "pct_tract")
save_fig(p, "wr-nit-faceted.png", width = 12, height = 6)

# 6 type options grid — generate separately and arrange
types <- c("pct_tract", "absolute", "pct_muni", "pct_valid",
           "pct_eligible", "density")
for (tp in types) {
  p <- tryCatch(
    plot(result_nit, variable = "Lula", type = tp, scale_bar = FALSE),
    error = function(e) NULL
  )
  if (!is.null(p)) {
    save_fig(p, sprintf("wr-nit-type-%s.png", tp), width = 6, height = 5)
  }
}

# Breaks comparison
for (brk in c("quantile", "continuous", "jenks")) {
  p <- tryCatch(
    plot(result_nit, variable = "Lula", breaks = brk, scale_bar = FALSE),
    error = function(e) NULL
  )
  if (!is.null(p)) {
    save_fig(p, sprintf("wr-nit-breaks-%s.png", brk), width = 6, height = 5)
  }
}

# With source points
p <- plot(result_nit, variable = "Lula", show_sources = TRUE)
save_fig(p, "wr-nit-sources.png")

# Alpha spatial map
tracts_nit <- result_nit$tracts_sf
tracts_nit$alpha <- coef(result_nit)
p <- ggplot(tracts_nit) +
  geom_sf(aes(fill = alpha), color = "white", linewidth = 0.05) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                        name = expression(alpha)) +
  labs(title = "Spatial Distribution of Alpha",
       subtitle = "Niteroi (RJ)") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom")
save_fig(p, "wr-nit-alpha-map.png")

# Alpha histogram
p <- ggplot(data.frame(alpha = coef(result_nit)), aes(x = alpha)) +
  geom_histogram(bins = 25, fill = "#4575b4", color = "white", alpha = 0.85) +
  geom_vline(xintercept = median(coef(result_nit)), linetype = "dashed",
             color = "#d73027") +
  labs(title = "Alpha Distribution",
       subtitle = sprintf("Niteroi — median: %.2f",
                           median(coef(result_nit))),
       x = expression(alpha), y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
save_fig(p, "wr-nit-alpha-hist.png", width = 7, height = 5)

# Residuals boxplot (aggregated to 7 age brackets)
resid_nit <- residuals(result_nit)
if (!is.null(resid_nit)) {
  resid_nit_agg <- aggregate_to_age(resid_nit, colnames(resid_nit))
  resid_long_nit <- tidyr::pivot_longer(
    as.data.frame(resid_nit_agg),
    everything(), names_to = "bracket", values_to = "residual"
  )
  p <- ggplot(resid_long_nit, aes(x = bracket, y = residual)) +
    geom_boxplot(fill = "#4575b4", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = "Calibration Residuals",
         subtitle = "Niteroi",
         x = "Age bracket", y = "Residual") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  save_fig(p, "wr-nit-residuals.png", width = 7, height = 5)
}


# ═══════════════════════════════════════════════════════════════════════════════
# 4. BELO HORIZONTE (MG) — working-with-results vignette (GPU)
# ═══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("BELO HORIZONTE (MG) — Working with Results (GPU)")
message(strrep("=", 70))

result_bh <- interpolate_election_br(
  "Belo Horizonte", year = 2022,
  cargo = "presidente",
  what = c("candidates", "turnout"),
  keep = c("weights", "sources_sf"),
  use_gpu = TRUE
)

saveRDS(result_bh, file.path(out_data, "belo_horizonte_2022.rds"))

message("\n[BH] Generating figures...")

# BH Lula choropleth
p <- plot(result_bh, variable = "Lula")
save_fig(p, "wr-bh-lula.png")

# Areal aggregation example: BH regionais
message("\n[BH] Areal aggregation example...")
tryCatch({
  # Download BH bairros or regionais from geobr
  bairros_bh <- geobr::read_neighborhood(year = 2010)
  bairros_bh <- bairros_bh[bairros_bh$code_muni == 3106200, ]

  if (nrow(bairros_bh) > 0) {
    # Match CRS (geobr uses SIRGAS 2000 / 4674, tracts may use EPSG:5880)
    bairros_bh <- st_transform(bairros_bh, st_crs(result_bh$tracts_sf))

    # Compute areal weights
    W_areal <- areal_weights(
      bairros_bh, result_bh$tracts_sf,
      target_id = "code_neighborhood", source_id = "code_tract"
    )

    # Aggregate candidate votes
    vote_data <- result_bh$interpolated
    aggregated <- areal_interpolate(vote_data, W_areal)

    # Save for vignette
    saveRDS(list(
      bairros_sf = bairros_bh,
      W_areal = W_areal,
      aggregated = aggregated
    ), file.path(out_data, "bh_areal_2022.rds"))

    # Side-by-side maps (tract vs bairro) — generate later in vignette
    message("  Areal aggregation data saved.")
  }
}, error = function(e) {
  message("  Areal aggregation failed: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 5. SAO PAULO (SP) — Representative points comparison (South)
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("SAO PAULO (SP) — Representative points (south)")
message(strrep("=", 70))

tryCatch({
  message("\n[SP] Preparing tracts...")
  pop_sp <- br_prepare_population(3550308, year = 2010)
  tracts_sp <- br_prepare_tracts(3550308, pop_sp, verbose = FALSE)

  # Filter to southern periphery (large rural/peri-urban tracts)
  tracts_sp_wgs <- st_transform(tracts_sp, 4326)
  centroids_sp <- st_centroid(tracts_sp_wgs)
  lat_sp <- st_coordinates(centroids_sp)[, 2]
  areas_sp <- as.numeric(st_area(tracts_sp)) / 1e6

  # South SP: large tracts below -23.78 latitude
  south_mask <- lat_sp < -23.78 & areas_sp > 0.3
  tracts_south <- tracts_sp[south_mask, ]
  message(sprintf("  %d southern tracts selected (of %d total)",
                  nrow(tracts_south), nrow(tracts_sp)))

  # Compute representative points (3 methods)
  message("[SP] Computing representative points (3 methods)...")
  if (!"id" %in% names(tracts_south))
    tracts_south$id <- as.character(tracts_south$code_tract)

  pts_sp_c <- compute_representative_points(
    tracts_south, method = "centroid", tract_id = "id"
  )
  pts_sp_p <- compute_representative_points(
    tracts_south, method = "point_on_surface", tract_id = "id"
  )
  pts_sp_w <- compute_representative_points(
    tracts_south, method = "pop_weighted",
    tract_id = "id", pop_min_area = 0.5
  )
  pop_raster_sp <- attr(pts_sp_w, "pop_raster")

  # Save RDS
  saveRDS(list(
    tracts_south = tracts_south,
    pts_centroid = pts_sp_c,
    pts_pos = pts_sp_p,
    pts_pop = pts_sp_w,
    pop_raster = pop_raster_sp
  ), file.path(out_data, "saopaulo_rep_points.rds"))

  # --- Figures (OSM background, coord_sf limits, larger fonts) ---
  message("[SP] Generating figures...")

  # Transform all to WGS84 for consistent plotting
  ts_wgs <- st_transform(tracts_south, 4326)
  pc_wgs <- st_transform(pts_sp_c, 4326)
  pp_wgs <- st_transform(pts_sp_p, 4326)
  pw_wgs <- st_transform(pts_sp_w, 4326)

  # Download neutral basemap tiles
  bb_sp <- st_bbox(ts_wgs)
  message("[SP] Downloading basemap tiles...")
  basemap_sp <- tryCatch({
    tiles <- get_tiles(x = st_as_sfc(bb_sp),
                       provider = "CartoDB.Positron",
                       crop = TRUE, zoom = 12)
    st_as_stars(tiles)
  }, error = function(e) {
    message("  Basemap download failed: ", conditionMessage(e))
    NULL
  })

  # 3-panel comparison map with OSM background and coord_sf limits
  pts_all <- rbind(
    cbind(pc_wgs[, "id"], method = "Centroid"),
    cbind(pp_wgs[, "id"], method = "Point on Surface"),
    cbind(pw_wgs[, "id"], method = "Pop-Weighted")
  )

  p <- ggplot()
  if (!is.null(basemap_sp)) {
    p <- p + layer_spatial(basemap_sp, dpi = 300)
  }
  p <- p +
    geom_sf(data = ts_wgs, fill = NA,
            color = "grey40", linewidth = 0.3) +
    geom_sf(data = pts_all, aes(color = method),
            size = 2, alpha = 0.9) +
    scale_color_manual(values = c(
      "Centroid" = "#d73027",
      "Point on Surface" = "#4575b4",
      "Pop-Weighted" = "#1a9850"
    )) +
    facet_wrap(~method, ncol = 3) +
    coord_sf(xlim = c(bb_sp["xmin"], bb_sp["xmax"]),
             ylim = c(bb_sp["ymin"], bb_sp["ymax"])) +
    labs(
      title = "Representative Point Methods Compared",
      subtitle = paste0(
        "South S\u00e3o Paulo \u2014 ",
        nrow(tracts_south), " tracts"
      )
    ) +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(color = "grey40", size = 13),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 14)
    )
  save_fig(p, "rep-points-saopaulo-comparison.png",
           width = 16, height = 7)

  # Zoom inset with OSM background
  biggest_idx <- which.max(areas_sp[south_mask])
  zoom_tract <- ts_wgs[biggest_idx, ]
  zoom_bb <- st_bbox(zoom_tract)

  # Expand bbox 10%
  dx <- (zoom_bb["xmax"] - zoom_bb["xmin"]) * 0.1
  dy <- (zoom_bb["ymax"] - zoom_bb["ymin"]) * 0.1
  zoom_bb["xmin"] <- zoom_bb["xmin"] - dx
  zoom_bb["xmax"] <- zoom_bb["xmax"] + dx
  zoom_bb["ymin"] <- zoom_bb["ymin"] - dy
  zoom_bb["ymax"] <- zoom_bb["ymax"] + dy

  basemap_zoom <- tryCatch({
    tiles_z <- get_tiles(x = st_as_sfc(zoom_bb),
                         provider = "CartoDB.Positron",
                         crop = TRUE, zoom = 14)
    st_as_stars(tiles_z)
  }, error = function(e) NULL)

  pts_zoom <- data.frame(
    lon = c(st_coordinates(pc_wgs[biggest_idx, ])[1],
            st_coordinates(pp_wgs[biggest_idx, ])[1],
            st_coordinates(pw_wgs[biggest_idx, ])[1]),
    lat = c(st_coordinates(pc_wgs[biggest_idx, ])[2],
            st_coordinates(pp_wgs[biggest_idx, ])[2],
            st_coordinates(pw_wgs[biggest_idx, ])[2]),
    method = c("Centroid", "Point on Surface",
               "Pop-Weighted")
  )
  pts_zoom_sf <- st_as_sf(pts_zoom, coords = c("lon", "lat"),
                           crs = 4326)

  p_zoom <- ggplot()
  if (!is.null(basemap_zoom)) {
    p_zoom <- p_zoom + layer_spatial(basemap_zoom, dpi = 300)
  }
  p_zoom <- p_zoom +
    geom_sf(data = zoom_tract, fill = NA,
            color = "grey30", linewidth = 0.6) +
    geom_sf(data = pts_zoom_sf,
            aes(color = method), size = 5, shape = 17) +
    scale_color_manual(values = c(
      "Centroid" = "#d73027",
      "Point on Surface" = "#4575b4",
      "Pop-Weighted" = "#1a9850"
    )) +
    coord_sf(
      xlim = c(zoom_bb["xmin"], zoom_bb["xmax"]),
      ylim = c(zoom_bb["ymin"], zoom_bb["ymax"])
    ) +
    labs(
      title = "Zoomed: Largest Tract",
      subtitle = "Centroid may fall in uninhabited area",
      color = "Method"
    ) +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 12)
    )
  save_fig(p_zoom, "rep-points-zoom-inset.png",
           width = 8, height = 8)

  message("  [SP] Done.")
}, error = function(e) {
  message("  [SP] FAILED: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 6. PALMAS (TO) — Voronoi vs Sinkhorn comparison
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("PALMAS (TO) — Voronoi comparison")
message(strrep("=", 70))

tryCatch({
  message("\n[Palmas] Running full pipeline...")
  result_pal <- interpolate_election_br(
    "Palmas", uf = "TO", year = 2022,
    cargo = "presidente",
    what = c("candidates", "turnout"),
    keep = c("weights", "sources_sf", "time_matrix")
  )

  tracts_pal <- result_pal$tracts_sf
  elec_pal <- result_pal$sources_sf
  W_pal <- result_pal$weights
  calib_pal <- result_pal$calib_cols

  # Build calibration matrices
  tracts_df_pal <- st_drop_geometry(tracts_pal)
  elec_df_pal <- st_drop_geometry(elec_pal)
  pop_mat_pal <- as.matrix(
    tracts_df_pal[, calib_pal$tracts]
  )
  src_mat_pal <- as.matrix(
    elec_df_pal[, calib_pal$sources]
  )
  storage.mode(pop_mat_pal) <- "double"
  storage.mode(src_mat_pal) <- "double"

  # Sinkhorn interpolated demographics
  fitted_pal <- W_pal %*% src_mat_pal

  # --- Voronoi assignment ---
  message("[Palmas] Computing Voronoi assignment...")
  # Find nearest station for each tract
  tract_pts <- st_centroid(tracts_pal)
  elec_pts <- st_transform(elec_pal, st_crs(tracts_pal))
  dist_mat <- st_distance(tract_pts, elec_pts)
  nearest_idx <- apply(dist_mat, 1, which.min)

  # Voronoi: each tract gets 100% of nearest station
  voronoi_fitted <- src_mat_pal[nearest_idx, , drop = FALSE]

  # Voronoi polygons for visualization
  muni_boundary <- st_union(tracts_pal)
  elec_union <- st_union(elec_pts)
  voronoi_raw <- st_voronoi(elec_union, envelope = muni_boundary)
  voronoi_sf <- st_collection_extract(voronoi_raw, "POLYGON")
  voronoi_sf <- st_intersection(voronoi_sf, muni_boundary)
  voronoi_sf <- st_as_sf(data.frame(
    id = seq_along(voronoi_sf),
    geometry = voronoi_sf
  ))

  # Compute SSE for both methods
  sse_sinkhorn <- sum((fitted_pal - pop_mat_pal)^2)
  sse_voronoi <- sum((voronoi_fitted - pop_mat_pal)^2)
  message(sprintf(
    "  SSE Sinkhorn: %.0f | SSE Voronoi: %.0f (ratio: %.1fx)",
    sse_sinkhorn, sse_voronoi,
    sse_voronoi / max(sse_sinkhorn, 1)
  ))

  # Save RDS
  saveRDS(list(
    result = result_pal,
    tracts_sf = tracts_pal,
    electoral_sf = elec_pal,
    pop_matrix = pop_mat_pal,
    source_matrix = src_mat_pal,
    fitted_sinkhorn = fitted_pal,
    fitted_voronoi = voronoi_fitted,
    nearest_station = nearest_idx,
    voronoi_sf = voronoi_sf,
    sse_sinkhorn = sse_sinkhorn,
    sse_voronoi = sse_voronoi,
    calib_cols = calib_pal
  ), file.path(out_data, "palmas_2022.rds"))

  # --- Figures ---
  message("[Palmas] Generating figures...")

  # Voronoi map
  # Color tracts by their assigned station
  tracts_vor <- tracts_pal
  tracts_vor$station <- factor(nearest_idx)
  n_stations <- length(unique(nearest_idx))

  p <- ggplot() +
    geom_sf(data = voronoi_sf, fill = NA,
            color = "red", linewidth = 0.6,
            linetype = "dashed") +
    geom_sf(data = tracts_vor,
            aes(fill = station),
            color = "white", linewidth = 0.05,
            alpha = 0.7) +
    geom_sf(data = elec_pts, color = "black",
            size = 2, shape = 17) +
    scale_fill_manual(
      values = rep(
        c("#4575b4", "#d73027", "#1a9850",
          "#fdae61", "#abd9e9", "#fee090",
          "#74add1", "#f46d43", "#a6d96a"),
        length.out = n_stations
      ),
      guide = "none"
    ) +
    labs(
      title = "Voronoi Assignment",
      subtitle = paste0(
        "Palmas (TO) \u2014 each tract assigned to ",
        "nearest station"
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40")
    )
  save_fig(p, "voronoi-palmas-map.png")

  # Scatter: predicted vs observed, both methods (aggregated to 7 age brackets)
  fitted_pal_agg <- aggregate_to_age(fitted_pal, calib_pal$tracts)
  pop_pal_agg <- aggregate_to_age(pop_mat_pal, calib_pal$tracts)
  voronoi_pal_agg <- aggregate_to_age(voronoi_fitted, calib_pal$tracts)
  age_labels_pal <- colnames(fitted_pal_agg)

  scatter_list <- list()
  for (k in seq_along(age_labels_pal)) {
    scatter_list[[length(scatter_list) + 1]] <- data.frame(
      observed = pop_pal_agg[, k],
      predicted = fitted_pal_agg[, k],
      bracket = age_labels_pal[k],
      method = "Sinkhorn IDW"
    )
    scatter_list[[length(scatter_list) + 1]] <- data.frame(
      observed = pop_pal_agg[, k],
      predicted = voronoi_pal_agg[, k],
      bracket = age_labels_pal[k],
      method = "Voronoi (nearest)"
    )
  }
  scatter_df_pal <- do.call(rbind, scatter_list)

  p <- ggplot(scatter_df_pal,
              aes(x = observed, y = predicted,
                  color = method)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey40") +
    scale_color_manual(values = c(
      "Sinkhorn IDW" = "#4575b4",
      "Voronoi (nearest)" = "#d73027"
    )) +
    facet_wrap(~method) +
    labs(
      title = "Predicted vs Observed Age Demographics",
      subtitle = sprintf(
        "Palmas (TO) \u2014 SSE: Sinkhorn=%.0f, Voronoi=%.0f",
        sse_sinkhorn, sse_voronoi
      ),
      x = "Census population", y = "Predicted",
      color = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "none"
    )
  save_fig(p, "voronoi-vs-sinkhorn-scatter.png",
           width = 10, height = 5)

  # Residual boxplot by bracket (aggregated to 7 age brackets)
  resid_list <- list()
  for (k in seq_along(age_labels_pal)) {
    resid_list[[length(resid_list) + 1]] <- data.frame(
      residual = fitted_pal_agg[, k] - pop_pal_agg[, k],
      bracket = age_labels_pal[k],
      method = "Sinkhorn IDW"
    )
    resid_list[[length(resid_list) + 1]] <- data.frame(
      residual = voronoi_pal_agg[, k] - pop_pal_agg[, k],
      bracket = age_labels_pal[k],
      method = "Voronoi (nearest)"
    )
  }
  resid_df_pal <- do.call(rbind, resid_list)

  p <- ggplot(resid_df_pal,
              aes(x = bracket, y = residual,
                  fill = method)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50") +
    scale_fill_manual(values = c(
      "Sinkhorn IDW" = "#4575b4",
      "Voronoi (nearest)" = "#d73027"
    )) +
    labs(
      title = "Calibration Residuals: Sinkhorn vs Voronoi",
      subtitle = "Palmas (TO) \u2014 fitted minus census",
      x = "Age bracket", y = "Residual", fill = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  save_fig(p, "voronoi-vs-sinkhorn-residuals.png",
           width = 9, height = 6)

  message("  [Palmas] Done.")
}, error = function(e) {
  message("  [Palmas] FAILED: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 7. RIO DE JANEIRO (RJ) — Age pyramids + Routes + Pipeline
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("RIO DE JANEIRO (RJ) — Age pyramids + routes + pipeline")
message(strrep("=", 70))

tryCatch({
  # ── 7a. Census age pyramids: Rocinha vs Copacabana (censobr 2022) ──

  message("\n[RJ] Downloading 2022 census Pessoas data...")
  pessoas_rj <- censobr::read_tracts(2022, "Pessoas") |>
    dplyr::filter(as.character(code_muni) == "3304557") |>
    dplyr::collect()

  # Convert to plain data.frame (censobr returns data.table)
  pessoas_rj <- as.data.frame(pessoas_rj)
  nm <- names(pessoas_rj)
  names(pessoas_rj) <- sub("^demografia_", "", nm)

  # Convert numeric columns
  num_cols <- setdiff(names(pessoas_rj), c("code_tract", "code_muni"))
  for (col in num_cols) {
    pessoas_rj[[col]] <- suppressWarnings(as.numeric(pessoas_rj[[col]]))
    pessoas_rj[[col]][is.na(pessoas_rj[[col]])] <- 0
  }
  # Normalize tract codes to avoid scientific notation from censobr
  pessoas_rj$code_tract <- normalize_code(pessoas_rj$code_tract)
  message(sprintf("  %d tracts, %d columns",
                  nrow(pessoas_rj), ncol(pessoas_rj)))

  # Total age columns (V01031-V01041 = 11 age brackets 0-4 to 70+)
  total_age_cols <- paste0("V010", sprintf("%02d", 31:41))
  has_total_age <- all(total_age_cols %in% names(pessoas_rj))

  # Get neighborhood boundaries
  message("[RJ] Loading neighborhood boundaries...")
  nbhoods_rj <- geobr::read_neighborhood(year = 2010)
  nbhoods_rj <- nbhoods_rj[nbhoods_rj$code_muni == 3304557, ]

  # Get 2022 tract geometries for spatial join
  message("[RJ] Loading 2022 tract geometries...")
  pop_rj_2022 <- br_prepare_population(3304557, year = 2022)
  tracts_rj_2022 <- br_prepare_tracts(3304557, pop_rj_2022,
                                       verbose = FALSE)
  nbhoods_rj <- st_transform(nbhoods_rj, st_crs(tracts_rj_2022))

  rocinha_nb <- nbhoods_rj[grepl("Rocinha",
    nbhoods_rj$name_neighborhood, ignore.case = TRUE), ]
  copacabana_nb <- nbhoods_rj[grepl("Copacabana",
    nbhoods_rj$name_neighborhood, ignore.case = TRUE), ]

  # Spatial join: tracts in each neighborhood
  tracts_rj_pts <- st_point_on_surface(tracts_rj_2022)
  roc_mask <- lengths(st_intersects(tracts_rj_pts, rocinha_nb)) > 0
  cop_mask <- lengths(st_intersects(tracts_rj_pts, copacabana_nb)) > 0
  roc_codes <- normalize_code(tracts_rj_2022$code_tract[roc_mask])
  cop_codes <- normalize_code(tracts_rj_2022$code_tract[cop_mask])
  message(sprintf("  Tracts: Rocinha=%d, Copacabana=%d",
                  length(roc_codes), length(cop_codes)))

  if (has_total_age) {
    age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24",
                     "25-29", "30-39", "40-49", "50-59",
                     "60-69", "70+")

    roc_data <- pessoas_rj[pessoas_rj$code_tract %in% roc_codes, ]
    cop_data <- pessoas_rj[pessoas_rj$code_tract %in% cop_codes, ]

    roc_totals <- colSums(
      roc_data[, total_age_cols, drop = FALSE])
    cop_totals <- colSums(
      cop_data[, total_age_cols, drop = FALSE])

    # Sex ratio from V01002 (males) / V01003 (females)
    if ("V01002" %in% names(pessoas_rj) &&
        "V01003" %in% names(pessoas_rj)) {
      roc_mr <- sum(roc_data$V01002, na.rm = TRUE) /
        (sum(roc_data$V01002, na.rm = TRUE) +
         sum(roc_data$V01003, na.rm = TRUE))
      cop_mr <- sum(cop_data$V01002, na.rm = TRUE) /
        (sum(cop_data$V01002, na.rm = TRUE) +
         sum(cop_data$V01003, na.rm = TRUE))
    } else {
      roc_mr <- 0.48; cop_mr <- 0.45
    }

    # Convert to PERCENTAGES within each neighborhood
    roc_pcts <- roc_totals / sum(roc_totals) * 100
    cop_pcts <- cop_totals / sum(cop_totals) * 100

    pyramid_df <- data.frame(
      bracket = rep(age_labels, 4),
      pct = c(-roc_pcts * roc_mr, roc_pcts * (1 - roc_mr),
              -cop_pcts * cop_mr, cop_pcts * (1 - cop_mr)),
      sex = rep(c("Male", "Female", "Male", "Female"),
                each = 11),
      neighborhood = rep(c("Rocinha", "Rocinha",
                            "Copacabana", "Copacabana"),
                          each = 11)
    )
    # Younger at bottom: no rev()
    pyramid_df$bracket <- factor(pyramid_df$bracket,
                                  levels = age_labels)
    max_abs <- max(abs(pyramid_df$pct))

    p <- ggplot(pyramid_df,
                aes(x = pct, y = bracket, fill = sex)) +
      geom_col(width = 0.75) +
      scale_fill_manual(values = c("Male" = "#4575b4",
                                    "Female" = "#d73027")) +
      facet_wrap(~neighborhood) +
      scale_x_continuous(
        labels = function(x) paste0(format(abs(x), digits = 1), "%"),
        limits = c(-max_abs * 1.08, max_abs * 1.08)
      ) +
      labs(
        title = "Age Structure: Rocinha vs Copacabana",
        subtitle = "Rio de Janeiro",
        x = "% of neighborhood population", y = "Age bracket", fill = "",
        caption = paste0(
          "Source: IBGE Demographic Census 2022 (via censobr). ",
          "Males shown to the left, females to the right."
        )
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "grey40",
                                      size = 13),
        plot.caption = element_text(color = "grey50", size = 9,
                                     hjust = 0),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)
      )
    save_fig(p, "age-pyramids-rocinha-copacabana.png",
             width = 12, height = 7)
  }

  # ── 7b. Full pipeline (GPU) for age recovery figure ────────────

  message("\n[RJ] Running full interpolation pipeline (GPU)...")
  result_rj <- interpolate_election_br(
    "Rio de Janeiro", year = 2022,
    cargo = "presidente",
    what = c("candidates", "turnout"),
    keep = c("weights", "sources_sf", "time_matrix"),
    use_gpu = TRUE
  )

  # Extract calibration data for recovery figure
  calib_rj <- result_rj$calib_cols
  tracts_rj_result <- result_rj$tracts_sf
  W_rj <- result_rj$weights
  elec_rj <- result_rj$sources_sf
  elec_df_rj <- st_drop_geometry(elec_rj)
  src_mat_rj <- as.matrix(
    elec_df_rj[, calib_rj$sources]
  )
  storage.mode(src_mat_rj) <- "double"
  fitted_rj <- W_rj %*% src_mat_rj

  # Match Rocinha/Copacabana tracts in the result
  tracts_rj_pts2 <- st_point_on_surface(tracts_rj_result)
  if (exists("rocinha_nb") && nrow(rocinha_nb) > 0) {
    roc_mask2 <- lengths(
      st_intersects(tracts_rj_pts2, rocinha_nb)) > 0
    cop_mask2 <- lengths(
      st_intersects(tracts_rj_pts2, copacabana_nb)) > 0

    roc_result_df <- st_drop_geometry(
      tracts_rj_result[roc_mask2, ])
    cop_result_df <- st_drop_geometry(
      tracts_rj_result[cop_mask2, ])

    calib_tract_cols <- calib_rj$tracts
    # Aggregate 28-column sums to 7 age brackets
    roc_census_raw <- colSums(
      roc_result_df[, calib_tract_cols, drop = FALSE])
    cop_census_raw <- colSums(
      cop_result_df[, calib_tract_cols, drop = FALSE])
    roc_interp_raw <- colSums(
      fitted_rj[roc_mask2, , drop = FALSE])
    cop_interp_raw <- colSums(
      fitted_rj[cop_mask2, , drop = FALSE])

    roc_census <- aggregate_sums_to_age(roc_census_raw,
                                         calib_tract_cols)
    cop_census <- aggregate_sums_to_age(cop_census_raw,
                                         calib_tract_cols)
    roc_interp <- aggregate_sums_to_age(roc_interp_raw,
                                         calib_tract_cols)
    cop_interp <- aggregate_sums_to_age(cop_interp_raw,
                                         calib_tract_cols)
    bracket_labels_rj <- names(roc_census)

    # Convert to PERCENTAGES within each neighborhood x source group
    roc_census_pct <- roc_census / sum(roc_census) * 100
    roc_interp_pct <- roc_interp / sum(roc_interp) * 100
    cop_census_pct <- cop_census / sum(cop_census) * 100
    cop_interp_pct <- cop_interp / sum(cop_interp) * 100

    recovery_df <- data.frame(
      bracket = rep(bracket_labels_rj, 4),
      pct = c(roc_census_pct, roc_interp_pct,
              cop_census_pct, cop_interp_pct),
      source = rep(c("Census", "Interpolated",
                      "Census", "Interpolated"),
                    each = length(bracket_labels_rj)),
      neighborhood = rep(
        c("Rocinha", "Rocinha",
          "Copacabana", "Copacabana"),
        each = length(bracket_labels_rj)
      )
    )
    # Younger at bottom: no rev()
    recovery_df$bracket <- factor(
      recovery_df$bracket,
      levels = bracket_labels_rj
    )

    # Same x-axis limits for both neighborhoods
    max_pct <- max(recovery_df$pct) * 1.05

    p <- ggplot(recovery_df,
                aes(x = pct, y = bracket, fill = source)) +
      geom_col(position = "dodge", alpha = 0.85) +
      scale_fill_manual(values = c(
        "Census" = "#4575b4",
        "Interpolated" = "#fdae61"
      )) +
      facet_wrap(~neighborhood) +
      scale_x_continuous(
        limits = c(0, max_pct),
        labels = function(x) paste0(format(x, digits = 1), "%")
      ) +
      labs(
        title = "Age Structure Recovery: Census vs Interpolated",
        subtitle = paste0(
          "Rio de Janeiro 2022 \u2014 Sinkhorn-interpolated ",
          "voter profiles vs census truth"
        ),
        x = "% of group total",
        y = "Age bracket", fill = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(color = "grey40",
                                      size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)
      )
    save_fig(p, "age-recovery-rocinha-copacabana.png",
             width = 12, height = 6)
  }

  # ── 7c. Route: Botafogo -> Laranjeiras (Corcovado mountain) ─────

  message("\n[RJ] Computing route: Botafogo -> Laranjeiras...")
  origin_df_rj <- data.frame(
    id = "botafogo", lon = -43.188008, lat = -22.947474)
  dest_df_rj <- data.frame(
    id = "laranjeiras", lon = -43.193558, lat = -22.938640)

  routes_rj <- tryCatch({
    # Build dedicated network for Botafogo-Laranjeiras area
    cache_dir <- get_interpElections_cache_dir()
    rj_route_dir <- file.path(cache_dir, "networks", "r5r",
                              "rj_botafogo_routes")
    dir.create(rj_route_dir, recursive = TRUE, showWarnings = FALSE)
    pbf_files <- list.files(rj_route_dir, pattern = "\\.pbf$",
                            full.names = TRUE)
    if (length(pbf_files) == 0) {
      message("  Downloading OSM data for Botafogo area...")
      rj_bb <- st_as_sf(
        data.frame(lon = c(-43.22, -43.16),
                   lat = c(-22.97, -22.92)),
        coords = c("lon", "lat"), crs = 4326)
      download_r5r_data(area_sf = rj_bb,
                        output_dir = rj_route_dir, verbose = TRUE)
    }
    r5r_core <- r5r::setup_r5(rj_route_dir, verbose = FALSE)
    route_lines <- r5r::detailed_itineraries(
      r5r_core,
      origins = origin_df_rj, destinations = dest_df_rj,
      mode = "WALK", max_trip_duration = 120L,
      shortest_path = TRUE)
    r5r::stop_r5(r5r_core)
    route_lines
  }, error = function(e) {
    message("  Route failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(routes_rj) && nrow(routes_rj) > 0) {
    message("[RJ] Generating route figure...")
    # Extend route to match euclidean endpoints
    route_sf <- extend_route(routes_rj,
      origin_df_rj$lon, origin_df_rj$lat,
      dest_df_rj$lon, dest_df_rj$lat)
    bb_route <- st_bbox(route_sf)
    pad <- 0.25 * max(0.005, max(bb_route["xmax"] - bb_route["xmin"],
                                   bb_route["ymax"] - bb_route["ymin"]))
    bb_route["xmin"] <- bb_route["xmin"] - pad
    bb_route["xmax"] <- bb_route["xmax"] + pad
    bb_route["ymin"] <- bb_route["ymin"] - pad
    bb_route["ymax"] <- bb_route["ymax"] + pad

    basemap_rj <- tryCatch({
      tiles <- get_tiles(x = st_as_sfc(bb_route),
        provider = "CartoDB.Positron",
        crop = TRUE, zoom = 15)
      st_as_stars(tiles)
    }, error = function(e) NULL)

    p_route <- ggplot()
    if (!is.null(basemap_rj)) {
      p_route <- p_route + layer_spatial(basemap_rj, dpi = 300)
    }
    p_route <- p_route +
      annotate("segment",
        x = origin_df_rj$lon, y = origin_df_rj$lat,
        xend = dest_df_rj$lon, yend = dest_df_rj$lat,
        linetype = "dotted", color = "#d73027",
        linewidth = 1) +
      geom_sf(data = route_sf, color = "#002D72",
              linewidth = 2.5, alpha = 0.85) +
      geom_sf(data = st_as_sf(origin_df_rj,
        coords = c("lon", "lat"), crs = 4326),
        color = "#d73027", size = 5, shape = 16) +
      geom_sf(data = st_as_sf(dest_df_rj,
        coords = c("lon", "lat"), crs = 4326),
        color = "#1a9850", size = 5, shape = 17) +
      annotate("label",
        x = origin_df_rj$lon - 0.002,
        y = origin_df_rj$lat,
        label = "Botafogo\n(origin)",
        size = 3.5, fontface = "bold",
        fill = "white", alpha = 0.8) +
      annotate("label",
        x = dest_df_rj$lon + 0.002,
        y = dest_df_rj$lat,
        label = "Laranjeiras\n(destination)",
        size = 3.5, fontface = "bold",
        fill = "white", alpha = 0.8) +
      coord_sf(xlim = c(bb_route["xmin"], bb_route["xmax"]),
               ylim = c(bb_route["ymin"], bb_route["ymax"])) +
      labs(
        title = "Mountain Barrier: Botafogo to Laranjeiras",
        subtitle = paste0(
          "Rio de Janeiro \u2014 Corcovado/Sumar\u00e9 hills ",
          "between origin and destination\n",
          "Dotted red = euclidean, solid blue = walking"),
        caption = paste0(
          "Map tiles: CartoDB Positron. ",
          "Route: r5r on OpenStreetMap.")
      ) +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "grey40",
                                      size = 10),
        plot.caption = element_text(color = "grey60", size = 8)
      )
    save_fig(p_route, "routes-rj-botafogo-laranjeiras.png",
             width = 10, height = 9)
  }

  # Save RDS with everything
  saveRDS(list(
    pop_data_2022 = pop_rj_2022,
    tracts_rj_2022 = tracts_rj_2022,
    result = result_rj,
    fitted_demographics = fitted_rj,
    routes = routes_rj,
    neighborhoods = nbhoods_rj
  ), file.path(out_data, "rio_methodology.rds"))

  message("  [RJ] Done.")
}, error = function(e) {
  message("  [RJ] FAILED: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 8. RECIFE (PE) — Water barrier route example
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("RECIFE (PE) — Water barrier route")
message(strrep("=", 70))

tryCatch({
  message("\n[Recife] Computing cross-river route...")

  origin <- data.frame(
    id = "boa_vista",
    lon = -34.885756, lat = -8.059575)
  dest <- data.frame(
    id = "brasilia_teimosa",
    lon = -34.880807, lat = -8.085098)

  routes_rec <- tryCatch({
    if (!requireNamespace("r5r", quietly = TRUE)) {
      message("  r5r not available, skipping")
      NULL
    } else {
      # Build dedicated network for Boa Vista - Brasilia Teimosa area
      cache_dir <- get_interpElections_cache_dir()
      rec_route_dir <- file.path(
        cache_dir, "networks", "r5r", "recife_boa_vista_routes")
      dir.create(rec_route_dir, recursive = TRUE,
                 showWarnings = FALSE)
      pbf_files <- list.files(rec_route_dir, pattern = "\\.pbf$",
                              full.names = TRUE)
      if (length(pbf_files) == 0) {
        message("  Downloading OSM data for Boa Vista area...")
        rec_bb <- st_as_sf(
          data.frame(lon = c(-34.92, -34.85),
                     lat = c(-8.10, -8.04)),
          coords = c("lon", "lat"), crs = 4326)
        download_r5r_data(area_sf = rec_bb,
                          output_dir = rec_route_dir, verbose = TRUE)
        pbf_files <- list.files(rec_route_dir, pattern = "\\.pbf$",
                                full.names = TRUE)
      }

      if (length(pbf_files) > 0) {
        r5r_core <- r5r::setup_r5(
          rec_route_dir, verbose = FALSE)
        on.exit(r5r::stop_r5(r5r_core), add = TRUE)

        route_lines <- r5r::detailed_itineraries(
          r5r_core, origins = origin,
          destinations = dest, mode = "WALK",
          max_trip_duration = 120L,
          shortest_path = TRUE)
        route_lines
      } else {
        message("  No Recife PBF found")
        NULL
      }
    }
  }, error = function(e) {
    message("  Route failed: ", conditionMessage(e))
    NULL
  })

  if (!is.null(routes_rec) && nrow(routes_rec) > 0) {
    message("[Recife] Generating route figure...")
    # Extend route to match euclidean endpoints
    route_sf_rec <- extend_route(routes_rec,
      origin$lon, origin$lat, dest$lon, dest$lat)

    bb_rec <- st_bbox(route_sf_rec)
    pad_x <- 0.3 * max(0.003,
      bb_rec["xmax"] - bb_rec["xmin"])
    pad_y <- 0.3 * max(0.003,
      bb_rec["ymax"] - bb_rec["ymin"])
    bb_rec["xmin"] <- bb_rec["xmin"] - pad_x
    bb_rec["xmax"] <- bb_rec["xmax"] + pad_x
    bb_rec["ymin"] <- bb_rec["ymin"] - pad_y
    bb_rec["ymax"] <- bb_rec["ymax"] + pad_y

    basemap_rec <- tryCatch({
      tiles <- get_tiles(x = st_as_sfc(bb_rec),
        provider = "CartoDB.Positron",
        crop = TRUE, zoom = 15)
      st_as_stars(tiles)
    }, error = function(e) NULL)

    p_rec <- ggplot()
    if (!is.null(basemap_rec)) {
      p_rec <- p_rec + layer_spatial(basemap_rec, dpi = 300)
    }
    p_rec <- p_rec +
      annotate("segment",
        x = origin$lon, y = origin$lat,
        xend = dest$lon, yend = dest$lat,
        linetype = "dotted", color = "#d73027",
        linewidth = 1) +
      geom_sf(data = route_sf_rec, color = "#002D72",
              linewidth = 2.5, alpha = 0.85) +
      geom_sf(data = st_as_sf(origin,
        coords = c("lon", "lat"), crs = 4326),
        color = "#d73027", size = 5, shape = 16) +
      geom_sf(data = st_as_sf(dest,
        coords = c("lon", "lat"), crs = 4326),
        color = "#1a9850", size = 5, shape = 17) +
      annotate("label",
        x = origin$lon, y = origin$lat - 0.002,
        label = "Boa Vista\n(origin)",
        size = 3.5, fontface = "bold",
        fill = "white", alpha = 0.8) +
      annotate("label",
        x = dest$lon, y = dest$lat + 0.002,
        label = "Bras\u00edlia Teimosa\n(destination)",
        size = 3.5, fontface = "bold",
        fill = "white", alpha = 0.8) +
      coord_sf(xlim = c(bb_rec["xmin"], bb_rec["xmax"]),
               ylim = c(bb_rec["ymin"], bb_rec["ymax"])) +
      labs(
        title = "Water Barrier: Boa Vista to Bras\u00edlia Teimosa",
        subtitle = paste0(
          "Recife (PE) \u2014 water/estuary barrier ",
          "between origin and destination\n",
          "Dotted red = euclidean, solid blue = walking"),
        caption = paste0(
          "Map tiles: CartoDB Positron. ",
          "Route: r5r on OpenStreetMap.")
      ) +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "grey40",
                                      size = 10),
        plot.caption = element_text(color = "grey60", size = 8)
      )
    save_fig(p_rec, "routes-recife-water.png",
             width = 10, height = 9)
  }

  saveRDS(list(routes = routes_rec),
          file.path(out_data, "recife_routes.rds"))

  message("  [Recife] Done.")
}, error = function(e) {
  message("  [Recife] FAILED: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 9. PIPELINE DIAGRAM — Stacked tilted maps (using Varginha)
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("PIPELINE DIAGRAM — Stacked tilted maps")
message(strrep("=", 70))

tryCatch({
  message("\n[Diagram] Generating pipeline figure...")

  library(ggnewscale)

  # Use Varginha data already in memory
  vga_tracts_d <- st_transform(tracts_sf_vga, 4326)
  vga_stations_d <- st_transform(electoral_sf_vga, 4326)
  vga_tracts_d$pop_total <- pop_total_vga
  vga_tracts_d$pct_lula <- if (length(cand_13_col) > 0) {
    interpolated_vga[, cand_13_col] /
      interpolated_vga[, qt_comp_col] * 100
  } else { rep(50, nrow(vga_tracts_d)) }

  # 1) Municipality boundary from geobr (not tract union)
  message("  Downloading municipality boundary from geobr...")
  muni_sf <- geobr::read_municipality(3170701, year = 2022)
  muni_sf <- st_transform(muni_sf, 4326)

  # 2) Population raster from fresh WorldPop download
  message("  Computing pop-weighted rep points (fresh WorldPop raster)...")
  if (!"id" %in% names(tracts_sf_vga))
    tracts_sf_vga$id <- as.character(tracts_sf_vga$code_tract)
  pts_pop_fresh <- compute_representative_points(
    tracts_sf_vga, method = "pop_weighted", tract_id = "id")
  pop_raster_vga <- attr(pts_pop_fresh, "pop_raster")

  pop_sf_raster <- NULL
  if (!is.null(pop_raster_vga)) {
    pop_df <- as.data.frame(pop_raster_vga, xy = TRUE, na.rm = TRUE)
    if (ncol(pop_df) >= 3) {
      names(pop_df)[3] <- "pop"
      pop_df <- pop_df[pop_df$pop > 0, ]
      if (nrow(pop_df) > 8000) {
        set.seed(42)
        pop_df <- pop_df[sample(nrow(pop_df), 8000), ]
      }
      pop_sf_raster <- st_as_sf(pop_df, coords = c("x", "y"),
                                 crs = terra::crs(pop_raster_vga))
      pop_sf_raster <- st_transform(pop_sf_raster, 4326)
      # Crop raster to municipality boundary
      muni_buf <- st_buffer(muni_sf, 0)
      pop_sf_raster <- st_intersection(pop_sf_raster, muni_buf)
      message(sprintf("  Raster: %d cells (cropped to municipality)",
                      nrow(pop_sf_raster)))
    }
  }

  # Pop-weighted representative points
  pts_pop_d <- st_transform(pts_pop_fresh, 4326)

  # Affine shear/rotation function — less rotation for legibility
  rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
    shear_m <- matrix(c(2, 1.2, 0, 1), 2, 2)
    rot_angle <- pi / 40  # less rotation (was pi/20)
    rot_m <- matrix(c(cos(rot_angle), sin(rot_angle),
                       -sin(rot_angle), cos(rot_angle)), 2, 2)
    geom <- st_geometry(data)
    new_geom <- (geom * shear_m * rot_m) + c(x_add, y_add)
    st_set_geometry(data, new_geom)
  }

  # 7 layers with tight spacing (slight overlap for visual stacking)
  y_step <- 0.17
  n_layers <- if (!is.null(pop_sf_raster)) 7 else 5
  y_offsets <- seq(0, by = y_step, length.out = n_layers)

  tracts_outline <- st_sf(geometry = st_geometry(vga_tracts_d))

  # Sample routes from a few tracts to nearest stations
  set.seed(42)
  sample_idx <- sample(seq_len(nrow(vga_tracts_d)),
                        min(8, nrow(vga_tracts_d)))
  tract_centroids_d <- st_centroid(vga_tracts_d)
  route_lines <- do.call(c, lapply(sample_idx, function(i) {
    nearest_j <- which.min(time_matrix_vga[i, ])
    pt1 <- st_coordinates(tract_centroids_d[i, ])
    pt2 <- st_coordinates(vga_stations_d[nearest_j, ])
    st_sfc(st_linestring(rbind(pt1, pt2)), crs = 4326)
  }))
  route_sf_d <- st_sf(id = seq_along(route_lines),
                       geometry = route_lines)

  # Transform all 7 layers
  if (!is.null(pop_sf_raster)) {
    L1 <- rotate_data_geom(muni_sf, y_add = y_offsets[1])
    L2 <- rotate_data_geom(vga_tracts_d, y_add = y_offsets[2])
    L3_muni <- rotate_data_geom(muni_sf, y_add = y_offsets[3])
    L3r <- rotate_data_geom(pop_sf_raster, y_add = y_offsets[3])
    L4_muni <- rotate_data_geom(muni_sf, y_add = y_offsets[4])
    L4p <- rotate_data_geom(pts_pop_d, y_add = y_offsets[4])
    L5t <- rotate_data_geom(tracts_outline, y_add = y_offsets[5])
    L5s <- rotate_data_geom(vga_stations_d, y_add = y_offsets[5])
    L6t <- rotate_data_geom(tracts_outline, y_add = y_offsets[6])
    L6r <- rotate_data_geom(route_sf_d, y_add = y_offsets[6])
    L7 <- rotate_data_geom(vga_tracts_d, y_add = y_offsets[7])

    layer_labels <- c(
      "1. Municipality\n   boundary",
      "2. Census tracts\n   (population)",
      "3. Population\n   raster (WorldPop)",
      "4. Pop-weighted\n   representative points",
      "5. Polling\n   stations",
      "6. Walking\n   routes",
      "7. Interpolated\n   results")
    layer_bboxes <- list(
      st_bbox(L1), st_bbox(L2), st_bbox(L3r),
      st_bbox(L4p), st_bbox(L5t), st_bbox(L6t), st_bbox(L7))
    arrow_labels <- c(
      "Subdivide into\ncensus tracts",
      "Overlay population\ndensity grid",
      "Compute weighted\ncentroids",
      "Locate polling\nstations",
      "Calculate walking\ntravel times",
      "Sinkhorn IDW\ninterpolation")
  } else {
    L1 <- rotate_data_geom(muni_sf, y_add = y_offsets[1])
    L2 <- rotate_data_geom(vga_tracts_d, y_add = y_offsets[2])
    L5t <- rotate_data_geom(tracts_outline, y_add = y_offsets[3])
    L5s <- rotate_data_geom(vga_stations_d, y_add = y_offsets[3])
    L6t <- rotate_data_geom(tracts_outline, y_add = y_offsets[4])
    L6r <- rotate_data_geom(route_sf_d, y_add = y_offsets[4])
    L7 <- rotate_data_geom(vga_tracts_d, y_add = y_offsets[5])

    layer_labels <- c(
      "1. Municipality\n   boundary",
      "2. Census tracts\n   (population)",
      "3. Polling\n   stations",
      "4. Walking\n   routes",
      "5. Interpolated\n   results")
    layer_bboxes <- list(
      st_bbox(L1), st_bbox(L2),
      st_bbox(L5t), st_bbox(L6t), st_bbox(L7))
    arrow_labels <- c(
      "Subdivide into\ncensus tracts",
      "Locate polling\nstations",
      "Calculate walking\ntravel times",
      "Sinkhorn IDW\ninterpolation")
  }

  # LEFT SIDE labels
  bb_all <- Reduce(function(a, b) {
    c(xmin = min(a["xmin"], b["xmin"]),
      ymin = min(a["ymin"], b["ymin"]),
      xmax = max(a["xmax"], b["xmax"]),
      ymax = max(a["ymax"], b["ymax"]))
  }, layer_bboxes)
  label_x <- bb_all["xmin"] - 0.10
  labels_df <- data.frame(
    x = rep(label_x, length(layer_labels)),
    y = sapply(layer_bboxes, function(bb) mean(c(bb["ymin"], bb["ymax"]))),
    label = layer_labels
  )

  # RIGHT SIDE arrows — short, uniform, all pointing UP
  layer_centers <- sapply(layer_bboxes,
    function(bb) mean(c(bb["ymin"], bb["ymax"])))
  n_arrows <- length(layer_bboxes) - 1
  arrow_x <- bb_all["xmax"] + 0.06
  arrow_half_len <- (y_step / 3) / 2  # short arrows (1/3 of gap)
  arrows_df <- data.frame(
    x = rep(arrow_x, n_arrows),
    xend = rep(arrow_x, n_arrows),
    y = (layer_centers[1:n_arrows] +
         layer_centers[2:(n_arrows + 1)]) / 2 - arrow_half_len,
    yend = (layer_centers[1:n_arrows] +
            layer_centers[2:(n_arrows + 1)]) / 2 + arrow_half_len,
    label = arrow_labels,
    label_y = (layer_centers[1:n_arrows] +
               layer_centers[2:(n_arrows + 1)]) / 2
  )

  # Explicit coordinate limits to show all labels/arrows
  x_lim <- c(label_x - 0.15, arrow_x + 0.20)
  y_lim <- c(bb_all["ymin"] - 0.06, bb_all["ymax"] + 0.06)

  # Build plot
  p_pipe <- ggplot() +
    geom_sf(data = L1, fill = "#FCECC9",
            color = "#8B7355", linewidth = 0.6) +
    geom_sf(data = L2, aes(fill = pop_total),
            color = "grey50", linewidth = 0.08) +
    scale_fill_viridis_c(option = "viridis", direction = -1,
                          name = "Population", guide = "none")

  # Population raster layer (with grey municipality outline behind)
  if (!is.null(pop_sf_raster)) {
    p_pipe <- p_pipe +
      new_scale_fill() +
      geom_sf(data = L3_muni, fill = "#E8E8E8",
              color = "grey50", linewidth = 0.3) +
      new_scale_color() +
      geom_sf(data = L3r, aes(color = pop),
              size = 0.3, alpha = 0.7) +
      scale_color_viridis_c(option = "inferno", direction = -1,
                             guide = "none") +
      # Pop-weighted representative points (with grey outline)
      new_scale_color() +
      geom_sf(data = L4_muni, fill = "#E8E8E8",
              color = "grey50", linewidth = 0.3) +
      geom_sf(data = L4p, color = "#e66101",
              size = 1.5, alpha = 0.8)
  }

  p_pipe <- p_pipe +
    geom_sf(data = L5t, fill = "#E8E8E8",
            color = "grey50", linewidth = 0.08) +
    geom_sf(data = L5s, color = "#d73027",
            size = 2.5, alpha = 0.9) +
    geom_sf(data = L6t, fill = "#E8E8E8",
            color = "grey50", linewidth = 0.08) +
    geom_sf(data = L6r, color = "#002D72",
            linewidth = 0.8, alpha = 0.7) +
    # Interpolated results — FILLED polygons
    new_scale_fill() +
    geom_sf(data = L7, aes(fill = pct_lula),
            color = "white", linewidth = 0.05) +
    scale_fill_distiller(palette = "RdYlBu", direction = -1,
                          guide = "none") +
    # Labels on the left
    geom_text(data = labels_df,
              aes(x = x, y = y, label = label),
              size = 4.5, hjust = 1, fontface = "bold",
              color = "grey25", lineheight = 0.9) +
    # Arrows pointing UP (short, uniform) + process labels
    geom_segment(data = arrows_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
      linewidth = 1.0, color = "grey40") +
    geom_text(data = arrows_df,
      aes(x = x + 0.02, y = label_y, label = label),
      size = 3.8, hjust = 0, color = "grey40",
      fontface = "italic", lineheight = 0.85) +
    coord_sf(xlim = x_lim, ylim = y_lim, expand = FALSE,
             clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white",
                                      color = "white"),
      plot.title = element_text(face = "bold", size = 16,
                                 hjust = 0.5),
      plot.subtitle = element_text(color = "grey40", size = 11,
                                    hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(
      title = paste0("Interpolation Pipeline: From Spatial ",
                      "Data to Tract-Level Estimates"),
      subtitle = paste0(
        "Varginha (MG) \u2014 ",
        nrow(vga_tracts_d), " census tracts, ",
        nrow(vga_stations_d), " polling stations")
    )

  save_fig(p_pipe, "pipeline-stacked-maps.png",
           width = 14, height = 16, dpi = 250)

  message("  [Diagram] Done.")
}, error = function(e) {
  message("  [Diagram] FAILED: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 10. OPTIMIZATION LOOP DIAGRAM
# ═════════════════════════════════════════════════════════════════════

message("\n[Diagram] Generating optimization loop diagram...")

tryCatch({
  # Simple flowchart using ggplot2 geometry
  boxes <- data.frame(
    x = c(0, 2, 4, 4, 2, 0),
    y = c(3, 3, 3, 1, 1, 1),
    w = 1.6, h = 0.7,
    label = c(
      "1. Set initial\nalpha = 1",
      "2. Build IDW\nkernel",
      "3. Sinkhorn\nbalance",
      "4. Interpolate\nage profiles",
      "5. Compare with\ncensus (SSE)",
      "6. Adjust alpha\n(PB-SGD step)"
    ),
    fill = c("#e0e0e0", "#abdda4", "#abdda4",
             "#abdda4", "#fdae61", "#fdae61")
  )

  # Arrow connections
  arrows <- data.frame(
    x = c(0.8, 2.8, 4, 3.2, 1.2, 0),
    y = c(3, 3, 2.65, 1, 1, 1.35),
    xend = c(1.2, 3.2, 4, 2.8, 0.8, 0),
    yend = c(3, 3, 1.35, 1, 1, 2.65)
  )

  p_loop <- ggplot() +
    # Boxes
    geom_rect(
      data = boxes,
      aes(xmin = x - w / 2, xmax = x + w / 2,
          ymin = y - h / 2, ymax = y + h / 2,
          fill = fill),
      color = "grey40", linewidth = 0.5
    ) +
    scale_fill_identity() +
    # Labels
    geom_text(
      data = boxes,
      aes(x = x, y = y, label = label),
      size = 3, lineheight = 0.9
    ) +
    # Arrows
    geom_segment(
      data = arrows,
      aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.15, "cm"),
                    type = "closed"),
      linewidth = 0.6, color = "grey30"
    ) +
    # "Repeat" label on the feedback arrow
    annotate("text", x = -0.7, y = 2,
             label = "repeat\nuntil\nconverged",
             size = 2.8, color = "grey40",
             fontface = "italic") +
    coord_fixed(ratio = 1,
                xlim = c(-1.3, 5.3),
                ylim = c(0.2, 3.8)) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = "white", color = "white"
      ),
      plot.title = element_text(
        face = "bold", size = 12, hjust = 0.5
      )
    ) +
    labs(title = "Optimization Loop")

  save_fig(p_loop, "optimization-loop-diagram.png",
           width = 8, height = 5, dpi = 200)

  message("  [Optimization diagram] Done.")
}, error = function(e) {
  message("  [Optimization diagram] FAILED: ",
          conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 11. FLORIANOPOLIS (SC) — Water barrier (bay crossing)
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("FLORIANOPOLIS (SC) — Water barrier (bay crossing)")
message(strrep("=", 70))

tryCatch({
  cache_dir <- get_interpElections_cache_dir()
  flor_r5r_dir <- file.path(
    cache_dir, "networks", "r5r", "florianopolis_routes")
  dir.create(flor_r5r_dir, recursive = TRUE,
             showWarnings = FALSE)

  pbf_files <- list.files(flor_r5r_dir, pattern = "\\.pbf$",
                          full.names = TRUE)
  if (length(pbf_files) == 0) {
    message("  Downloading OSM data for Florianopolis...")
    flor_bb <- st_as_sf(
      data.frame(lon = c(-48.62, -48.48),
                 lat = c(-27.68, -27.56)),
      coords = c("lon", "lat"), crs = 4326)
    r5r_result <- download_r5r_data(
      area_sf = flor_bb,
      output_dir = flor_r5r_dir, verbose = TRUE)
    pbf_files <- list.files(flor_r5r_dir, pattern = "\\.pbf$",
                            full.names = TRUE)
  }

  if (length(pbf_files) > 0) {
    message("  Setting up r5r for Florianopolis...")
    r5r_core <- r5r::setup_r5(flor_r5r_dir, verbose = FALSE)

    origin_df_fl <- data.frame(
      id = "coqueiros", lon = -48.581466, lat = -27.611141)
    dest_df_fl <- data.frame(
      id = "costeira", lon = -48.524405, lat = -27.633252)

    message("  Computing cross-bay route...")
    routes_flor <- r5r::detailed_itineraries(
      r5r_core, origins = origin_df_fl,
      destinations = dest_df_fl, mode = "WALK",
      max_trip_duration = 180L, shortest_path = TRUE)
    r5r::stop_r5(r5r_core)

    if (!is.null(routes_flor) && nrow(routes_flor) > 0) {
      message("[Florianopolis] Generating route figure...")
      # Extend route to match euclidean endpoints
      route_sf_fl <- extend_route(routes_flor,
        origin_df_fl$lon, origin_df_fl$lat,
        dest_df_fl$lon, dest_df_fl$lat)
      bb_fl <- st_bbox(route_sf_fl)
      pad_x <- 0.35 * max(0.005,
        bb_fl["xmax"] - bb_fl["xmin"])
      pad_y <- 0.35 * max(0.005,
        bb_fl["ymax"] - bb_fl["ymin"])
      bb_fl["xmin"] <- bb_fl["xmin"] - pad_x
      bb_fl["xmax"] <- bb_fl["xmax"] + pad_x
      bb_fl["ymin"] <- bb_fl["ymin"] - pad_y
      bb_fl["ymax"] <- bb_fl["ymax"] + pad_y

      basemap_fl <- tryCatch({
        tiles <- get_tiles(x = st_as_sfc(bb_fl),
          provider = "CartoDB.Positron",
          crop = TRUE, zoom = 13)
        st_as_stars(tiles)
      }, error = function(e) NULL)

      p_fl <- ggplot()
      if (!is.null(basemap_fl)) {
        p_fl <- p_fl + layer_spatial(basemap_fl, dpi = 300)
      }
      p_fl <- p_fl +
        annotate("segment",
          x = origin_df_fl$lon, y = origin_df_fl$lat,
          xend = dest_df_fl$lon, yend = dest_df_fl$lat,
          linetype = "dotted", color = "#d73027",
          linewidth = 1) +
        geom_sf(data = route_sf_fl, color = "#002D72",
                linewidth = 2.5, alpha = 0.85) +
        geom_sf(data = st_as_sf(origin_df_fl,
          coords = c("lon", "lat"), crs = 4326),
          color = "#d73027", size = 5, shape = 16) +
        geom_sf(data = st_as_sf(dest_df_fl,
          coords = c("lon", "lat"), crs = 4326),
          color = "#1a9850", size = 5, shape = 17) +
        annotate("label",
          x = origin_df_fl$lon - 0.005,
          y = origin_df_fl$lat,
          label = "Coqueiros\n(mainland)",
          size = 3.5, fontface = "bold",
          fill = "white", alpha = 0.8) +
        annotate("label",
          x = dest_df_fl$lon + 0.005,
          y = dest_df_fl$lat,
          label = "Costeira do\nPirajuba\u00e9 (island)",
          size = 3.5, fontface = "bold",
          fill = "white", alpha = 0.8) +
        coord_sf(xlim = c(bb_fl["xmin"], bb_fl["xmax"]),
                 ylim = c(bb_fl["ymin"], bb_fl["ymax"])) +
        labs(
          title = "Water Barrier: Coqueiros to Costeira do Pirajuba\u00e9",
          subtitle = paste0(
            "Florian\u00f3polis (SC) \u2014 bay crossing ",
            "between mainland and island\n",
            "Dotted red = euclidean, solid blue = walking"),
          caption = paste0(
            "Map tiles: CartoDB Positron. ",
            "Route: r5r on OpenStreetMap.")
        ) +
        theme_void(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(color = "grey40",
                                        size = 10),
          plot.caption = element_text(color = "grey60",
                                       size = 8)
        )
      save_fig(p_fl, "routes-florianopolis-water.png",
               width = 10, height = 9)

      saveRDS(list(routes = routes_flor),
              file.path(out_data, "florianopolis_routes.rds"))
    }
  }
  message("  [Florianopolis] Done.")
}, error = function(e) {
  message("  [Florianopolis] FAILED: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 12. MULTI-CITY VORONOI — Cuiaba, Manaus, Natal
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("MULTI-CITY VORONOI — Cuiaba, Manaus, Natal")
message(strrep("=", 70))

tryCatch({
  voronoi_cities <- list(
    list(name = "Cuiab\u00e1", uf = "MT"),
    list(name = "Manaus",  uf = "AM"),
    list(name = "Natal",   uf = "RN")
  )

  voronoi_results <- list()
  for (city in voronoi_cities) {
    message(sprintf("\n  [%s] Running full pipeline...",
                    city$name))
    r <- tryCatch({
      result <- interpolate_election_br(
        city$name, uf = city$uf, year = 2022,
        cargo = "presidente",
        what = c("candidates", "turnout"),
        keep = c("weights", "sources_sf", "time_matrix"))

      tracts <- result$tracts_sf
      elec <- result$sources_sf
      W <- result$weights
      calib <- result$calib_cols
      tracts_df <- st_drop_geometry(tracts)
      elec_df <- st_drop_geometry(elec)
      pop_mat <- as.matrix(tracts_df[, calib$tracts])
      src_mat <- as.matrix(elec_df[, calib$sources])
      storage.mode(pop_mat) <- "double"
      storage.mode(src_mat) <- "double"

      fitted_sink <- W %*% src_mat
      tract_pts <- st_centroid(tracts)
      elec_pts <- st_transform(elec, st_crs(tracts))
      dist_mat <- st_distance(tract_pts, elec_pts)
      nearest_idx <- apply(dist_mat, 1, which.min)
      fitted_vor <- src_mat[nearest_idx, , drop = FALSE]

      sse_s <- sum((fitted_sink - pop_mat)^2)
      sse_v <- sum((fitted_vor - pop_mat)^2)
      message(sprintf(
        "  [%s] SSE: Sinkhorn=%.0f, Voronoi=%.0f (%.1fx)",
        city$name, sse_s, sse_v,
        sse_v / max(sse_s, 1)))

      city_data <- list(
        city = city$name,
        pop_matrix = pop_mat, source_matrix = src_mat,
        fitted_sinkhorn = fitted_sink,
        fitted_voronoi = fitted_vor,
        sse_sinkhorn = sse_s, sse_voronoi = sse_v,
        calib_cols = calib,
        n_tracts = nrow(tracts),
        n_stations = nrow(elec))
      saveRDS(city_data, file.path(out_data,
        sprintf("%s_2022.rds",
          tolower(gsub("[^a-zA-Z]", "", city$name)))))
      city_data
    }, error = function(e) {
      message(sprintf("  [%s] FAILED: %s",
                      city$name, conditionMessage(e)))
      NULL
    })
    if (!is.null(r)) voronoi_results[[city$name]] <- r
  }

  # Combined figures with all 4 cities
  palmas_data <- readRDS(file.path(out_data, "palmas_2022.rds"))

  all_sse <- data.frame(
    city = "Palmas (TO)",
    sse_sinkhorn = palmas_data$sse_sinkhorn,
    sse_voronoi = palmas_data$sse_voronoi,
    stringsAsFactors = FALSE)

  for (nm in names(voronoi_results)) {
    r <- voronoi_results[[nm]]
    uf <- voronoi_cities[[which(
      sapply(voronoi_cities, function(x) x$name) == nm)]]$uf
    all_sse <- rbind(all_sse, data.frame(
      city = sprintf("%s (%s)", nm, uf),
      sse_sinkhorn = r$sse_sinkhorn,
      sse_voronoi = r$sse_voronoi,
      stringsAsFactors = FALSE))
  }
  all_sse$ratio <- all_sse$sse_voronoi /
    pmax(all_sse$sse_sinkhorn, 1)
  all_sse$city <- factor(all_sse$city,
    levels = all_sse$city[order(all_sse$ratio)])

  # SSE ratio bar chart
  p <- ggplot(all_sse, aes(x = city, y = ratio)) +
    geom_col(fill = "#4575b4", alpha = 0.85, width = 0.6) +
    geom_text(aes(label = sprintf("%.0fx", ratio)),
              hjust = -0.2, size = 4.5, fontface = "bold") +
    geom_hline(yintercept = 1, linetype = "dashed",
               color = "grey50") +
    coord_flip(ylim = c(0, max(all_sse$ratio) * 1.15)) +
    labs(
      title = "Voronoi vs Sinkhorn: SSE Ratio by City",
      subtitle = paste0(
        "How many times worse is Voronoi (nearest-station) ",
        "compared to Sinkhorn IDW?"),
      x = "", y = "SSE(Voronoi) / SSE(Sinkhorn)",
      caption = paste0(
        "SSE = sum of squared errors in age bracket recovery. ",
        "Higher ratio = larger Sinkhorn advantage.")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40",
                                    size = 11),
      plot.caption = element_text(color = "grey50", size = 8,
                                   hjust = 0),
      axis.text = element_text(size = 12)
    )
  save_fig(p, "voronoi-sse-ratio-cities.png",
           width = 10, height = 6)

  # Combined scatter plot (aggregated to 7 age brackets)
  all_scatter <- list()
  pal_cols <- palmas_data$calib_cols$tracts
  pop_pal_a <- aggregate_to_age(palmas_data$pop_matrix, pal_cols)
  sk_pal_a <- aggregate_to_age(palmas_data$fitted_sinkhorn, pal_cols)
  vor_pal_a <- aggregate_to_age(palmas_data$fitted_voronoi, pal_cols)
  age_labs <- colnames(pop_pal_a)

  for (k in seq_along(age_labs)) {
    all_scatter[[length(all_scatter) + 1]] <- data.frame(
      observed = pop_pal_a[, k],
      predicted = sk_pal_a[, k],
      bracket = age_labs[k],
      method = "Sinkhorn", city = "Palmas (TO)")
    all_scatter[[length(all_scatter) + 1]] <- data.frame(
      observed = pop_pal_a[, k],
      predicted = vor_pal_a[, k],
      bracket = age_labs[k],
      method = "Voronoi", city = "Palmas (TO)")
  }
  for (nm in names(voronoi_results)) {
    r <- voronoi_results[[nm]]
    uf <- voronoi_cities[[which(
      sapply(voronoi_cities, function(x) x$name) == nm)]]$uf
    city_label <- sprintf("%s (%s)", nm, uf)
    r_cols <- r$calib_cols$tracts
    r_pop_a <- aggregate_to_age(r$pop_matrix, r_cols)
    r_sk_a <- aggregate_to_age(r$fitted_sinkhorn, r_cols)
    r_vor_a <- aggregate_to_age(r$fitted_voronoi, r_cols)
    r_labs <- colnames(r_pop_a)
    for (k in seq_along(r_labs)) {
      all_scatter[[length(all_scatter) + 1]] <- data.frame(
        observed = r_pop_a[, k],
        predicted = r_sk_a[, k],
        bracket = r_labs[k],
        method = "Sinkhorn", city = city_label)
      all_scatter[[length(all_scatter) + 1]] <- data.frame(
        observed = r_pop_a[, k],
        predicted = r_vor_a[, k],
        bracket = r_labs[k],
        method = "Voronoi", city = city_label)
    }
  }
  scatter_df_v <- do.call(rbind, all_scatter)

  p <- ggplot(scatter_df_v,
    aes(x = observed, y = predicted, color = method)) +
    geom_point(alpha = 0.15, size = 0.5) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", color = "grey40") +
    scale_color_manual(values = c(
      "Sinkhorn" = "#4575b4", "Voronoi" = "#d73027")) +
    facet_grid(city ~ method, scales = "free") +
    labs(
      title = "Predicted vs Observed: Sinkhorn vs Voronoi",
      subtitle = paste0(
        "Age bracket demographics across 4 Brazilian cities"),
      x = "Census population (observed)",
      y = "Predicted population", color = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40",
                                    size = 11),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10)
    )
  save_fig(p, "voronoi-vs-sinkhorn-scatter.png",
           width = 10, height = 12)

  # Combined residual boxplot (aggregated to 7 age brackets)
  all_resid <- list()
  for (k in seq_along(age_labs)) {
    all_resid[[length(all_resid) + 1]] <- data.frame(
      residual = sk_pal_a[, k] - pop_pal_a[, k],
      bracket = age_labs[k],
      method = "Sinkhorn", city = "Palmas (TO)")
    all_resid[[length(all_resid) + 1]] <- data.frame(
      residual = vor_pal_a[, k] - pop_pal_a[, k],
      bracket = age_labs[k],
      method = "Voronoi", city = "Palmas (TO)")
  }
  for (nm in names(voronoi_results)) {
    r <- voronoi_results[[nm]]
    uf <- voronoi_cities[[which(
      sapply(voronoi_cities, function(x) x$name) == nm)]]$uf
    city_label <- sprintf("%s (%s)", nm, uf)
    r_cols <- r$calib_cols$tracts
    r_pop_a <- aggregate_to_age(r$pop_matrix, r_cols)
    r_sk_a <- aggregate_to_age(r$fitted_sinkhorn, r_cols)
    r_vor_a <- aggregate_to_age(r$fitted_voronoi, r_cols)
    r_labs <- colnames(r_pop_a)
    for (k in seq_along(r_labs)) {
      all_resid[[length(all_resid) + 1]] <- data.frame(
        residual = r_sk_a[, k] - r_pop_a[, k],
        bracket = r_labs[k],
        method = "Sinkhorn", city = city_label)
      all_resid[[length(all_resid) + 1]] <- data.frame(
        residual = r_vor_a[, k] - r_pop_a[, k],
        bracket = r_labs[k],
        method = "Voronoi", city = city_label)
    }
  }
  resid_df_v <- do.call(rbind, all_resid)

  p <- ggplot(resid_df_v,
    aes(x = bracket, y = residual, fill = method)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50") +
    scale_fill_manual(values = c(
      "Sinkhorn" = "#4575b4", "Voronoi" = "#d73027")) +
    facet_wrap(~city, ncol = 2, scales = "free_y") +
    labs(
      title = "Calibration Residuals: Sinkhorn vs Voronoi",
      subtitle = paste0(
        "Fitted minus census, by age bracket and city"),
      x = "Age bracket", y = "Residual", fill = ""
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40",
                                    size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1,
                                  size = 8),
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 11)
    )
  save_fig(p, "voronoi-vs-sinkhorn-residuals.png",
           width = 12, height = 10)

  message("  [Multi-city Voronoi] Done.")
}, error = function(e) {
  message("  [Multi-city Voronoi] FAILED: ",
          conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 13. LULA VOTES vs HOUSEHOLD HEAD INCOME (censobr)
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("LULA VOTES vs INCOME — correlation (censobr)")
message(strrep("=", 70))

tryCatch({
  income_cities <- list(
    list(name = "S\u00e3o Paulo", ibge = "3550308", uf = "SP",
         gpu = TRUE,  sk = 5L),
    list(name = "Salvador",      ibge = "2927408", uf = "BA",
         gpu = TRUE,  sk = 15L),
    list(name = "Porto Alegre",  ibge = "4314902", uf = "RS",
         gpu = FALSE, sk = 15L),
    list(name = "Cuiab\u00e1",   ibge = "5103403", uf = "MT",
         gpu = FALSE, sk = 15L)
  )

  income_results <- list()

  for (city in income_cities) {
    message(sprintf("\n  [%s] Running pipeline (K=%d)...",
                    city$name, city$sk))

    result_inc <- tryCatch({
      interpolate_election_br(
        city$name, uf = city$uf, year = 2022,
        cargo = "presidente",
        what = c("candidates", "turnout"),
        keep = c("weights", "sources_sf"),
        use_gpu = city$gpu,
        sk_iter = city$sk,
        force = TRUE
      )
    }, error = function(e) {
      message(sprintf("    FAILED: %s", conditionMessage(e)))
      NULL
    })

    if (is.null(result_inc)) next

    # Extract % Lula
    interp_inc <- result_inc$interpolated
    interp_cols_inc <- result_inc$interp_cols
    cand_13_inc <- grep("^CAND_13$", interp_cols_inc, value = TRUE)

    if (length(cand_13_inc) == 0) {
      message(sprintf("    [%s] No CAND_13 column", city$name))
      next
    }

    pct_lula_inc <- interp_inc[, cand_13_inc] /
      interp_inc[, "QT_COMPARECIMENTO"] * 100
    tract_codes_inc <- as.character(result_inc$tracts_sf$code_tract)

    # Download income data
    message(sprintf("  [%s] Downloading censobr income...", city$name))
    renda <- censobr::read_tracts(2022, "ResponsavelRenda") |>
      dplyr::filter(as.character(code_muni) == city$ibge) |>
      dplyr::select(code_tract, renda = V06004) |>
      dplyr::collect() |>
      dplyr::mutate(
        code_tract = as.character(code_tract),
        renda = as.numeric(stringr::str_replace_all(
          as.character(renda), ",", ".")))

    message(sprintf("  [%s] Income data: %d tracts",
                    city$name, nrow(renda)))

    df <- data.frame(code_tract = tract_codes_inc,
                     pct_lula = pct_lula_inc,
                     stringsAsFactors = FALSE)
    df <- merge(df, renda, by = "code_tract", all.x = TRUE)
    df <- df[!is.na(df$renda) & df$renda > 0, ]
    df$log_income <- log(df$renda + 1)
    df$city <- sprintf("%s (%s)", city$name, city$uf)

    r_val <- cor(df$log_income, df$pct_lula, use = "complete.obs")
    message(sprintf("  [%s] Matched: %d tracts, r = %.3f",
                    city$name, nrow(df), r_val))

    income_results[[city$name]] <- df
  }

  if (length(income_results) > 0) {
    all_income <- do.call(rbind, income_results)

    # Compute r per city for panel labels
    r_by_city <- all_income |>
      group_by(city) |>
      summarize(
        r = cor(log_income, pct_lula, use = "complete.obs"),
        n = n(), .groups = "drop"
      ) |>
      mutate(label = sprintf("r = %.3f (n = %d)", r, n))

    p <- ggplot(all_income, aes(x = log_income, y = pct_lula)) +
      geom_point(alpha = 0.3, color = "#4575b4", size = 1) +
      geom_smooth(method = "lm", se = TRUE,
                  color = "#d73027", linewidth = 0.8) +
      geom_text(data = r_by_city,
                aes(x = -Inf, y = Inf, label = label),
                hjust = -0.1, vjust = 1.5, size = 4,
                color = "grey30", inherit.aes = FALSE) +
      facet_wrap(~city, ncol = 2, scales = "free") +
      labs(
        title = "Lula Vote Share vs Household Head Income",
        subtitle = paste0(
          "2022 presidential election \u2014 ",
          "ecological correlation at census tract level"),
        x = "log(Average monthly income + 1)",
        y = "% votes for Lula (PT)",
        caption = paste0(
          "Income: IBGE Census 2022 ",
          "(ResponsavelRenda via censobr). ",
          "Votes: interpolated from TSE data ",
          "(Sinkhorn IDW interpolation).")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey40", size = 11),
        plot.caption = element_text(color = "grey50",
                                     size = 8, hjust = 0),
        strip.text = element_text(face = "bold", size = 12)
      )
    save_fig(p, "lula-income-correlation.png",
             width = 12, height = 10)
  }
  message("  [Income Correlation] Done.")
}, error = function(e) {
  message("  [Income Correlation] FAILED: ",
          conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# Done
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("All pre-computation complete!")
message("Data:    ", normalizePath(out_data))
message("Figures: ", normalizePath(out_fig))
message(strrep("=", 70))

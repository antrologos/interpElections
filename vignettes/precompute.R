# ── precompute.R ──────────────────────────────────────────────────────────────
#
# Generates all pre-computed artifacts (RDS data + PNG figures) required by
# the interpElections vignettes. Run this script locally BEFORE deploying
# the pkgdown site. It is NOT executed during pkgdown::build_site().
#
# Requirements: torch, r5r, Java 21+, osmium-tool, sf, ggplot2, terra,
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

# 3.2: Population pyramid
pop_brackets_vga <- data.frame(
  bracket = c("18-20", "21-24", "25-29", "30-39", "40-49", "50-59", "60-69"),
  total = colSums(pop_matrix_vga)
)
pop_brackets_vga$bracket <- factor(pop_brackets_vga$bracket,
                                    levels = rev(pop_brackets_vga$bracket))
p <- ggplot(pop_brackets_vga, aes(x = total, y = bracket)) +
  geom_col(fill = "#4575b4", alpha = 0.85) +
  labs(title = "Census Population by Age Bracket",
       subtitle = sprintf("Varginha (MG) — %d tracts, total pop: %s",
                           nrow(pop_matrix_vga),
                           format(sum(pop_matrix_vga), big.mark = ",")),
       x = "Population", y = "Age bracket") +
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

# 3.5: Age pyramids comparison (census vs electoral)
elec_totals <- colSums(source_matrix_vga)
pyramid_df <- data.frame(
  bracket = rep(pop_brackets_vga$bracket, 2),
  count = c(pop_brackets_vga$total, elec_totals),
  source = rep(c("Census (population)", "TSE (voters)"),
               each = nrow(pop_brackets_vga))
)
p <- ggplot(pyramid_df, aes(x = count, y = bracket, fill = source)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("#4575b4", "#d73027")) +
  labs(title = "Census Population vs. Registered Voters by Age",
       subtitle = "Varginha (MG) — 2010 Census vs. 2022 Election",
       x = "Count", y = "Age bracket", fill = "") +
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
         x = "ADAM step", y = "Objective (SSE, log scale)") +
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
fitted_demo <- W_vga %*% source_matrix_vga
bracket1 <- calib_cols_vga$tracts[1]
scatter_df <- data.frame(
  census = pop_matrix_vga[, 1],
  interpolated = fitted_demo[, 1]
)
r2 <- cor(scatter_df$census, scatter_df$interpolated)^2
p <- ggplot(scatter_df, aes(x = census, y = interpolated)) +
  geom_point(alpha = 0.6, color = "#4575b4") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Interpolated vs. Census Demographics",
       subtitle = sprintf("Varginha — %s (R² = %.4f)", bracket1, r2),
       x = "Census population", y = "Interpolated value") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
save_fig(p, "interp-scatter-calib.png", width = 6, height = 6)

# 3.15: Residuals boxplot
resid_vga <- fitted_demo - pop_matrix_vga
resid_long <- tidyr::pivot_longer(
  as.data.frame(resid_vga),
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

# Residuals boxplot
resid_nit <- residuals(result_nit)
if (!is.null(resid_nit)) {
  resid_long_nit <- tidyr::pivot_longer(
    as.data.frame(resid_nit),
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

  # --- Figures ---
  message("[SP] Generating figures...")

  # Transform all to WGS84 for consistent plotting
  ts_wgs <- st_transform(tracts_south, 4326)
  pc_wgs <- st_transform(pts_sp_c, 4326)
  pp_wgs <- st_transform(pts_sp_p, 4326)
  pw_wgs <- st_transform(pts_sp_w, 4326)

  # 3-panel comparison map
  pts_all <- rbind(
    cbind(pc_wgs[, "id"], method = "Centroid"),
    cbind(pp_wgs[, "id"], method = "Point on Surface"),
    cbind(pw_wgs[, "id"], method = "Pop-Weighted")
  )

  p <- ggplot() +
    geom_sf(data = ts_wgs, fill = "grey95",
            color = "grey60", linewidth = 0.2) +
    geom_sf(data = pts_all, aes(color = method),
            size = 1.5, alpha = 0.8) +
    scale_color_manual(values = c(
      "Centroid" = "#d73027",
      "Point on Surface" = "#4575b4",
      "Pop-Weighted" = "#1a9850"
    )) +
    facet_wrap(~method, ncol = 3) +
    labs(
      title = "Representative Point Methods Compared",
      subtitle = paste0(
        "South S\u00e3o Paulo \u2014 ",
        nrow(tracts_south), " tracts"
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40"),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11)
    )
  save_fig(p, "rep-points-saopaulo-comparison.png",
           width = 14, height = 6)

  # Zoom inset: pick the largest tract
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

  # Try to add raster background
  p_zoom <- ggplot() +
    geom_sf(data = zoom_tract, fill = "grey90",
            color = "grey40", linewidth = 0.4)

  if (!is.null(pop_raster_sp)) {
    rast_crop <- tryCatch({
      zoom_ext <- ext(
        zoom_bb["xmin"], zoom_bb["xmax"],
        zoom_bb["ymin"], zoom_bb["ymax"]
      )
      rc <- crop(
        project(pop_raster_sp, "EPSG:4326"),
        zoom_ext
      )
      rc_df <- as.data.frame(rc, xy = TRUE)
      names(rc_df)[3] <- "pop"
      rc_df
    }, error = function(e) NULL)
    if (!is.null(rast_crop) && nrow(rast_crop) > 0) {
      p_zoom <- p_zoom +
        geom_raster(
          data = rast_crop,
          aes(x = x, y = y, fill = pop), alpha = 0.7
        ) +
        scale_fill_viridis_c(
          option = "inferno", name = "Population\ndensity",
          na.value = "transparent"
        )
    }
  }

  p_zoom <- p_zoom +
    geom_sf(data = zoom_tract, fill = NA,
            color = "grey30", linewidth = 0.5) +
    geom_sf(data = pts_zoom_sf,
            aes(color = method), size = 4, shape = 17) +
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
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40"),
      legend.position = "bottom"
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

  # Scatter: predicted vs observed, both methods
  brackets <- calib_pal$tracts
  scatter_list <- list()
  for (k in seq_along(brackets)) {
    scatter_list[[length(scatter_list) + 1]] <- data.frame(
      observed = pop_mat_pal[, k],
      predicted = fitted_pal[, k],
      bracket = brackets[k],
      method = "Sinkhorn IDW"
    )
    scatter_list[[length(scatter_list) + 1]] <- data.frame(
      observed = pop_mat_pal[, k],
      predicted = voronoi_fitted[, k],
      bracket = brackets[k],
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

  # Residual boxplot by bracket
  resid_list <- list()
  for (k in seq_along(brackets)) {
    resid_list[[length(resid_list) + 1]] <- data.frame(
      residual = fitted_pal[, k] - pop_mat_pal[, k],
      bracket = brackets[k],
      method = "Sinkhorn IDW"
    )
    resid_list[[length(resid_list) + 1]] <- data.frame(
      residual = voronoi_fitted[, k] - pop_mat_pal[, k],
      bracket = brackets[k],
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
  # ── 7a. Census age pyramids: Rocinha vs Copacabana ──────────────

  message("\n[RJ] Loading census population data...")
  pop_rj <- br_prepare_population(3304557, year = 2010)

  message("[RJ] Loading tract geometries...")
  tracts_rj <- br_prepare_tracts(3304557, pop_rj, verbose = FALSE)

  message("[RJ] Identifying neighborhoods...")
  # Download neighborhood boundaries
  nbhoods_rj <- geobr::read_neighborhood(year = 2010)
  nbhoods_rj <- nbhoods_rj[nbhoods_rj$code_muni == 3304557, ]
  nbhoods_rj <- st_transform(nbhoods_rj, st_crs(tracts_rj))

  # Find Rocinha and Copacabana neighborhood polygons
  rocinha_nb <- nbhoods_rj[
    grepl("Rocinha", nbhoods_rj$name_neighborhood,
          ignore.case = TRUE), ]
  copacabana_nb <- nbhoods_rj[
    grepl("Copacabana", nbhoods_rj$name_neighborhood,
          ignore.case = TRUE), ]

  if (nrow(rocinha_nb) > 0 && nrow(copacabana_nb) > 0) {
    message(sprintf(
      "  Found: Rocinha (%d polygons), Copacabana (%d polygons)",
      nrow(rocinha_nb), nrow(copacabana_nb)))

    # Spatial join: which tracts fall in each neighborhood
    tracts_rj_pts <- st_point_on_surface(tracts_rj)
    rocinha_tracts <- tracts_rj[
      lengths(st_intersects(tracts_rj_pts, rocinha_nb)) > 0, ]
    copacabana_tracts <- tracts_rj[
      lengths(st_intersects(tracts_rj_pts, copacabana_nb)) > 0, ]

    message(sprintf(
      "  Tracts: Rocinha=%d, Copacabana=%d",
      nrow(rocinha_tracts), nrow(copacabana_tracts)))

    # Age brackets (matching calibration brackets)
    age_cols <- grep("^pop_\\d", names(tracts_rj), value = TRUE)
    # Filter to calibration brackets only (18+)
    age_cols_18 <- age_cols[!grepl(
      "pop_00|pop_05|pop_10|pop_70", age_cols)]

    if (length(age_cols_18) > 0) {
      roc_df <- st_drop_geometry(rocinha_tracts)
      cop_df <- st_drop_geometry(copacabana_tracts)

      roc_totals <- colSums(roc_df[, age_cols_18, drop = FALSE])
      cop_totals <- colSums(cop_df[, age_cols_18, drop = FALSE])

      # Clean bracket labels
      bracket_labels <- gsub("pop_", "", age_cols_18)
      bracket_labels <- gsub("_", "-", bracket_labels)

      pyramid_rj <- data.frame(
        bracket = rep(bracket_labels, 2),
        population = c(roc_totals, cop_totals),
        neighborhood = rep(
          c("Rocinha (favela)", "Copacabana (wealthy)"),
          each = length(age_cols_18)
        )
      )
      # Normalize to percentages for comparison
      pyramid_rj <- pyramid_rj |>
        group_by(neighborhood) |>
        mutate(pct = population / sum(population) * 100) |>
        ungroup()
      pyramid_rj$bracket <- factor(
        pyramid_rj$bracket,
        levels = rev(unique(pyramid_rj$bracket))
      )

      p <- ggplot(pyramid_rj,
                  aes(x = pct, y = bracket,
                      fill = neighborhood)) +
        geom_col(position = "dodge", alpha = 0.85) +
        scale_fill_manual(values = c(
          "Rocinha (favela)" = "#d73027",
          "Copacabana (wealthy)" = "#4575b4"
        )) +
        labs(
          title = "Age Structure: Poor vs Wealthy Neighborhood",
          subtitle = paste0(
            "Rio de Janeiro \u2014 2010 Census ",
            "(voting-age brackets)"
          ),
          x = "% of neighborhood population",
          y = "Age bracket", fill = ""
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          legend.position = "bottom"
        )
      save_fig(p, "age-pyramids-rocinha-copacabana.png",
               width = 8, height = 5)
    }
  } else {
    message("  WARNING: Could not find Rocinha/Copacabana")
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
  # (result tracts may have different ordering/subsetting)
  tracts_rj_pts2 <- st_point_on_surface(tracts_rj_result)
  if (exists("rocinha_nb") && nrow(rocinha_nb) > 0) {
    roc_mask <- lengths(
      st_intersects(tracts_rj_pts2, rocinha_nb)) > 0
    cop_mask <- lengths(
      st_intersects(tracts_rj_pts2, copacabana_nb)) > 0

    roc_result_df <- st_drop_geometry(
      tracts_rj_result[roc_mask, ])
    cop_result_df <- st_drop_geometry(
      tracts_rj_result[cop_mask, ])

    # Census values for these tracts
    calib_tract_cols <- calib_rj$tracts
    roc_census <- colSums(
      roc_result_df[, calib_tract_cols, drop = FALSE])
    cop_census <- colSums(
      cop_result_df[, calib_tract_cols, drop = FALSE])

    # Interpolated (Sinkhorn) values
    roc_interp <- colSums(fitted_rj[roc_mask, , drop = FALSE])
    cop_interp <- colSums(fitted_rj[cop_mask, , drop = FALSE])

    bracket_labels_rj <- gsub("pop_", "", calib_tract_cols)
    bracket_labels_rj <- gsub("_", "-", bracket_labels_rj)

    recovery_df <- data.frame(
      bracket = rep(bracket_labels_rj, 4),
      value = c(roc_census, roc_interp,
                cop_census, cop_interp),
      source = rep(c("Census", "Interpolated",
                      "Census", "Interpolated"),
                    each = length(calib_tract_cols)),
      neighborhood = rep(
        c("Rocinha", "Rocinha",
          "Copacabana", "Copacabana"),
        each = length(calib_tract_cols)
      )
    )
    recovery_df$bracket <- factor(
      recovery_df$bracket,
      levels = rev(unique(recovery_df$bracket))
    )

    p <- ggplot(recovery_df,
                aes(x = value, y = bracket,
                    fill = source)) +
      geom_col(position = "dodge", alpha = 0.85) +
      scale_fill_manual(values = c(
        "Census" = "#4575b4",
        "Interpolated" = "#fdae61"
      )) +
      facet_wrap(~neighborhood, scales = "free_x") +
      labs(
        title = "Age Structure Recovery",
        subtitle = paste0(
          "Census truth vs Sinkhorn-interpolated \u2014 ",
          "Rio de Janeiro 2022"
        ),
        x = "Population / Voters",
        y = "Age bracket", fill = ""
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    save_fig(p, "age-recovery-rocinha-copacabana.png",
             width = 10, height = 6)
  }

  # ── 7c. Route visualization: Santa Marta / Botafogo ────────────

  message("\n[RJ] Computing route for Santa Marta...")
  routes_rj <- tryCatch({
    if (!requireNamespace("r5r", quietly = TRUE)) {
      message("  r5r not available, skipping routes")
      NULL
    } else {
      cache_dir <- get_interpElections_cache_dir()
      # r5r networks are stored under networks/r5r/<bbox_hash>/
      r5r_base <- file.path(cache_dir, "networks", "r5r")
      pbf_files <- list.files(
        r5r_base, pattern = "\\.pbf$",
        full.names = TRUE, recursive = TRUE)
      # Also check downloads/osm/ for state-level extracts
      osm_base <- file.path(cache_dir, "downloads", "osm")
      pbf_files <- c(pbf_files, list.files(
        osm_base, pattern = "\\.pbf$",
        full.names = TRUE, recursive = TRUE))

      # Find RJ network (clipped PBFs or state-level rio extract)
      rj_pbf <- pbf_files[grepl(
        "rio|RJ", pbf_files, ignore.case = TRUE)]
      if (length(rj_pbf) == 0) rj_pbf <- pbf_files

      if (length(rj_pbf) > 0) {
        network_dir <- dirname(rj_pbf[1])
        r5r_core <- r5r::setup_r5(
          network_dir, verbose = FALSE)
        on.exit(r5r::stop_r5(r5r_core), add = TRUE)

        # Santa Marta tract area -> polling station
        # Use a tract centroid near Santa Marta
        origin <- data.frame(
          id = "santa_marta",
          lon = -43.1946, lat = -22.9538
        )
        # Two nearby stations: Botafogo + Laranjeiras
        dests <- data.frame(
          id = c("botafogo", "laranjeiras"),
          lon = c(-43.1784, -43.1878),
          lat = c(-22.9507, -22.9370)
        )

        route_lines <- r5r::detailed_itineraries(
          r5r_core,
          origins = origin,
          destinations = dests,
          mode = "WALK",
          max_trip_duration = 120L,
          shortest_path = TRUE
        )
        route_lines
      } else {
        message("  No RJ PBF found")
        NULL
      }
    }
  }, error = function(e) {
    message("  Route computation failed: ",
            conditionMessage(e))
    NULL
  })

  # Generate route figure with OSM basemap
  if (!is.null(routes_rj) && nrow(routes_rj) > 0) {
    message("[RJ] Generating route figure...")
    route_sf <- st_transform(routes_rj, 4326)

    # Get OSM basemap tiles
    bb_route <- st_bbox(route_sf)
    pad_x <- 0.15 * (bb_route["xmax"] - bb_route["xmin"])
    pad_y <- 0.15 * (bb_route["ymax"] - bb_route["ymin"])
    bb_route["xmin"] <- bb_route["xmin"] - pad_x
    bb_route["xmax"] <- bb_route["xmax"] + pad_x
    bb_route["ymin"] <- bb_route["ymin"] - pad_y
    bb_route["ymax"] <- bb_route["ymax"] + pad_y
    bb_sfc <- st_as_sfc(bb_route)

    basemap_rj <- tryCatch({
      tiles <- get_tiles(
        x = bb_sfc, provider = "OpenStreetMap",
        crop = TRUE, zoom = 15)
      st_as_stars(tiles)
    }, error = function(e) {
      message("  Basemap download failed: ",
              conditionMessage(e))
      NULL
    })

    # Origin and destination points for annotation
    origin_pt <- st_as_sf(
      data.frame(lon = -43.1946, lat = -22.9538,
                 label = "Santa Marta"),
      coords = c("lon", "lat"), crs = 4326
    )
    dest_pts <- st_as_sf(
      data.frame(
        lon = c(-43.1784, -43.1878),
        lat = c(-22.9507, -22.9370),
        label = c("Botafogo station",
                   "Laranjeiras station")
      ),
      coords = c("lon", "lat"), crs = 4326
    )

    p_route <- ggplot()
    if (!is.null(basemap_rj)) {
      p_route <- p_route +
        layer_spatial(basemap_rj, dpi = 300)
    }
    p_route <- p_route +
      geom_sf(data = route_sf, color = "#002D72",
              linewidth = 2, alpha = 0.8) +
      geom_sf(data = origin_pt, color = "#d73027",
              size = 4, shape = 16) +
      geom_sf(data = dest_pts, color = "#1a9850",
              size = 4, shape = 17) +
      # Straight-line distance
      annotate("segment",
               x = -43.1946, y = -22.9538,
               xend = -43.1784, yend = -22.9507,
               linetype = "dotted",
               color = "red", linewidth = 0.8) +
      coord_sf(
        xlim = c(bb_route["xmin"], bb_route["xmax"]),
        ylim = c(bb_route["ymin"], bb_route["ymax"])
      ) +
      labs(
        title = "Walking Route: Santa Marta to Botafogo",
        subtitle = paste0(
          "Rio de Janeiro \u2014 dotted line = ",
          "euclidean distance"
        )
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey40")
      )
    save_fig(p_route, "routes-rj-santa-marta.png",
             width = 9, height = 8)
  }

  # Save RDS with everything
  saveRDS(list(
    pop_data = pop_rj,
    tracts_rj = tracts_rj,
    result = result_rj,
    fitted_demographics = fitted_rj,
    routes = routes_rj,
    rocinha_tracts = if (exists("rocinha_tracts"))
      rocinha_tracts else NULL,
    copacabana_tracts = if (exists("copacabana_tracts"))
      copacabana_tracts else NULL,
    neighborhoods = nbhoods_rj
  ), file.path(out_data, "rio_methodology.rds"))

  message("  [RJ] Done.")
}, error = function(e) {
  message("  [RJ] FAILED: ", conditionMessage(e))
})


# ═════════════════════════════════════════════════════════════════════
# 8. RECIFE (PE) — River barrier route example
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("RECIFE (PE) — River barrier route")
message(strrep("=", 70))

tryCatch({
  message("\n[Recife] Computing cross-river route...")

  routes_rec <- tryCatch({
    if (!requireNamespace("r5r", quietly = TRUE)) {
      message("  r5r not available, skipping")
      NULL
    } else {
      # Download Recife OSM data if needed
      # Try to use interpElections cache or download
      cache_dir <- get_interpElections_cache_dir()

      # We need the r5r network for Recife
      # Check networks/r5r/ and downloads/osm/ for PBF files
      r5r_base <- file.path(cache_dir, "networks", "r5r")
      osm_base <- file.path(cache_dir, "downloads", "osm")
      pbf_files <- c(
        list.files(r5r_base, pattern = "\\.pbf$",
                   full.names = TRUE, recursive = TRUE),
        list.files(osm_base, pattern = "\\.pbf$",
                   full.names = TRUE, recursive = TRUE)
      )
      rec_pbf <- pbf_files[grepl(
        "recife|pernambuco|PE", pbf_files, ignore.case = TRUE)]

      if (length(rec_pbf) == 0) {
        # Download OSM data for Recife via the package
        message("  Downloading Recife OSM data...")
        # Get tracts to define bounding box
        pop_rec <- br_prepare_population(2611606, year = 2010)
        tracts_rec <- br_prepare_tracts(
          2611606, pop_rec, verbose = FALSE)

        # Create r5r output directory in cache
        rec_r5r_dir <- file.path(
          cache_dir, "networks", "r5r", "recife_routes")
        dir.create(rec_r5r_dir, recursive = TRUE,
                   showWarnings = FALSE)

        # Download r5r data using area_sf + output_dir
        r5r_result <- download_r5r_data(
          area_sf = tracts_rec,
          output_dir = rec_r5r_dir,
          verbose = TRUE
        )
        rec_pbf <- list.files(
          rec_r5r_dir, pattern = "\\.pbf$",
          full.names = TRUE)
      }

      if (length(rec_pbf) > 0) {
        network_dir <- dirname(rec_pbf[1])
        r5r_core <- r5r::setup_r5(
          network_dir, verbose = FALSE)
        on.exit(r5r::stop_r5(r5r_core), add = TRUE)

        # Points on opposite sides of Capibaribe river
        # South bank: Boa Vista
        # North bank: Espinheiro
        origin <- data.frame(
          id = "boa_vista",
          lon = -34.8811, lat = -8.0589
        )
        dest <- data.frame(
          id = "espinheiro",
          lon = -34.8895, lat = -8.0435
        )

        route_lines <- r5r::detailed_itineraries(
          r5r_core,
          origins = origin,
          destinations = dest,
          mode = "WALK",
          max_trip_duration = 120L,
          shortest_path = TRUE
        )
        route_lines
      } else {
        message("  No Recife PBF found")
        NULL
      }
    }
  }, error = function(e) {
    message("  Route computation failed: ",
            conditionMessage(e))
    NULL
  })

  if (!is.null(routes_rec) && nrow(routes_rec) > 0) {
    message("[Recife] Generating route figure...")
    route_sf_rec <- st_transform(routes_rec, 4326)

    bb_rec <- st_bbox(route_sf_rec)
    pad_x <- 0.2 * (bb_rec["xmax"] - bb_rec["xmin"])
    pad_y <- 0.2 * (bb_rec["ymax"] - bb_rec["ymin"])
    bb_rec["xmin"] <- bb_rec["xmin"] - pad_x
    bb_rec["xmax"] <- bb_rec["xmax"] + pad_x
    bb_rec["ymin"] <- bb_rec["ymin"] - pad_y
    bb_rec["ymax"] <- bb_rec["ymax"] + pad_y
    bb_sfc_rec <- st_as_sfc(bb_rec)

    basemap_rec <- tryCatch({
      tiles <- get_tiles(
        x = bb_sfc_rec, provider = "OpenStreetMap",
        crop = TRUE, zoom = 15)
      st_as_stars(tiles)
    }, error = function(e) {
      message("  Basemap download failed: ",
              conditionMessage(e))
      NULL
    })

    origin_pt_rec <- st_as_sf(
      data.frame(lon = -34.8811, lat = -8.0589,
                 label = "Boa Vista (south bank)"),
      coords = c("lon", "lat"), crs = 4326
    )
    dest_pt_rec <- st_as_sf(
      data.frame(lon = -34.8895, lat = -8.0435,
                 label = "Espinheiro (north bank)"),
      coords = c("lon", "lat"), crs = 4326
    )

    p_rec <- ggplot()
    if (!is.null(basemap_rec)) {
      p_rec <- p_rec +
        layer_spatial(basemap_rec, dpi = 300)
    }
    p_rec <- p_rec +
      geom_sf(data = route_sf_rec, color = "#002D72",
              linewidth = 2, alpha = 0.8) +
      geom_sf(data = origin_pt_rec, color = "#d73027",
              size = 4, shape = 16) +
      geom_sf(data = dest_pt_rec, color = "#1a9850",
              size = 4, shape = 17) +
      annotate("segment",
               x = -34.8811, y = -8.0589,
               xend = -34.8895, yend = -8.0435,
               linetype = "dotted",
               color = "red", linewidth = 0.8) +
      coord_sf(
        xlim = c(bb_rec["xmin"], bb_rec["xmax"]),
        ylim = c(bb_rec["ymin"], bb_rec["ymax"])
      ) +
      labs(
        title = "Walking Route: Crossing the Capibaribe",
        subtitle = paste0(
          "Recife (PE) \u2014 bridge detour vs ",
          "euclidean distance (dotted)"
        )
      ) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "grey40")
      )
    save_fig(p_rec, "routes-recife-river.png",
             width = 9, height = 8)
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

  # Use Varginha data already in memory
  # Transform all layers to a projected CRS for shearing
  vga_tracts <- st_transform(tracts_sf_vga, 4326)
  vga_stations <- st_transform(electoral_sf_vga, 4326)

  # Affine shear/rotation function (from user's example)
  rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
    shear_m <- matrix(c(2, 1.2, 0, 1), 2, 2)
    rot_angle <- pi / 20
    rot_m <- matrix(c(cos(rot_angle), sin(rot_angle),
                       -sin(rot_angle), cos(rot_angle)),
                     2, 2)
    geom <- st_geometry(data)
    new_geom <- (geom * shear_m * rot_m) + c(x_add, y_add)
    st_set_geometry(data, new_geom)
  }

  # Compute interpolated pct for top layer
  if (length(cand_13_col) > 0) {
    vga_tracts$pct_lula <- interpolated_vga[, cand_13_col] /
      interpolated_vga[, qt_comp_col] * 100
  } else {
    vga_tracts$pct_lula <- 50
  }
  vga_tracts$pop_t <- pop_total_vga

  # Layer offsets (vertical stacking)
  y_step <- 0.06

  # Build the stacked figure
  p_pipe <- ggplot() +
    # Layer 1 (bottom): Municipality boundary
    geom_sf(
      data = rotate_data_geom(
        st_sf(geometry = st_sfc(st_union(st_geometry(vga_tracts)),
                                crs = st_crs(vga_tracts))),
        y_add = 0),
      fill = "#FCDE70", color = "grey30",
      linewidth = 0.3) +

    # Layer 2: Census tracts colored by population
    geom_sf(
      data = rotate_data_geom(vga_tracts, y_add = y_step),
      aes(fill = pop_t),
      color = "white", linewidth = 0.05) +
    scale_fill_viridis_c(
      option = "viridis", direction = -1,
      name = "Population", guide = "none") +

    # Layer 3: Tracts outline + polling stations
    geom_sf(
      data = rotate_data_geom(
        st_sf(geometry = st_geometry(vga_tracts)),
        y_add = 2 * y_step),
      fill = "grey95", color = "grey60",
      linewidth = 0.1) +
    geom_sf(
      data = rotate_data_geom(
        vga_stations, y_add = 2 * y_step),
      color = "#d73027", size = 1.5, alpha = 0.8) +

    # Layer 4 (top): Interpolated results
    geom_sf(
      data = rotate_data_geom(vga_tracts, y_add = 3 * y_step),
      aes(color = pct_lula),
      fill = NA, linewidth = 0.3) +
    scale_color_distiller(
      palette = "RdYlBu", direction = -1,
      name = "Lula %", guide = "none") +

    theme_void() +
    theme(
      plot.background = element_rect(
        fill = "white", color = "white"),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey40")
    ) +
    labs(
      title = paste0(
        "Pipeline: From Spatial Data to ",
        "Tract-Level Estimates"
      ),
      subtitle = paste0(
        "Census tracts \u2192 Polling stations \u2192 ",
        "Travel times \u2192 Interpolated results"
      )
    )

  save_fig(p_pipe, "pipeline-stacked-maps.png",
           width = 10, height = 12, dpi = 250)

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
      "6. Adjust alpha\n(ADAM gradient)"
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
# Done
# ═════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("All pre-computation complete!")
message("Data:    ", normalizePath(out_data))
message("Figures: ", normalizePath(out_fig))
message(strrep("=", 70))

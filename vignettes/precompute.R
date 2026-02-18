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


# ═══════════════════════════════════════════════════════════════════════════════
# Done
# ═══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 70))
message("All pre-computation complete!")
message("Data:    ", normalizePath(out_data))
message("Figures: ", normalizePath(out_fig))
message(strrep("=", 70))

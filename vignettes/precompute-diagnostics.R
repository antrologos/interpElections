# ── precompute-diagnostics.R ──────────────────────────────────────────────────
#
# Generates all pre-computed artifacts for the "Diagnostics and Validation"
# vignette. Re-runs all 4 municipalities with the current package version,
# then generates ~60 PNG figures + ~16 text outputs.
#
# Requirements: interpElections, torch, r5r, Java 21, sf, ggplot2, spdep,
#   censobr (for plot_income), network access.
#
# Usage (from package root):
#   Rscript vignettes/precompute-diagnostics.R
#
# Output:
#   vignettes/precomputed/*.rds         — result objects
#   vignettes/figures/diag-*.png        — plot figures
#   vignettes/figures/diag-*.txt        — text outputs
# ─────────────────────────────────────────────────────────────────────────────

devtools::load_all(".")
library(ggplot2)
library(sf)

sf::sf_use_s2(FALSE)

out_data <- "vignettes/precomputed"
out_fig  <- "vignettes/figures"
dir.create(out_data, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig,  recursive = TRUE, showWarnings = FALSE)

# Helper: save ggplot as PNG
save_fig <- function(p, filename, width = 8, height = 6, dpi = 200) {
  path <- file.path(out_fig, filename)
  ggsave(path, plot = p, width = width, height = height, dpi = dpi,
         bg = "white")
  message("  Saved: ", path)
}

# Helper: save text output
save_txt <- function(expr, filename) {
  path <- file.path(out_fig, filename)
  out <- capture.output(expr)
  writeLines(out, path)
  message("  Saved: ", path)
}


# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: Re-run interpolations
# ═══════════════════════════════════════════════════════════════════════════════

message("\n===== Running Igrejinha 2022 =====")
result_igr <- interpolate_election_br(
  "Igrejinha", year = 2022, cargo = "presidente",
  what = c("candidates", "turnout"),
  keep = "electoral_sf"
)
saveRDS(result_igr, file.path(out_data, "igrejinha_2022.rds"))
message("  Saved: igrejinha_2022.rds")

message("\n===== Running Varginha 2022 (turno 1) =====")
result_vga <- interpolate_election_br(
  "Varginha", year = 2022, cargo = "presidente",
  what = c("candidates", "turnout"),
  keep = "electoral_sf"
)
saveRDS(result_vga, file.path(out_data, "varginha_2022.rds"))
message("  Saved: varginha_2022.rds")

message("\n===== Running Varginha 2022 (turno 2) =====")
result_vga2 <- interpolate_election_br(
  "Varginha", year = 2022, cargo = "presidente",
  turno = 2L,
  what = c("candidates", "turnout"),
  keep = "electoral_sf"
)
saveRDS(result_vga2, file.path(out_data, "varginha_2022_t2.rds"))
message("  Saved: varginha_2022_t2.rds")

message("\n===== Running Niteroi 2022 =====")
result_nit <- interpolate_election_br(
  "Niteroi", year = 2022, cargo = "presidente",
  what = c("candidates", "parties", "turnout", "demographics"),
  keep = "electoral_sf"
)
saveRDS(result_nit, file.path(out_data, "niteroi_2022.rds"))
message("  Saved: niteroi_2022.rds")

message("\n===== Running Belo Horizonte 2022 =====")
result_bh <- interpolate_election_br(
  "Belo Horizonte", year = 2022, cargo = "presidente",
  what = c("candidates", "turnout"),
  keep = "electoral_sf",
  optim = optim_control(use_gpu = TRUE)
)
saveRDS(result_bh, file.path(out_data, "belo_horizonte_2022.rds"))
message("  Saved: belo_horizonte_2022.rds")


# ═══════════════════════════════════════════════════════════════════════════════
# PART 2: Generate diagnostic figures
# ═══════════════════════════════════════════════════════════════════════════════

# Discover actual tract IDs for catchment/connection examples
vga_ids <- sf::st_drop_geometry(result_vga$tracts_sf)[[result_vga$tract_id]]


# ── Section 0: Setup summaries ───────────────────────────────────────────────

message("\n===== Section 0: Summaries =====")
save_txt(summary(result_igr), "diag-s0-summary-igr.txt")
save_txt(summary(result_vga), "diag-s0-summary-vga.txt")
save_txt(summary(result_nit), "diag-s0-summary-nit.txt")
save_txt(summary(result_bh),  "diag-s0-summary-bh.txt")


# ── Section 1: diagnostics() ─────────────────────────────────────────────────

message("\n===== Section 1: diagnostics() =====")
save_txt(diagnostics(result_vga), "diag-s1-checks-vga.txt")
save_txt(diagnostics(result_bh),  "diag-s1-checks-bh.txt")
# Programmatic access example
d <- diagnostics(result_vga, verbose = FALSE)
save_txt({
  cat("Convergence value:", d$checks$convergence$value, "\n")
  cat("Residual RMSE:", d$checks$residual_rmse$value, "\n")
  cat("Pop-voter gap (%):", d$checks$pop_voter_gap$value, "\n")
}, "diag-s1-checks-programmatic.txt")


# ── Section 2: plot_convergence() ────────────────────────────────────────────

message("\n===== Section 2: plot_convergence() =====")
save_fig(plot_convergence(result_vga), "diag-s2-conv-vga-all.png")
save_fig(plot_convergence(result_bh),  "diag-s2-conv-bh-all.png")
save_fig(plot_convergence(result_vga, which = "loss"),
         "diag-s2-conv-vga-loss.png")
save_fig(plot_convergence(result_vga, which = c("loss", "gradient"),
                          log_y = TRUE),
         "diag-s2-conv-vga-log.png")


# ── Section 3: plot_alpha() ──────────────────────────────────────────────────

message("\n===== Section 3: plot_alpha() =====")

# Map mode — summary_fn variations
save_fig(plot_alpha(result_vga, type = "map", summary_fn = "median"),
         "diag-s3-alpha-vga-map-median.png")
save_fig(plot_alpha(result_vga, type = "map", summary_fn = "mean"),
         "diag-s3-alpha-vga-map-mean.png")
save_fig(plot_alpha(result_vga, type = "map", summary_fn = "pop_weighted"),
         "diag-s3-alpha-vga-map-popwt.png")
save_fig(plot_alpha(result_vga, type = "map", summary_fn = "range"),
         "diag-s3-alpha-vga-map-range.png")
save_fig(plot_alpha(result_vga, type = "map", summary_fn = 1),
         "diag-s3-alpha-vga-map-bracket1.png")
save_fig(plot_alpha(result_bh, type = "map", summary_fn = "median"),
         "diag-s3-alpha-bh-map-median.png")

# Histogram mode
save_fig(plot_alpha(result_nit, type = "histogram"),
         "diag-s3-alpha-nit-hist-all.png", width = 10, height = 5)
save_fig(plot_alpha(result_nit, type = "histogram",
                    brackets = c(1, 7, 8, 14)),
         "diag-s3-alpha-nit-hist-sel.png", width = 10, height = 4)
save_fig(plot_alpha(result_igr, type = "histogram"),
         "diag-s3-alpha-igr-hist-all.png", width = 10, height = 5)

# Bracket boxplot
save_fig(plot_alpha(result_bh, type = "bracket"),
         "diag-s3-alpha-bh-bracket.png", width = 10, height = 5)
save_fig(plot_alpha(result_nit, type = "bracket", brackets = 1:7),
         "diag-s3-alpha-nit-bracket-male.png", width = 8, height = 5)


# ── Section 4: plot_residuals() + residual_summary() ─────────────────────────

message("\n===== Section 4: plot_residuals() =====")

# Map mode
save_fig(plot_residuals(result_vga, type = "map", summary_fn = "rmse"),
         "diag-s4-resid-vga-map-rmse.png")
save_fig(plot_residuals(result_vga, type = "map", summary_fn = "mean"),
         "diag-s4-resid-vga-map-mean.png")
save_fig(plot_residuals(result_vga, type = "map", residual_type = "pearson",
                        summary_fn = "rmse"),
         "diag-s4-resid-vga-map-pearson.png")
save_fig(plot_residuals(result_bh, type = "map", summary_fn = "rmse"),
         "diag-s4-resid-bh-map-rmse.png")
save_fig(plot_residuals(result_vga, type = "map", summary_fn = 1),
         "diag-s4-resid-vga-map-bracket1.png")

# Histogram mode
save_fig(plot_residuals(result_nit, type = "histogram"),
         "diag-s4-resid-nit-hist-raw.png", width = 10, height = 5)
save_fig(plot_residuals(result_nit, type = "histogram",
                        residual_type = "pearson"),
         "diag-s4-resid-nit-hist-pearson.png", width = 10, height = 5)
save_fig(plot_residuals(result_nit, type = "histogram",
                        brackets = c(1, 7, 8, 14)),
         "diag-s4-resid-nit-hist-sel.png", width = 10, height = 4)

# Bracket boxplot
save_fig(plot_residuals(result_bh, type = "bracket"),
         "diag-s4-resid-bh-bracket-raw.png", width = 10, height = 5)
save_fig(plot_residuals(result_bh, type = "bracket",
                        residual_type = "deviance"),
         "diag-s4-resid-bh-bracket-dev.png", width = 10, height = 5)

# Scatter
save_fig(plot_residuals(result_igr, type = "scatter"),
         "diag-s4-resid-igr-scatter.png", width = 10, height = 5)
save_fig(plot_residuals(result_vga, type = "scatter",
                        brackets = c(1, 14)),
         "diag-s4-resid-vga-scatter-sel.png", width = 10, height = 5)
save_fig(plot_residuals(result_nit, type = "scatter",
                        residual_type = "pearson"),
         "diag-s4-resid-nit-scatter-pear.png", width = 10, height = 5)

# Tabular
save_txt(print(residual_summary(result_vga)),
         "diag-s4-ressumm-vga-raw.txt")
save_txt(print(residual_summary(result_vga, type = "pearson")),
         "diag-s4-ressumm-vga-pearson.txt")


# ── Section 5: Weight diagnostics ────────────────────────────────────────────

message("\n===== Section 5: Weight diagnostics =====")

# weight_summary tables
save_txt({
  ws <- weight_summary(result_vga)
  print(head(ws, 15))
  cat("\n... (", nrow(ws), " rows total)\n", sep = "")
}, "diag-s5-wsumm-vga.txt")

save_txt({
  ws <- weight_summary(result_bh)
  print(head(ws, 15))
  cat("\n... (", nrow(ws), " rows total)\n", sep = "")
}, "diag-s5-wsumm-bh.txt")

# Entropy maps
save_fig(plot_weights(result_vga, type = "entropy"),
         "diag-s5-wt-vga-entropy.png")
save_fig(plot_weights(result_bh, type = "entropy"),
         "diag-s5-wt-bh-entropy.png")

# Dominant station
save_fig(plot_weights(result_vga, type = "dominant"),
         "diag-s5-wt-vga-dominant.png")
save_fig(plot_weights(result_nit, type = "dominant"),
         "diag-s5-wt-nit-dominant.png")

# Catchment
save_fig(plot_weights(result_vga, type = "catchment"),
         "diag-s5-wt-vga-catchment-auto.png")
# Pick a specific tract (middle of the city)
mid_idx <- ceiling(length(vga_ids) / 2)
save_fig(plot_weights(result_vga, type = "catchment", tract = mid_idx),
         "diag-s5-wt-vga-catchment-mid.png")
save_fig(plot_weights(result_vga, type = "catchment", tract = 1,
                      top_k = 3, threshold = 0.05),
         "diag-s5-wt-vga-catchment-filt.png")

# Connections
save_fig(plot_connections(result_vga),
         "diag-s5-conn-vga-overview.png")
save_fig(plot_connections(result_bh),
         "diag-s5-conn-bh-overview.png")
save_fig(plot_connections(result_vga, tract = 1),
         "diag-s5-conn-vga-detail1.png")
save_fig(plot_connections(result_vga,
                          tract = c(1, mid_idx, length(vga_ids)),
                          top_k = 3),
         "diag-s5-conn-vga-detail-multi.png")


# Interactive widgets (save as self-contained HTML)
message("\n===== Section 5b: Interactive widgets =====")

if (requireNamespace("mapview", quietly = TRUE) &&
    requireNamespace("leaflet", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {

  tryCatch({
    widget <- plot_connections(result_vga, interactive = TRUE)
    htmlwidgets::saveWidget(
      widget@map,
      file.path(normalizePath(out_fig), "diag-s5-conn-vga-interactive.html"),
      selfcontained = TRUE
    )
    message("  Saved: diag-s5-conn-vga-interactive.html")
  }, error = function(e) message("  SKIP conn-vga interactive: ", e$message))

  tryCatch({
    widget <- plot_connections(result_bh, interactive = TRUE)
    htmlwidgets::saveWidget(
      widget@map,
      file.path(normalizePath(out_fig), "diag-s5-conn-bh-interactive.html"),
      selfcontained = TRUE
    )
    message("  Saved: diag-s5-conn-bh-interactive.html")
  }, error = function(e) message("  SKIP conn-bh interactive: ", e$message))

  tryCatch({
    widget <- plot_weights(result_vga, type = "dominant", interactive = TRUE)
    htmlwidgets::saveWidget(
      widget@map,
      file.path(normalizePath(out_fig), "diag-s5-dom-vga-interactive.html"),
      selfcontained = TRUE
    )
    message("  Saved: diag-s5-dom-vga-interactive.html")
  }, error = function(e) message("  SKIP dom-vga interactive: ", e$message))

  tryCatch({
    widget <- plot_weights(result_bh, type = "entropy", interactive = TRUE)
    htmlwidgets::saveWidget(
      widget@map,
      file.path(normalizePath(out_fig), "diag-s5-ent-bh-interactive.html"),
      selfcontained = TRUE
    )
    message("  Saved: diag-s5-ent-bh-interactive.html")
  }, error = function(e) message("  SKIP ent-bh interactive: ", e$message))

} else {
  message("  SKIPPED: mapview/leaflet/htmlwidgets not installed")
}


# ── Section 6: Travel times ──────────────────────────────────────────────────

message("\n===== Section 6: Travel times =====")

save_fig(plot_travel_times(result_vga, type = "histogram"),
         "diag-s6-tt-vga-hist.png")
save_fig(plot_travel_times(result_bh, type = "histogram"),
         "diag-s6-tt-bh-hist.png")
save_fig(plot_travel_times(result_vga, type = "heatmap"),
         "diag-s6-tt-vga-heatmap.png", width = 8, height = 8)
save_fig(plot_travel_times(result_vga, type = "map", tract = 1),
         "diag-s6-tt-vga-map.png")


# ── Section 7: Spatial autocorrelation ────────────────────────────────────────

message("\n===== Section 7: Spatial autocorrelation =====")

if (requireNamespace("spdep", quietly = TRUE)) {
  save_fig(plot_residual_autocorrelation(result_vga),
           "diag-s7-moran-resid-vga.png")
  save_fig(plot_residual_autocorrelation(result_vga,
                                         residual_type = "pearson"),
           "diag-s7-moran-resid-vga-pear.png")
  save_fig(plot_residual_autocorrelation(result_nit, summary_fn = "mean"),
           "diag-s7-moran-resid-nit-mean.png")

  save_fig(plot_moran(result_vga, variable = "Lula", type = "lisa"),
           "diag-s7-lisa-vga-lula.png")
  save_fig(plot_moran(result_vga, variable = "Lula", type = "moran"),
           "diag-s7-moran-vga-lula.png")
  save_fig(plot_moran(result_bh, variable = "Lula", type = "lisa"),
           "diag-s7-lisa-bh-lula.png")
  save_fig(plot_moran(result_nit, variable = "PT", type = "lisa"),
           "diag-s7-lisa-nit-pt.png")
} else {
  message("  SKIPPED: spdep not installed")
}


# ── Section 8: Baseline comparisons ──────────────────────────────────────────

message("\n===== Section 8: Baseline comparisons =====")

save_txt(compare_baselines(result_vga),
         "diag-s8-baselines-vga.txt")
save_txt(compare_baselines(result_bh,
                           methods = c("nearest", "uniform")),
         "diag-s8-baselines-bh.txt")
save_txt(compare_baselines(result_igr),
         "diag-s8-baselines-igr.txt")

save_txt(leave_one_out(result_igr),
         "diag-s8-loo-igr.txt")
save_txt(leave_one_out(result_vga),
         "diag-s8-loo-vga.txt")


# ── Section 9: Ecological validation ─────────────────────────────────────────

message("\n===== Section 9a: Turnout rates =====")

save_fig(plot_turnout_rates(result_vga, type = "bracket"),
         "diag-s9-turnout-vga-bracket.png", width = 10, height = 5)
save_fig(plot_turnout_rates(result_nit, type = "bracket"),
         "diag-s9-turnout-nit-bracket.png", width = 10, height = 5)
save_fig(plot_turnout_rates(result_bh, type = "map"),
         "diag-s9-turnout-bh-map.png")
save_fig(plot_turnout_rates(result_vga, type = "histogram"),
         "diag-s9-turnout-vga-hist.png", width = 10, height = 5)


message("\n===== Section 9b: Income-vote correlation =====")

if (requireNamespace("censobr", quietly = TRUE)) {
  tryCatch({
    save_fig(plot_income(result_vga, variable = "Lula", type = "scatter"),
             "diag-s9-income-vga-lula.png")
    save_fig(plot_income(result_vga, variable = 22, type = "scatter"),
             "diag-s9-income-vga-bolso.png")
    save_fig(plot_income(result_bh, variable = "Lula", type = "scatter"),
             "diag-s9-income-bh-lula.png")
    save_fig(plot_income(result_nit, variable = "Lula", type = "map"),
             "diag-s9-income-nit-map.png", width = 12, height = 6)
  }, error = function(e) {
    message("  plot_income() error: ", conditionMessage(e))
  })
} else {
  message("  SKIPPED: censobr not installed")
}


message("\n===== Section 9c: Cross-round consistency =====")

save_fig(plot_ecological(result_vga, result_vga2,
                         variable = "Lula", type = "scatter"),
         "diag-s9-ecol-vga-scatter.png")
save_fig(plot_ecological(result_vga, result_vga2,
                         variable = "Lula", type = "map"),
         "diag-s9-ecol-vga-map.png", width = 12, height = 6)


# ═══════════════════════════════════════════════════════════════════════════════
message("\n\nDone! All diagnostic artifacts generated.")
message("  RDS files: ", out_data)
message("  Figures:   ", out_fig)

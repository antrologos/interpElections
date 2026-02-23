# Trim example data files to reasonable sizes for package distribution
# Run after create_brazil_example_data.R
library(sf)

extdir <- "inst/extdata"

# 1. Pop data: filter to just Boa Vista (code_muni = 1400100)
cat("Trimming pop_data...\n")
pop_data <- readRDS(file.path(extdir, "example_pop_data.rds"))
cat(sprintf("  Before: %d rows, %.1f MB\n", nrow(pop_data),
            file.size(file.path(extdir, "example_pop_data.rds")) / 1e6))
pop_data <- pop_data[pop_data$code_muni == 1400100, ]
saveRDS(pop_data, file.path(extdir, "example_pop_data.rds"))
cat(sprintf("  After: %d rows, %.1f KB\n", nrow(pop_data),
            file.size(file.path(extdir, "example_pop_data.rds")) / 1e3))

# 2. Electoral data: keep only first 10 CAND_* columns + essentials
cat("Trimming electoral_data...\n")
elec <- readRDS(file.path(extdir, "example_electoral_data.rds"))
cat(sprintf("  Before: %d cols, %.1f KB\n", ncol(elec),
            file.size(file.path(extdir, "example_electoral_data.rds")) / 1e3))
cand_cols <- grep("^CAND_", names(elec), value = TRUE)
keep_cands <- head(cand_cols, 10)
essential_cols <- c("id", "lat", "long",
                    grep("^votantes_", names(elec), value = TRUE),
                    "QT_COMPARECIMENTO")
essential_cols <- intersect(essential_cols, names(elec))
elec_trim <- elec[, c(essential_cols, keep_cands)]
saveRDS(elec_trim, file.path(extdir, "example_electoral_data.rds"))
cat(sprintf("  After: %d cols, %.1f KB\n", ncol(elec_trim),
            file.size(file.path(extdir, "example_electoral_data.rds")) / 1e3))

# 3. Result: strip heavy matrices and trim to 10 candidates
cat("Trimming br_result...\n")
br_result <- readRDS(file.path(extdir, "example_br_result.rds"))
cat(sprintf("  Before: %.1f MB\n",
            file.size(file.path(extdir, "example_br_result.rds")) / 1e6))

# Strip heavy matrices
br_result$weights <- NULL
br_result$time_matrix <- NULL
br_result$electoral_sf <- NULL
br_result$pop_data <- NULL

# Trim interpolated matrix to 10 candidates
interp_mat <- br_result$interpolated
interp_cands <- grep("^CAND_", colnames(interp_mat), value = TRUE)
keep_interp <- head(interp_cands, 10)
other_cols <- setdiff(colnames(interp_mat), interp_cands)
br_result$interpolated <- interp_mat[, c(other_cols, keep_interp), drop = FALSE]

# Update interp_cols
old_interp <- br_result$interp_cols
br_result$interp_cols <- intersect(c(other_cols, keep_interp), old_interp)
if (length(br_result$interp_cols) == 0) {
  br_result$interp_cols <- keep_interp
}

# Trim tracts_sf: geometry + pop columns + calibration + 10 candidates
geom_col <- attr(br_result$tracts_sf, "sf_column")
sf_cols <- unique(c("code_tract",
                     grep("^pop_", names(br_result$tracts_sf), value = TRUE),
                     geom_col, keep_interp, "QT_COMPARECIMENTO"))
sf_cols <- intersect(sf_cols, names(br_result$tracts_sf))
br_result$tracts_sf <- br_result$tracts_sf[, sf_cols]

# Trim sources
if (!is.null(br_result$sources)) {
  src_cols <- c("id", "lat", "long",
                grep("^votantes_", names(br_result$sources), value = TRUE),
                keep_interp, "QT_COMPARECIMENTO")
  src_cols <- intersect(src_cols, names(br_result$sources))
  br_result$sources <- br_result$sources[, src_cols, drop = FALSE]
}

saveRDS(br_result, file.path(extdir, "example_br_result.rds"))
cat(sprintf("  After: %.1f KB\n",
            file.size(file.path(extdir, "example_br_result.rds")) / 1e3))
cat(sprintf("  interp_cols: %d\n", length(br_result$interp_cols)))
cat(sprintf("  interpolated: %d x %d\n", nrow(br_result$interpolated),
            ncol(br_result$interpolated)))
cat(sprintf("  tracts_sf: %d x %d\n", nrow(br_result$tracts_sf),
            ncol(br_result$tracts_sf)))

cat("\nFinal file sizes:\n")
for (f in list.files(extdir, pattern = "^example_.*\\.rds$")) {
  fp <- file.path(extdir, f)
  cat(sprintf("  %s: %.1f KB\n", f, file.size(fp) / 1e3))
}

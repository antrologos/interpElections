# Tests for plot_residuals() and residual_summary() diagnostics

# --- Shared mock helper ---

.mock_diag_result <- function(n = 10, m = 5, k = 14, p = 3) {
  set.seed(42)
  pop_cols <- paste0("pop_hom_", c("18_20","21_24","25_29","30_39",
                                    "40_49","50_59","60_69"))
  pop_cols <- c(pop_cols,
                paste0("pop_mul_", c("18_20","21_24","25_29","30_39",
                                      "40_49","50_59","60_69")))
  pop_cols <- pop_cols[seq_len(k)]
  src_cols <- paste0("src_", seq_len(k))
  interp_names <- paste0("VAR_", seq_len(p))

  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(x0, y0, x0+1, y0, x0+1, y0+1,
                                  x0, y0+1, x0, y0),
                                ncol = 2, byrow = TRUE)))
  })
  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  for (col in pop_cols) tracts_df[[col]] <- rpois(n, 80)
  tracts_sf <- sf::st_sf(tracts_df,
                          geometry = sf::st_sfc(polys, crs = 4326))

  interp_mat <- matrix(runif(n * p, 10, 200), n, p)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) sources[[col]] <- rpois(m, 160)
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha_mat <- matrix(runif(n * k, 1, 5), n, k)
  K <- (tt + 1)^(-rowMeans(alpha_mat))
  cs <- colSums(K); cs[cs == 0] <- 1
  W <- t(t(K) / cs)

  result <- list(
    interpolated = interp_mat, alpha = alpha_mat,
    tracts_sf = tracts_sf, sources = sources,
    optimization = list(method = "pb_sgd_colnorm_cpu", value = 50,
                        convergence = 0L),
    offset = 1, call = quote(interpolate_election()),
    tract_id = "zone_id", point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = pop_cols, sources = src_cols),
    weights = W, time_matrix = tt, electoral_sf = NULL,
    code_muni = NULL, muni_boundary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


# ============================================================
# plot_residuals() tests
# ============================================================

# --- Map type ---

test_that("plot_residuals map returns ggplot with raw residuals", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", residual_type = "raw")
  expect_s3_class(p, "gg")
  expect_true(grepl("Raw", p$labels$title))
})

test_that("plot_residuals map with pearson residuals", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", residual_type = "pearson")
  expect_s3_class(p, "gg")
  expect_true(grepl("Pearson", p$labels$title))
})

test_that("plot_residuals map with deviance residuals", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", residual_type = "deviance")
  expect_s3_class(p, "gg")
  expect_true(grepl("Deviance", p$labels$title))
})

test_that("plot_residuals map with summary_fn = mean", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", summary_fn = "mean")
  expect_s3_class(p, "gg")
  expect_true(grepl("MEAN", p$labels$title))
})

test_that("plot_residuals map with summary_fn = mean_abs", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", summary_fn = "mean_abs")
  expect_s3_class(p, "gg")
})

test_that("plot_residuals map with summary_fn = max_abs", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", summary_fn = "max_abs")
  expect_s3_class(p, "gg")
})

test_that("plot_residuals map with bracket index", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", summary_fn = 5)
  expect_s3_class(p, "gg")
  expect_true(grepl("bracket 5", p$labels$title))
})

test_that("plot_residuals map with continuous breaks", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "map", breaks = "continuous")
  expect_s3_class(p, "gg")
})

test_that("plot_residuals map errors without tracts_sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()
  obj$tracts_sf <- NULL

  expect_error(plot_residuals(obj, type = "map"))
})


# --- Histogram type ---

test_that("plot_residuals histogram returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "histogram")
  expect_s3_class(p, "gg")
  # Should be faceted
  expect_false(inherits(p$facet, "FacetNull"))
})

test_that("plot_residuals histogram with brackets filter", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "histogram", brackets = c(1, 7))
  expect_s3_class(p, "gg")
})

test_that("plot_residuals histogram with single bracket (k=1)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result(k = 1)

  p <- plot_residuals(obj, type = "histogram")
  expect_s3_class(p, "gg")
  # Single bracket -> no faceting
  expect_true(inherits(p$facet, "FacetNull"))
})

test_that("plot_residuals histogram has vertical zero line", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "histogram")
  # Check vline layer exists
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomVline" %in% layer_types)
})

test_that("plot_residuals histogram with pearson type", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "histogram", residual_type = "pearson")
  expect_s3_class(p, "gg")
  expect_true(grepl("Pearson", p$labels$title))
})


# --- Bracket boxplot type ---

test_that("plot_residuals bracket returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "bracket")
  expect_s3_class(p, "gg")
})

test_that("plot_residuals bracket with brackets filter", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "bracket", brackets = c(1, 7, 14))
  expect_s3_class(p, "gg")
})

test_that("plot_residuals bracket has horizontal zero line", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "bracket")
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_types)
})

test_that("plot_residuals bracket has rotated x labels", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "bracket")
  theme_el <- p$theme$axis.text.x
  expect_equal(theme_el$angle, 45)
})


# --- Scatter type ---

test_that("plot_residuals scatter returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "scatter")
  expect_s3_class(p, "gg")
})

test_that("plot_residuals scatter with brackets filter", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "scatter", brackets = c(1, 7))
  expect_s3_class(p, "gg")
})

test_that("plot_residuals scatter with single bracket (k=1)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result(k = 1)

  p <- plot_residuals(obj, type = "scatter")
  expect_s3_class(p, "gg")
  # Single bracket -> no faceting
  expect_true(inherits(p$facet, "FacetNull"))
})

test_that("plot_residuals scatter has reference line", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_residuals(obj, type = "scatter")
  layer_types <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomAbline" %in% layer_types)
})

test_that("plot_residuals scatter errors without weights", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()
  obj$weights <- NULL
  obj$optimization$W <- NULL

  expect_error(plot_residuals(obj, type = "scatter"), "Weight matrix")
})


# ============================================================
# residual_summary() tests
# ============================================================

test_that("residual_summary returns correct structure", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  expect_true(is.list(s))
  expect_true(all(c("per_bracket", "per_tract", "overall") %in% names(s)))
})

test_that("residual_summary per_bracket has correct dimensions", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  pb <- s$per_bracket
  expect_true(is.data.frame(pb))
  expect_equal(nrow(pb), 14)  # k = 14
  expect_true(all(c("bracket", "mean", "rmse", "max_abs", "pct_gt_2sd")
                   %in% names(pb)))
})

test_that("residual_summary per_tract has correct dimensions", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  pt <- s$per_tract
  expect_true(is.data.frame(pt))
  expect_equal(nrow(pt), 10)  # n = 10
  expect_true(all(c("tract_id", "mean", "rmse", "worst_bracket")
                   %in% names(pt)))
})

test_that("residual_summary per_tract uses tract_id from result", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  expect_equal(as.character(s$per_tract$tract_id),
               paste0("Z", 1:10))
})

test_that("residual_summary overall has correct elements", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  ov <- s$overall
  expect_true(is.list(ov))
  expect_true(all(c("rmse", "mean_bias", "total_deviance")
                   %in% names(ov)))
  expect_true(is.numeric(ov$rmse))
  expect_true(is.numeric(ov$mean_bias))
  expect_true(is.numeric(ov$total_deviance))
})

test_that("residual_summary overall rmse is positive", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  expect_true(s$overall$rmse > 0)
})

test_that("residual_summary with pearson type works", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj, type = "pearson")
  expect_true(is.data.frame(s$per_bracket))
  expect_equal(nrow(s$per_bracket), 14)
})

test_that("residual_summary with deviance type works", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj, type = "deviance")
  expect_true(is.data.frame(s$per_bracket))
  expect_equal(nrow(s$per_bracket), 14)
})

test_that("residual_summary with single bracket (k=1)", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(k = 1)

  s <- residual_summary(obj)
  expect_equal(nrow(s$per_bracket), 1)
  expect_equal(nrow(s$per_tract), 10)
})

test_that("residual_summary pct_gt_2sd is between 0 and 100", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  pcts <- s$per_bracket$pct_gt_2sd
  expect_true(all(pcts >= 0 & pcts <= 100))
})

test_that("residual_summary worst_bracket is a valid bracket label", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  labels <- .bracket_labels(obj)
  expect_true(all(s$per_tract$worst_bracket %in% labels))
})

test_that("residual_summary per_bracket rmse >= 0", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  expect_true(all(s$per_bracket$rmse >= 0))
})

test_that("residual_summary per_bracket max_abs >= 0", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  s <- residual_summary(obj)
  expect_true(all(s$per_bracket$max_abs >= 0))
})

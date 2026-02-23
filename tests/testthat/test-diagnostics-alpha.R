# Tests for plot_alpha() diagnostics

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


# --- Map type tests ---

test_that("plot_alpha map returns ggplot with matrix alpha", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map with vector alpha works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()
  obj$alpha <- runif(10, 1, 5)

  p <- plot_alpha(obj, type = "map")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map with summary_fn = mean works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", summary_fn = "mean")
  expect_s3_class(p, "gg")
  expect_true(grepl("mean", p$labels$title))
})

test_that("plot_alpha map with summary_fn = bracket index works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", summary_fn = 3)
  expect_s3_class(p, "gg")
  expect_true(grepl("bracket 3", p$labels$title))
})

test_that("plot_alpha map with pop_weighted summary works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", summary_fn = "pop_weighted")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map with continuous breaks works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", breaks = "continuous")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map with custom breaks works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", breaks = c(1, 2, 3, 4, 5))
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map errors without tracts_sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()
  obj$tracts_sf <- NULL

  expect_error(plot_alpha(obj, type = "map"), "tracts_sf")
})


# --- Histogram type tests ---

test_that("plot_alpha histogram returns ggplot with matrix alpha", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "histogram")
  expect_s3_class(p, "gg")
  # Should have faceting for brackets
  expect_false(inherits(p$facet, "FacetNull"))
})

test_that("plot_alpha histogram with vector alpha (single panel)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()
  obj$alpha <- runif(10, 1, 5)

  p <- plot_alpha(obj, type = "histogram")
  expect_s3_class(p, "gg")
  # No faceting for vector alpha
  expect_true(inherits(p$facet, "FacetNull"))
})

test_that("plot_alpha histogram with brackets filter", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "histogram", brackets = c(1, 3, 5))
  expect_s3_class(p, "gg")
})

test_that("plot_alpha histogram uses bracket labels", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "histogram")
  # Build the plot to access data
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(p, "gg")
})


# --- Bracket boxplot type tests ---

test_that("plot_alpha bracket returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "bracket")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha bracket with brackets filter", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "bracket", brackets = c(1, 7, 14))
  expect_s3_class(p, "gg")
})

test_that("plot_alpha bracket errors with vector alpha", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()
  obj$alpha <- runif(10, 1, 5)

  expect_error(plot_alpha(obj, type = "bracket"), "matrix alpha")
})

test_that("plot_alpha bracket has rotated x labels", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "bracket")
  theme_el <- p$theme$axis.text.x
  expect_equal(theme_el$angle, 45)
})


# --- Edge cases ---

test_that("plot_alpha errors with NULL alpha", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()
  obj$alpha <- NULL

  expect_error(plot_alpha(obj), "No alpha values")
})

test_that("plot_alpha with single-column matrix alpha treated as vector", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result(k = 1)
  # alpha is n x 1 matrix

  p <- plot_alpha(obj, type = "map")
  expect_s3_class(p, "gg")
  # Title should not have summary function
  expect_equal(p$labels$title, "Alpha decay parameter")
})

test_that("plot_alpha map with range summary works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", summary_fn = "range")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map with min summary works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", summary_fn = "min")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map with max summary works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", summary_fn = "max")
  expect_s3_class(p, "gg")
})

test_that("plot_alpha map with different palette", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_diag_result()

  p <- plot_alpha(obj, type = "map", palette = "viridis",
                  breaks = "continuous")
  expect_s3_class(p, "gg")
})

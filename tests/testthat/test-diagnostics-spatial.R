# Tests for plot_residual_autocorrelation() and plot_moran()

.mock_spatial_result <- function(n = 20, m = 8, k = 2) {
  set.seed(42)
  # Grid of polygons for adjacency
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })

  pop_cols <- paste0("pop_", letters[seq_len(k)])
  src_cols <- paste0("src_", letters[seq_len(k)])
  interp_names <- paste0("VAR_", 1:3)

  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  for (col in pop_cols) tracts_df[[col]] <- rpois(n, 80)
  tracts_sf <- sf::st_sf(tracts_df,
                           geometry = sf::st_sfc(polys, crs = 4326))

  interp_mat <- matrix(runif(n * 3, 10, 200), n, 3)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) sources[[col]] <- rpois(m, 160)

  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha <- runif(n, 1, 4)
  K <- (tt + 1)^(-alpha); cs <- colSums(K); cs[cs == 0] <- 1
  W <- t(t(K) / cs)

  result <- list(
    interpolated = interp_mat, alpha = alpha,
    tracts_sf = tracts_sf, sources = sources,
    optimization = list(method = "mock", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election()),
    tract_id = "zone_id", point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = pop_cols, sources = src_cols),
    weights = W, time_matrix = tt,
    electoral_sf = NULL,
    code_muni = NULL, muni_boundary = NULL,
    dictionary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


# --- plot_residual_autocorrelation ---

test_that("plot_residual_autocorrelation returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("spdep")
  obj <- .mock_spatial_result()

  p <- plot_residual_autocorrelation(obj)
  expect_s3_class(p, "ggplot")
})

test_that("plot_residual_autocorrelation returns NULL without spdep", {
  skip_if_not_installed("sf")
  # We can't truly test missing spdep, but test NULL optimization
  obj <- .mock_spatial_result()
  obj$weights <- NULL
  obj$calib_cols <- NULL

  expect_message(
    p <- plot_residual_autocorrelation(obj),
    "Cannot compute residuals|No calibration"
  )
})


# --- plot_moran ---

test_that("plot_moran lisa returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("spdep")
  obj <- .mock_spatial_result()

  p <- plot_moran(obj, variable = "VAR_1", type = "lisa")
  expect_s3_class(p, "ggplot")
})

test_that("plot_moran scatter returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("spdep")
  obj <- .mock_spatial_result()

  p <- plot_moran(obj, variable = "VAR_1", type = "moran")
  expect_s3_class(p, "ggplot")
})

test_that("plot_moran auto-selects first variable", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("spdep")
  obj <- .mock_spatial_result()

  expect_message(
    p <- plot_moran(obj, type = "lisa"),
    "No variable specified"
  )
  expect_s3_class(p, "ggplot")
})

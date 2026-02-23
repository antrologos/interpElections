# Tests for plot_convergence() function

# Helper: build a mock result with optimization histories
.mock_convergence_result <- function(n = 10, m = 5, k = 2, p = 3,
                                      n_epochs = 20) {
  set.seed(42)

  pop_cols <- paste0("pop_", letters[seq_len(k)])
  src_cols <- paste0("src_", letters[seq_len(k)])
  interp_names <- paste0("VAR_", seq_len(p))

  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5
    y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  for (col in pop_cols) tracts_df[[col]] <- rpois(n, 80)
  tracts_sf <- sf::st_sf(tracts_df, geometry = sfc)

  interp_mat <- matrix(runif(n * p, 10, 200), n, p)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) sources[[col]] <- rpois(m, 160)

  alpha <- runif(n, 0.5, 3)

  result <- list(
    interpolated = interp_mat,
    alpha = alpha,
    tracts_sf = tracts_sf,
    sources = sources,
    optimization = list(
      method = "pb_sgd_colnorm_cpu",
      value = 50.0,
      convergence = 0L,
      epochs = n_epochs,
      history = seq(100, 50, length.out = n_epochs),
      grad_history = seq(10, 0.1, length.out = n_epochs),
      lr_history = rep(0.05, n_epochs)
    ),
    offset = 1,
    call = quote(interpolate_election()),
    tract_id = "zone_id",
    point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = pop_cols, sources = src_cols),
    weights = NULL,
    time_matrix = NULL,
    electoral_sf = NULL,
    code_muni = NULL,
    dictionary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


# --- Basic behavior ---

test_that("plot_convergence returns a ggplot with all panels", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj)
  expect_s3_class(p, "ggplot")
})

test_that("plot_convergence with single panel works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, which = "loss")
  expect_s3_class(p, "ggplot")
})

test_that("plot_convergence with two panels works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, which = c("loss", "gradient"))
  expect_s3_class(p, "ggplot")
})


# --- NULL optimization ---

test_that("plot_convergence returns NULL with message when no optimization", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()
  obj$optimization <- NULL

  expect_message(
    result <- plot_convergence(obj),
    "No optimization data"
  )
  expect_null(result)
})


# --- Missing histories ---

test_that("plot_convergence skips panels with NULL history", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()
  obj$optimization$grad_history <- NULL
  obj$optimization$lr_history <- NULL

  # Should still produce a plot with just the loss panel
  p <- plot_convergence(obj)
  expect_s3_class(p, "ggplot")
})

test_that("plot_convergence returns NULL when all requested histories missing", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()
  obj$optimization$history <- NULL

  expect_message(
    result <- plot_convergence(obj, which = "loss"),
    "No history data"
  )
  expect_null(result)
})


# --- log_y parameter ---

test_that("plot_convergence with log_y = FALSE works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, log_y = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_convergence with log_y = TRUE for loss+gradient only", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, which = c("loss", "gradient"), log_y = TRUE)
  expect_s3_class(p, "ggplot")
})


# --- Error handling ---

test_that("plot_convergence errors on non-result object", {
  skip_if_not_installed("ggplot2")
  expect_error(plot_convergence(list(a = 1)), "interpElections_result")
})

test_that("plot_convergence with invalid which gives error", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  expect_error(plot_convergence(obj, which = "invalid"))
})

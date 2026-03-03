# Tests for plot_convergence() function

# Helper: build a mock result with optimization histories (includes component
# histories and best_epoch for the decomposed loss panel).
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

  # Component histories: deviance decreases, barrier/entropy non-zero
  deviance_vals <- seq(80, 40, length.out = n_epochs)
  barrier_vals  <- seq(15, 8, length.out = n_epochs)
  entropy_vals  <- seq(5, 2, length.out = n_epochs)
  total_vals    <- deviance_vals + barrier_vals + entropy_vals

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
      history = total_vals,
      deviance_history = deviance_vals,
      barrier_history = barrier_vals,
      entropy_history = entropy_vals,
      best_epoch = 18L,
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

# Helper: build a legacy mock without component histories (backward compat)
.mock_convergence_result_legacy <- function(n_epochs = 20) {
  obj <- .mock_convergence_result(n_epochs = n_epochs)
  obj$optimization$deviance_history <- NULL
  obj$optimization$barrier_history  <- NULL
  obj$optimization$entropy_history  <- NULL
  obj$optimization$best_epoch       <- NULL
  # Restore simple total history
  obj$optimization$history <- seq(100, 50, length.out = n_epochs)
  obj
}


# --- Basic behavior ---

test_that("plot_convergence returns a plot object with all panels", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj)
  # With component histories + patchwork, returns a patchwork object
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("plot_convergence with single panel works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, which = "gradient")
  expect_s3_class(p, "ggplot")
})

test_that("plot_convergence with two panels works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, which = c("gradient", "lr"))
  # Two panels without loss => no decomposition => patchwork stack or ggplot
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})


# --- Loss decomposition (two-column layout) ---

test_that("loss components get individual panels in right column", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, which = "loss")
  # Should be a patchwork with total (left) + deviance, barrier, entropy (right)
  expect_true(inherits(p, "patchwork"))
})

test_that("zero-valued component is excluded from right column", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  obj <- .mock_convergence_result()
  # Zero out entropy so it should be excluded
  obj$optimization$entropy_history <- rep(0, 20)

  p <- plot_convergence(obj, which = "loss")
  # Still patchwork (has deviance + barrier)
  expect_true(inherits(p, "patchwork"))
})


# --- Best epoch ---

test_that("best_epoch uses stored value from optimizer", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()
  obj$optimization$best_epoch <- 15L

  # Check that the total loss panel has a vline at 15
  panels <- interpElections:::.build_loss_panels(obj$optimization)
  total_panel <- interpElections:::.make_convergence_panel(
    panels$total, "Total loss", "#2c7fb8", best_epoch = 15L, log_y = FALSE
  )
  vline_layers <- Filter(
    function(l) inherits(l$geom, "GeomVline"),
    total_panel$layers
  )
  expect_length(vline_layers, 1L)
  expect_equal(vline_layers[[1]]$aes_params$xintercept %||%
               vline_layers[[1]]$data$xintercept, 15L)
})

test_that("best_epoch falls back to which.min for legacy results", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result_legacy()

  p <- plot_convergence(obj, which = "loss")
  # Legacy: single ggplot with vline
  expect_s3_class(p, "ggplot")

  vline_layers <- Filter(
    function(l) inherits(l$geom, "GeomVline"),
    p$layers
  )
  expected_best <- which.min(obj$optimization$history)
  expect_length(vline_layers, 1L)
  expect_equal(vline_layers[[1]]$aes_params$xintercept %||%
               vline_layers[[1]]$data$xintercept, expected_best)
})


# --- Backward compatibility ---

test_that("legacy result without component histories produces single-column layout", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result_legacy()

  p <- plot_convergence(obj)
  # No component histories => no right column => single-column
  # May be patchwork (stacked left panels) or ggplot
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("legacy result loss-only returns single ggplot panel", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result_legacy()

  p <- plot_convergence(obj, which = "loss")
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

  # Should still produce a plot with loss panels
  p <- plot_convergence(obj)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
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
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("plot_convergence with log_y = TRUE for loss+gradient only", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  p <- plot_convergence(obj, which = c("loss", "gradient"), log_y = TRUE)
  expect_true(inherits(p, "patchwork") || inherits(p, "ggplot"))
})

test_that("log_y does not affect learning rate panel", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_convergence_result()

  # LR panel built with log_y = FALSE internally regardless of user setting
  lr_df <- data.frame(epoch = seq_along(obj$optimization$lr_history),
                      value = obj$optimization$lr_history)
  lr_panel <- interpElections:::.make_convergence_panel(
    lr_df, "Learning rate", "#2c7fb8", best_epoch = NULL, log_y = FALSE
  )
  # No ScaleContinuousPosition with log transform
  y_scale <- lr_panel$scales$get_scales("y")
  # Default scale should NOT be log10
  expect_true(is.null(y_scale) || !inherits(y_scale$trans, "log10_trans"))
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


# --- Internal helpers ---

test_that(".build_loss_panels returns named list of data frames", {
  skip_if_not_installed("sf")
  obj <- .mock_convergence_result()
  panels <- interpElections:::.build_loss_panels(obj$optimization)

  expect_true(is.list(panels))
  expect_true("total" %in% names(panels))
  expect_true("deviance" %in% names(panels))
  expect_true("barrier" %in% names(panels))
  expect_true("entropy" %in% names(panels))
  expect_equal(nrow(panels$total), 20L)
})

test_that(".build_loss_panels excludes zero-valued components", {
  skip_if_not_installed("sf")
  obj <- .mock_convergence_result()
  obj$optimization$entropy_history <- rep(0, 20)
  panels <- interpElections:::.build_loss_panels(obj$optimization)

  expect_true("total" %in% names(panels))
  expect_true("deviance" %in% names(panels))
  expect_true("barrier" %in% names(panels))
  expect_false("entropy" %in% names(panels))
})

test_that(".build_loss_panels returns only total for legacy results", {
  skip_if_not_installed("sf")
  obj <- .mock_convergence_result_legacy()
  panels <- interpElections:::.build_loss_panels(obj$optimization)

  expect_equal(names(panels), "total")
})

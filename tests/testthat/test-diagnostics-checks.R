# Tests for diagnostics() function

# Helper: build a mock interpElections_result for diagnostics testing
.mock_diag_result <- function(
    n = 10, m = 5, k = 2, p = 3,
    keep_weights = FALSE, keep_time = FALSE,
    add_history = FALSE
) {
  set.seed(42)

  pop_cols <- paste0("pop_", letters[seq_len(k)])
  src_cols <- paste0("src_", letters[seq_len(k)])
  interp_names <- paste0("VAR_", seq_len(p))

  # Mock tracts_sf
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

  # Mock interpolated matrix
  interp_mat <- matrix(runif(n * p, 10, 200), n, p)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  # Mock sources data.frame
  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) sources[[col]] <- rpois(m, 160)
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  # Mock time matrix + weights
  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha <- runif(n, 0.5, 3)

  pop_mat_calib <- as.matrix(tracts_df[, pop_cols, drop = FALSE])
  storage.mode(pop_mat_calib) <- "double"
  src_mat_calib <- as.matrix(sources[, src_cols, drop = FALSE])
  storage.mode(src_mat_calib) <- "double"
  pop_total <- rowSums(pop_mat_calib)
  row_targets <- pop_total / sum(pop_total) * m

  K <- (tt + 1) ^ (-alpha)
  K[!is.finite(K)] <- 0
  cs <- colSums(K)
  cs[cs == 0] <- 1
  W <- t(t(K) / cs)

  optimization <- list(
    method = "pb_sgd_colnorm_cpu",
    value = 123.45,
    convergence = 0L,
    epochs = 20L
  )

  if (add_history) {
    optimization$history <- seq(100, 50, length.out = 20)
    optimization$grad_history <- seq(10, 0.1, length.out = 20)
    optimization$lr_history <- rep(0.05, 20)
  }

  result <- list(
    interpolated = interp_mat,
    alpha = alpha,
    tracts_sf = tracts_sf,
    sources = sources,
    optimization = optimization,
    offset = 1,
    row_targets = row_targets,
    call = quote(interpolate_election()),
    tract_id = "zone_id",
    point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = pop_cols, sources = src_cols),
    weights = if (keep_weights) W else NULL,
    time_matrix = if (keep_time) tt else NULL,
    electoral_sf = NULL,
    code_muni = NULL,
    dictionary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


# --- diagnostics() basic behavior ---

test_that("diagnostics returns interpElections_diagnostics class", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  out <- diagnostics(obj, verbose = FALSE)
  expect_s3_class(out, "interpElections_diagnostics")
  expect_true(is.list(out$checks))
})

test_that("diagnostics output contains expected check names", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  out <- diagnostics(obj, verbose = FALSE)
  expected_names <- c("convergence", "alpha_distribution", "residual_rmse",
                       "residual_bias", "pop_voter_gap", "unreachable_pairs",
                       "alpha_spatial_cv", "implied_turnout")
  expect_true(all(expected_names %in% names(out$checks)))
})

test_that("diagnostics verbose prints checklist", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  printed <- capture.output(diagnostics(obj, verbose = TRUE))
  full <- paste(printed, collapse = "\n")

  expect_match(full, "interpElections diagnostics")
  expect_match(full, "Convergence")
  expect_match(full, "Alpha distribution")
  expect_match(full, "PASS|WARN|FAIL|SKIP")
})

test_that("diagnostics verbose = FALSE suppresses output", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  printed <- capture.output(out <- diagnostics(obj, verbose = FALSE))
  expect_equal(length(printed), 0)
  expect_s3_class(out, "interpElections_diagnostics")
})


# --- Convergence check ---

test_that("convergence check passes with code 0", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$convergence$status, "pass")
  expect_match(out$checks$convergence$detail, "converged")
})

test_that("convergence check warns with code 1", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()
  obj$optimization$convergence <- 1L

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$convergence$status, "warn")
  expect_match(out$checks$convergence$detail, "NOT converge")
})

test_that("convergence check skips when optimization is NULL", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()
  obj$optimization <- NULL

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$convergence$status, "skip")
})


# --- Alpha distribution ---

test_that("alpha distribution reports median and IQR", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  out <- diagnostics(obj, verbose = FALSE)
  chk <- out$checks$alpha_distribution
  expect_match(chk$detail, "median=")
  expect_match(chk$detail, "IQR=")
})

test_that("alpha distribution warns when > 5% above 15", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()
  # Set most alphas above 15
  obj$alpha <- rep(20, length(obj$alpha))

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$alpha_distribution$status, "warn")
})


# --- Residual checks ---

test_that("residual checks work with weights present", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$residual_rmse$status, "pass")
  expect_true(is.numeric(out$checks$residual_rmse$value))
})

test_that("residual checks skip when no weights or time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = FALSE, keep_time = FALSE)

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$residual_rmse$status, "skip")
  expect_equal(out$checks$residual_bias$status, "skip")
})


# --- Population-voter gap ---

test_that("population-voter gap is computed correctly", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  out <- diagnostics(obj, verbose = FALSE)
  chk <- out$checks$pop_voter_gap
  expect_true(chk$status %in% c("pass", "warn", "fail"))
  expect_match(chk$detail, "census=")
  expect_match(chk$detail, "source=")
})


# --- Unreachable pairs ---

test_that("unreachable pairs skips without time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_time = FALSE)

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$unreachable_pairs$status, "skip")
})

test_that("unreachable pairs computed with time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_time = TRUE)

  out <- diagnostics(obj, verbose = FALSE)
  chk <- out$checks$unreachable_pairs
  expect_true(chk$status %in% c("pass", "warn", "fail"))
  expect_true(is.numeric(chk$value))
})


# --- Alpha spatial variation ---

test_that("alpha spatial CV is computed", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()

  out <- diagnostics(obj, verbose = FALSE)
  chk <- out$checks$alpha_spatial_cv
  expect_true(chk$status %in% c("pass", "warn"))
  expect_true(is.numeric(chk$value))
})

test_that("alpha spatial CV warns with constant alpha", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()
  obj$alpha <- rep(2.5, length(obj$alpha))

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$alpha_spatial_cv$status, "warn")
})


# --- Implied turnout ---

test_that("implied turnout computed with weights", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  out <- diagnostics(obj, verbose = FALSE)
  chk <- out$checks$implied_turnout
  expect_true(chk$status %in% c("pass", "warn"))
  expect_match(chk$detail, "implied turnout")
})

test_that("implied turnout skips without weights", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = FALSE)

  out <- diagnostics(obj, verbose = FALSE)
  expect_equal(out$checks$implied_turnout$status, "skip")
})


# --- Print method ---

test_that("print.interpElections_diagnostics re-prints checklist", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result(keep_weights = TRUE)

  diag <- diagnostics(obj, verbose = FALSE)
  printed <- capture.output(print(diag))
  full <- paste(printed, collapse = "\n")

  expect_match(full, "interpElections diagnostics")
  expect_match(full, "PASS")
  # Summary line at the end
  expect_match(full, "\\d+ PASS")
})


# --- Error handling ---

test_that("diagnostics errors on non-result object", {
  expect_error(diagnostics(list(a = 1)), "interpElections_result")
})

test_that("diagnostics works with minimal result (many skips)", {
  skip_if_not_installed("sf")
  obj <- .mock_diag_result()
  obj$optimization <- NULL
  obj$weights <- NULL
  obj$time_matrix <- NULL

  out <- diagnostics(obj, verbose = FALSE)
  expect_s3_class(out, "interpElections_diagnostics")
  # At least convergence and residuals should be skipped
  expect_equal(out$checks$convergence$status, "skip")
  expect_equal(out$checks$residual_rmse$status, "skip")
})

# Round-trip integration test:
# optimize_alpha → W from result → verify valid interpolation

test_that("optimize_alpha returns W that produces valid interpolation", {
  skip_if_not_installed("torch")

  set.seed(42)
  n <- 10  # zones
  m <- 8   # sources
  k <- 3   # demographic groups

  # Generate realistic-ish data
  time_matrix <- matrix(abs(rnorm(n * m, 15, 5)) + 1, n, m)
  source_matrix <- matrix(rpois(m * k, 50), m, k)
  storage.mode(source_matrix) <- "double"

  # Population = rough IDW at alpha=1 + noise
  pop_matrix <- matrix(rpois(n * k, 80), n, k)
  storage.mode(pop_matrix) <- "double"

  # Optimize
  result <- suppressWarnings(optimize_alpha(
    time_matrix = time_matrix,
    pop_matrix = pop_matrix,
    source_matrix = source_matrix,
    offset = 0,
    use_gpu = FALSE,
    max_epochs = 50L,
    verbose = FALSE
  ))

  expect_s3_class(result, "interpElections_optim")
  expect_true(all(result$alpha >= 0))
  expect_true(is.finite(result$value))

  # W is returned from optimizer
  expect_true(is.matrix(result$W))
  expect_equal(dim(result$W), c(n, m))

  # Interpolate using W from result
  interpolated <- result$W %*% source_matrix

  expect_equal(nrow(interpolated), n)
  expect_equal(ncol(interpolated), k)
  expect_true(all(is.finite(interpolated)))
})

test_that("compute_weight_matrix produces valid weights", {
  skip_if_not_installed("torch")

  set.seed(1)
  n <- 8; m <- 4; k <- 2
  time_matrix <- matrix(runif(n * m, 1, 30), nrow = n)
  alpha <- matrix(runif(n * k, 0.5, 2), n, k)
  pop <- matrix(rpois(n * k, 80), n, k) + 1
  src <- matrix(rpois(m * k, 160), m, k) + 1
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"

  W <- compute_weight_matrix(time_matrix, alpha, pop, src, verbose = FALSE)

  expect_true(is.matrix(W))
  expect_equal(dim(W), c(n, m))
  expect_true(all(W >= 0))
  expect_equal(colSums(W), rep(1, m), tolerance = 0.05)
})

test_that("W from optimize_alpha matches compute_weight_matrix with same alpha", {
  skip_if_not_installed("torch")

  set.seed(2)
  n <- 6; m <- 4; k <- 2
  time_matrix <- matrix(runif(n * m, 1, 30), nrow = n)
  pop <- matrix(rpois(n * k, 80), n, k) + 1
  src <- matrix(rpois(m * k, 150), m, k) + 1
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"

  result <- suppressWarnings(optimize_alpha(
    time_matrix, pop, src,
    use_gpu = FALSE, max_epochs = 30L, verbose = FALSE
  ))

  W_recomputed <- compute_weight_matrix(
    time_matrix, result$alpha, pop, src,
    offset = 1, verbose = FALSE
  )

  # Should be very close (both use the same column normalization)
  expect_equal(result$W, W_recomputed, tolerance = 0.01)
})

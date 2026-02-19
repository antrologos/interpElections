# Round-trip integration test:
# optimize_alpha → sinkhorn_weights → verify valid results

test_that("optimize + sinkhorn_weights round-trip produces valid results", {
  skip_if_not_installed("torch")

  set.seed(42)
  n <- 10  # zones
  m <- 8   # sources
  k <- 3   # demographic groups

  # Generate realistic-ish data
  time_matrix <- matrix(abs(rnorm(n * m, 15, 5)) + 1, n, m)
  source_matrix <- matrix(rpois(m * k, 50), m, k)
  storage.mode(source_matrix) <- "double"

  # Population = rough Sinkhorn-balanced IDW at alpha=1
  pop_total <- runif(n, 20, 200)
  r <- pop_total / sum(pop_total) * m
  W <- suppressWarnings(
    sinkhorn_weights(time_matrix, rep(1, n), offset = 0, row_targets = r)
  )
  pop_matrix <- W %*% source_matrix + matrix(rnorm(n * k, 0, 2), n, k)
  pop_matrix <- pmax(round(pop_matrix), 0)
  storage.mode(pop_matrix) <- "double"

  # Optimize
  result <- optimize_alpha(
    time_matrix = time_matrix,
    pop_matrix = pop_matrix,
    source_matrix = source_matrix,
    offset = 0,
    use_gpu = FALSE,
    max_steps = 50L,
    verbose = FALSE
  )

  expect_s3_class(result, "interpElections_optim")
  expect_true(all(result$alpha >= 0))
  expect_true(is.finite(result$value))

  # Build final weights (per-bracket mode)
  W_final <- suppressWarnings(sinkhorn_weights(
    time_matrix, result$alpha, offset = 0,
    row_targets = result$row_targets,
    pop_matrix = pop_matrix,
    source_matrix = source_matrix
  ))

  interpolated <- W_final %*% source_matrix

  expect_equal(nrow(interpolated), n)
  expect_equal(ncol(interpolated), k)
  expect_true(all(is.finite(interpolated)))
})

test_that("row sums match row_targets after Sinkhorn weighting", {
  set.seed(1)
  n <- 8
  m <- 4
  time_matrix <- matrix(runif(n * m, 1, 30), nrow = n)
  alpha <- runif(n, 0.5, 2)
  pop <- runif(n, 10, 200)
  r <- pop / sum(pop) * m

  W <- sinkhorn_weights(time_matrix, alpha, row_targets = r)

  expect_equal(rowSums(W), r, tolerance = 1e-6)
})

test_that("column sums approximately 1 after Sinkhorn weighting", {
  set.seed(2)
  n <- 8
  m <- 4
  time_matrix <- matrix(runif(n * m, 1, 30), nrow = n)
  alpha <- runif(n, 0.5, 2)
  pop <- runif(n, 10, 200)
  r <- pop / sum(pop) * m

  W <- sinkhorn_weights(time_matrix, alpha, row_targets = r)

  expect_equal(colSums(W), rep(1, m), tolerance = 1e-6)
})

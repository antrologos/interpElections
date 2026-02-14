# Round-trip integration test:
# optimize_alpha → idw_interpolate → verify objective is lower

test_that("optimize + interpolate round-trip produces valid results", {
  set.seed(42)
  n <- 10  # zones

  m <- 8   # sources
  k <- 3   # demographic groups

  # Generate realistic-ish data
  time_matrix <- matrix(abs(rnorm(n * m, 15, 5)) + 1, n, m)
  source_matrix <- matrix(rpois(m * k, 50), m, k)
  # Population = rough IDW at alpha=1 (so optimum is near 1)
  W <- time_matrix ^ (-1)
  W_std <- t(t(W) / colSums(W))
  pop_matrix <- W_std %*% source_matrix + matrix(rnorm(n * k, 0, 2), n, k)
  pop_matrix <- pmax(round(pop_matrix), 0)
  storage.mode(pop_matrix) <- "double"

  # Optimize
  result <- optimize_alpha(
    time_matrix = time_matrix,
    pop_matrix = pop_matrix,
    source_matrix = source_matrix,
    offset = 0,  # already offset
    verbose = FALSE,
    cpu_parallel = FALSE
  )

  expect_s3_class(result, "interpElections_optim")
  expect_true(all(result$alpha >= 0))
  expect_true(is.finite(result$value))

  # The optimized objective should be less than the initial (alpha=1)
  init_obj <- idw_objective(rep(1, n), time_matrix, pop_matrix, source_matrix)
  expect_true(result$value <= init_obj)

  # Interpolate with the optimal alpha
  interpolated <- idw_interpolate(
    time_matrix, result$alpha, source_matrix, offset = 0
  )

  expect_equal(nrow(interpolated), n)
  expect_equal(ncol(interpolated), k)
  expect_true(all(is.finite(interpolated)))
  # Column sums should be preserved (weights sum to 1 per column)
  expect_equal(colSums(interpolated), colSums(source_matrix),
               tolerance = 1e-6)
})

test_that("alpha = 0 produces uniform weights", {
  n <- 5
  m <- 3
  k <- 2
  time_matrix <- matrix(abs(rnorm(n * m)) + 1, n, m)
  alpha <- rep(0, n)

  W <- idw_weights(time_matrix, alpha, offset = 0)

  # All alpha = 0 means t^0 = 1 for all entries
  # So W_std should have all rows equal to 1/n
  expect_equal(W, matrix(1 / n, n, m), tolerance = 1e-10)
})

test_that("single row/column matrices work", {
  time_matrix <- matrix(c(5, 10), nrow = 2, ncol = 1)
  alpha <- c(1, 1)
  source_matrix <- matrix(100, nrow = 1, ncol = 1)
  pop_matrix <- matrix(c(60, 40), nrow = 2, ncol = 1)

  obj <- idw_objective(alpha, time_matrix, pop_matrix, source_matrix)
  expect_true(is.finite(obj))

  grad <- idw_gradient(alpha, time_matrix, pop_matrix, source_matrix)
  expect_length(grad, 2)
  expect_true(all(is.finite(grad)))
})

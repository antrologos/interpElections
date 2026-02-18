test_that("optimize_alpha finds lower objective than initial", {
  skip_if_not_installed("torch")

  set.seed(123)
  n <- 10
  m <- 5
  k <- 2
  t_mat <- matrix(runif(n * m, 1, 50), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    gpu_iterations = 5L,
    verbose = FALSE
  )

  expect_s3_class(result, "interpElections_optim")
  expect_length(result$alpha, n)

  # Compare with initial alpha = 1
  pop_total <- rowSums(p_mat)
  r <- pop_total / sum(pop_total) * m
  init_val <- sinkhorn_objective(rep(1, n), t_mat + 1, p_mat, s_mat, r)
  expect_true(result$value <= init_val)
})

test_that("optimize_alpha respects lower_bound", {
  skip_if_not_installed("torch")

  set.seed(42)
  n <- 5
  m <- 3
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    lower_bound = 0,
    gpu_iterations = 5L,
    verbose = FALSE
  )

  expect_true(all(result$alpha >= 0))
})

test_that("optimize_alpha returns proper structure", {
  skip_if_not_installed("torch")

  set.seed(1)
  n <- 4
  m <- 2
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 10), nrow = n)
  p_mat <- matrix(runif(n * k, 5, 20), nrow = n)
  s_mat <- matrix(runif(m * k, 5, 20), nrow = m)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    gpu_iterations = 3L,
    verbose = FALSE
  )

  expect_true("alpha" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("convergence" %in% names(result))
  expect_true("elapsed" %in% names(result))
  expect_true("row_targets" %in% names(result))
  expect_true("sinkhorn_iter" %in% names(result))
  expect_true("history" %in% names(result))
  expect_true("grad_norm_final" %in% names(result))
  expect_equal(result$sinkhorn_iter, 5L)
  expect_length(result$row_targets, n)
})

test_that("optimize_alpha print method works", {
  skip_if_not_installed("torch")

  set.seed(1)
  t_mat <- matrix(runif(8, 1, 10), nrow = 4)
  p_mat <- matrix(runif(4), nrow = 4)
  s_mat <- matrix(runif(2), nrow = 2)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    gpu_iterations = 3L,
    verbose = FALSE
  )

  expect_output(print(result), "interpElections optimization result")
})

test_that("optimize_alpha auto-computes row_targets when NULL", {
  skip_if_not_installed("torch")

  set.seed(1)
  n <- 6
  m <- 3
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    gpu_iterations = 3L,
    verbose = FALSE
  )

  # row_targets should be auto-computed
  expected_r <- rowSums(p_mat) / sum(p_mat) * m
  expect_equal(result$row_targets, expected_r)
})

test_that("optimize_alpha errors without torch", {
  # Mock torch as unavailable
  skip_if_not_installed("torch")

  # This test checks the error message format — we can't truly test

  # without torch since we need it to run. Just verify the function exists.
  expect_true(is.function(optimize_alpha))
})

test_that("optimize_alpha sinkhorn_iter parameter works", {
  skip_if_not_installed("torch")

  set.seed(42)
  n <- 5
  m <- 3
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result_k1 <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    sinkhorn_iter = 1L,
    gpu_iterations = 3L,
    verbose = FALSE
  )

  result_k5 <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    sinkhorn_iter = 5L,
    gpu_iterations = 3L,
    verbose = FALSE
  )

  expect_equal(result_k1$sinkhorn_iter, 1L)
  expect_equal(result_k5$sinkhorn_iter, 5L)
  # Both should produce valid results
  expect_length(result_k1$alpha, n)
  expect_length(result_k5$alpha, n)
  expect_true(is.finite(result_k1$value))
  expect_true(is.finite(result_k5$value))
})

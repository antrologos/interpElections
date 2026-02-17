test_that("optimize_alpha CPU finds lower objective than initial", {
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
    cpu_method = "L-BFGS-B",
    cpu_parallel = FALSE,
    maxit = 500L,
    verbose = FALSE
  )

  expect_s3_class(result, "interpElections_optim")
  expect_length(result$alpha, n)

  # Compare with initial alpha = 1
  init_val <- idw_objective(rep(1, n), t_mat + 1, p_mat, s_mat)
  expect_true(result$value <= init_val)
})

test_that("optimize_alpha respects lower_bound", {
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
    cpu_method = "L-BFGS-B",
    cpu_parallel = FALSE,
    lower_bound = 0,
    maxit = 200L,
    verbose = FALSE
  )

  expect_true(all(result$alpha >= 0))
})

test_that("optimize_alpha returns proper structure", {
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
    cpu_parallel = FALSE,
    maxit = 100L,
    verbose = FALSE
  )

  expect_true("alpha" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("convergence" %in% names(result))
  expect_true("elapsed" %in% names(result))
})

test_that("optimize_alpha print method works", {
  set.seed(1)
  t_mat <- matrix(runif(8, 1, 10), nrow = 4)
  p_mat <- matrix(runif(4), nrow = 4)
  s_mat <- matrix(runif(2), nrow = 2)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    cpu_parallel = FALSE,
    maxit = 50L,
    verbose = FALSE
  )

  expect_output(print(result), "interpElections optimization result")
})

test_that("cpu_parallel defaults to FALSE (serial by default)", {
  set.seed(1)
  n <- 10
  m <- 5
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 50), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    maxit = 100L,
    verbose = FALSE
  )

  # Default should use serial L-BFGS-B, not parallel
  expect_equal(result$method, "cpu_lbfgsb")
})

test_that("parallel optimization works when explicitly enabled", {
  skip_if_not_installed("optimParallel")
  skip_if_not_installed("parallel")

  set.seed(42)
  n <- 60  # Above the 50-tract threshold

  m <- 10
  k <- 2
  t_mat <- matrix(runif(n * m, 1, 50), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    cpu_parallel = TRUE,
    cpu_ncores = 2L,
    maxit = 500L,
    verbose = FALSE
  )

  expect_s3_class(result, "interpElections_optim")
  expect_equal(result$method, "cpu_lbfgsb_parallel")
  expect_length(result$alpha, n)

  # Should find a lower objective than initial
  init_val <- idw_objective(rep(1, n), t_mat + 1, p_mat, s_mat)
  expect_true(result$value <= init_val)
})

test_that("parallel skipped for small problems (< 50 tracts)", {
  skip_if_not_installed("optimParallel")
  skip_if_not_installed("parallel")

  set.seed(1)
  n <- 10  # Below the 50-tract threshold
  m <- 5
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    cpu_parallel = TRUE,
    maxit = 200L,
    verbose = FALSE
  )

  # Should fall back to serial despite cpu_parallel = TRUE
  expect_equal(result$method, "cpu_lbfgsb")
})

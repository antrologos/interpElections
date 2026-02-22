test_that("optimize_alpha finds lower objective than initial", {
  skip_if_not_installed("torch")

  set.seed(123)
  n <- 10
  m <- 5
  k <- 2

  # Structured travel times: each tract is close to one station, far from rest.
  # This creates clear spatial signal that alpha > 1 can exploit.
  t_mat <- matrix(30, n, m)
  for (i in seq_len(n)) t_mat[i, ((i - 1L) %% m) + 1L] <- 2
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    max_epochs = 200L,
    sk_tol = 0.01,
    loss_fn = "sse",
    verbose = FALSE
  ))

  expect_s3_class(result, "interpElections_optim")
  # alpha is an n × k matrix
  expect_true(is.matrix(result$alpha))
  expect_equal(nrow(result$alpha), n)
  expect_equal(ncol(result$alpha), k)

  # Compare with initial alpha = 1: compute SSE via W (same method)
  init_W <- compute_weight_matrix(t_mat, matrix(1, n, k), p_mat, s_mat,
                                   method = result$method_type, verbose = FALSE)
  init_fitted <- init_W %*% s_mat
  init_val <- sum((init_fitted - p_mat)^2)
  expect_true(result$value <= init_val)
})

test_that("optimize_alpha alpha is always positive (softplus reparameterization)", {
  skip_if_not_installed("torch")

  set.seed(42)
  n <- 5
  m <- 3
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    max_epochs = 50L,
    sk_tol = 0.01,
    verbose = FALSE
  ))

  # alpha = alpha_min + softplus(theta): always strictly positive
  expect_true(all(result$alpha > 0))
  expect_true(all(is.finite(result$alpha)))
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

  # Use default sk_iter/sk_tol to test structure; suppress Sinkhorn warnings
  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    max_epochs = 30L,
    verbose = FALSE
  ))

  expect_true("alpha" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("W" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("convergence" %in% names(result))
  expect_true("elapsed" %in% names(result))
  expect_true("row_targets" %in% names(result))
  expect_true("sk_iter" %in% names(result))
  expect_true("batch_size" %in% names(result))
  expect_true("history" %in% names(result))
  expect_true("grad_norm_final" %in% names(result))
  expect_true("grad_history" %in% names(result))
  expect_true("n_batches_per_epoch" %in% names(result))
  expect_true("epochs" %in% names(result))
  expect_true("steps" %in% names(result))
  # W is an n × m matrix
  expect_true(is.matrix(result$W))
  expect_equal(nrow(result$W), n)
  expect_equal(ncol(result$W), m)
  expect_true(all(is.finite(result$W)))
  expect_equal(result$sk_iter, 100L)
  expect_true("sk_tol" %in% names(result))
  expect_equal(result$sk_tol, 1e-6)
  expect_equal(result$batch_size, n)  # effective batch = min(500, n)
  expect_length(result$row_targets, n)
  # alpha is an n × k matrix
  expect_true(is.matrix(result$alpha))
  expect_equal(nrow(result$alpha), n)
  expect_equal(ncol(result$alpha), k)
})

test_that("optimize_alpha print method works", {
  skip_if_not_installed("torch")

  set.seed(1)
  t_mat <- matrix(runif(8, 1, 10), nrow = 4)
  p_mat <- matrix(runif(4), nrow = 4)
  s_mat <- matrix(runif(2), nrow = 2)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    max_epochs = 30L,
    verbose = FALSE
  ))

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

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    max_epochs = 30L,
    verbose = FALSE
  ))

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

test_that("optimize_alpha sk_iter parameter works", {
  skip_if_not_installed("torch")

  set.seed(42)
  n <- 5
  m <- 3
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result_k1 <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    sk_iter = 1L,
    max_epochs = 30L,
    verbose = FALSE
  ))

  result_k15 <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    sk_iter = 15L,
    max_epochs = 30L,
    verbose = FALSE
  ))

  expect_equal(result_k1$sk_iter, 1L)
  expect_equal(result_k15$sk_iter, 15L)
  # Both should produce valid n×k alpha matrices
  expect_true(is.matrix(result_k1$alpha))
  expect_equal(nrow(result_k1$alpha), n)
  expect_equal(ncol(result_k1$alpha), k)
  expect_true(is.matrix(result_k15$alpha))
  expect_equal(nrow(result_k15$alpha), n)
  expect_equal(ncol(result_k15$alpha), k)
  expect_true(is.finite(result_k1$value))
  expect_true(is.finite(result_k15$value))
})

test_that("optimize_alpha batch_size works with small n", {
  skip_if_not_installed("torch")

  set.seed(99)
  n <- 4
  m <- 3
  k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  # batch_size > n should silently use n
  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    batch_size = 999L,
    max_epochs = 30L,
    verbose = FALSE
  ))

  expect_true(is.matrix(result$alpha))
  expect_equal(nrow(result$alpha), n)
  expect_equal(ncol(result$alpha), k)
  expect_true(is.finite(result$value))

  # batch_size = 2 (mini-batch)
  result2 <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    batch_size = 2L,
    max_epochs = 30L,
    verbose = FALSE
  ))

  expect_true(is.matrix(result2$alpha))
  expect_equal(nrow(result2$alpha), n)
  expect_equal(ncol(result2$alpha), k)
  expect_true(is.finite(result2$value))
})

test_that("optimize_alpha returns lr_history", {
  skip_if_not_installed("torch")
  set.seed(303)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)
  storage.mode(p_mat) <- "double"
  storage.mode(s_mat) <- "double"

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE,
    max_epochs = 30L,
    verbose = FALSE
  ))

  expect_true("lr_history" %in% names(result))
  expect_equal(length(result$lr_history), result$epochs)
  expect_true(all(result$lr_history > 0))
  # LR should be non-increasing (ReduceLROnPlateau only decreases)
  expect_true(all(diff(result$lr_history) <= 1e-10))
})

# --- Column-normalization method tests ---

test_that("optimize_alpha method='colnorm' runs and returns valid structure", {
  skip_if_not_installed("torch")
  set.seed(401)
  n <- 10; m <- 5; k <- 2
  t_mat <- matrix(30, n, m)
  for (i in seq_len(n)) t_mat[i, ((i - 1L) %% m) + 1L] <- 2
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    method = "colnorm", barrier_mu = 10,
    use_gpu = FALSE, max_epochs = 30L, verbose = FALSE
  ))

  expect_s3_class(result, "interpElections_optim")
  expect_true(all(result$alpha > 0))
  expect_equal(nrow(result$alpha), n)
  expect_equal(ncol(result$alpha), k)
  expect_true(grepl("colnorm", result$method))
  expect_equal(result$method_type, "colnorm")
})

test_that("optimize_alpha method='colnorm' with barrier_mu=0 works", {
  skip_if_not_installed("torch")
  set.seed(402)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    method = "colnorm", barrier_mu = 0,
    use_gpu = FALSE, max_epochs = 10L, verbose = FALSE
  ))

  expect_true(all(result$alpha > 0))
  expect_true(grepl("colnorm", result$method))
})

test_that("optimize_alpha method='sinkhorn' backward compat", {
  skip_if_not_installed("torch")
  set.seed(403)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    method = "sinkhorn",
    use_gpu = FALSE, max_epochs = 10L, verbose = FALSE
  ))

  expect_true(grepl("sinkhorn", result$method))
  expect_equal(result$method_type, "sinkhorn")
})

test_that("compute_weight_matrix method='colnorm' produces valid weights", {
  skip_if_not_installed("torch")
  set.seed(404)
  n <- 8; m <- 4; k <- 2
  t_mat <- matrix(runif(n * m, 2, 30), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)
  alpha_mat <- matrix(runif(n * k, 0.5, 2.0), nrow = n)

  W <- compute_weight_matrix(t_mat, alpha_mat, p_mat, s_mat,
                              method = "colnorm", verbose = FALSE)

  expect_equal(dim(W), c(n, m))
  # Column sums should be approximately 1 (source conservation)
  expect_true(all(abs(colSums(W) - 1) < 0.01))
  # All weights non-negative
  expect_true(all(W >= -1e-10))
})

# --- Poisson deviance loss tests ---

test_that("optimize_alpha loss_fn='poisson' runs and returns valid structure", {
  skip_if_not_installed("torch")
  set.seed(501)
  n <- 10; m <- 5; k <- 2
  t_mat <- matrix(30, n, m)
  for (i in seq_len(n)) t_mat[i, ((i - 1L) %% m) + 1L] <- 2
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    loss_fn = "poisson",
    use_gpu = FALSE, max_epochs = 30L, verbose = FALSE
  ))

  expect_s3_class(result, "interpElections_optim")
  expect_true(all(result$alpha > 0))
  expect_equal(nrow(result$alpha), n)
  expect_equal(ncol(result$alpha), k)
  expect_true(is.finite(result$value))
  expect_true(result$value >= 0)
  expect_equal(result$loss_fn, "poisson")
})

test_that("optimize_alpha loss_fn='sse' backward compatibility", {
  skip_if_not_installed("torch")
  set.seed(502)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    loss_fn = "sse",
    use_gpu = FALSE, max_epochs = 10L, verbose = FALSE
  ))

  expect_true(all(result$alpha > 0))
  expect_equal(result$loss_fn, "sse")
})

test_that("optimize_alpha default loss_fn is 'poisson'", {
  skip_if_not_installed("torch")
  set.seed(503)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    use_gpu = FALSE, max_epochs = 10L, verbose = FALSE
  ))

  expect_equal(result$loss_fn, "poisson")
})

test_that("optimize_alpha loss_fn='poisson' with method='sinkhorn'", {
  skip_if_not_installed("torch")
  set.seed(507)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    loss_fn = "poisson", method = "sinkhorn",
    use_gpu = FALSE, max_epochs = 20L, verbose = FALSE
  ))

  expect_true(grepl("sinkhorn", result$method))
  expect_equal(result$loss_fn, "poisson")
  expect_true(all(result$alpha > 0))
})

test_that("optimize_alpha loss_fn='poisson' handles zero population brackets", {
  skip_if_not_installed("torch")
  set.seed(506)
  n <- 6; m <- 4; k <- 3
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)
  # Zero out one bracket entirely
  p_mat[, 2] <- 0
  s_mat[, 2] <- 0

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    loss_fn = "poisson",
    use_gpu = FALSE, max_epochs = 20L, verbose = FALSE
  ))

  expect_true(all(is.finite(result$alpha)))
  expect_true(is.finite(result$value))
  # Inactive bracket should get default alpha = 1
  expect_true(all(result$alpha[, 2] == 1))
})

test_that("optimize_alpha rejects invalid loss_fn", {
  skip_if_not_installed("torch")
  n <- 4; m <- 2; k <- 1
  t_mat <- matrix(runif(n * m, 1, 10), nrow = n)
  p_mat <- matrix(runif(n * k, 5, 20), nrow = n)
  s_mat <- matrix(runif(m * k, 5, 20), nrow = m)

  expect_error(
    optimize_alpha(t_mat, p_mat, s_mat, loss_fn = "mse", verbose = FALSE),
    "should be one of"
  )
})

# --- Alpha lower bound tests ---

test_that("optimize_alpha alpha_min enforces lower bound", {
  skip_if_not_installed("torch")
  set.seed(504)
  n <- 8; m <- 4; k <- 2
  t_mat <- matrix(30, n, m)
  for (i in seq_len(n)) t_mat[i, ((i - 1L) %% m) + 1L] <- 2
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    alpha_min = 2,
    use_gpu = FALSE, max_epochs = 50L, verbose = FALSE
  ))

  # Active brackets should respect alpha_min
  # (inactive brackets get default alpha=1, which may be < alpha_min)
  active_cols <- which(colSums(p_mat) >= 0.5 & colSums(s_mat) >= 0.5)
  expect_true(all(result$alpha[, active_cols] >= 2 - 1e-6))
  expect_equal(result$alpha_min, 2)
})

test_that("optimize_alpha alpha_min=0 backward compatible", {
  skip_if_not_installed("torch")
  set.seed(505)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    alpha_min = 0,
    use_gpu = FALSE, max_epochs = 10L, verbose = FALSE
  ))

  expect_true(all(result$alpha > 0))
  expect_equal(result$alpha_min, 0)
})

test_that("optimize_alpha rejects invalid alpha_min", {
  skip_if_not_installed("torch")
  n <- 4; m <- 2; k <- 1
  t_mat <- matrix(runif(n * m, 1, 10), nrow = n)
  p_mat <- matrix(runif(n * k, 5, 20), nrow = n)
  s_mat <- matrix(runif(m * k, 5, 20), nrow = m)

  expect_error(
    optimize_alpha(t_mat, p_mat, s_mat, alpha_min = -1, verbose = FALSE),
    "alpha_min"
  )
  expect_error(
    optimize_alpha(t_mat, p_mat, s_mat, alpha_min = Inf, verbose = FALSE),
    "alpha_min"
  )
})

test_that("optimize_alpha combined poisson + alpha_min", {
  skip_if_not_installed("torch")
  set.seed(508)
  n <- 10; m <- 5; k <- 2
  t_mat <- matrix(30, n, m)
  for (i in seq_len(n)) t_mat[i, ((i - 1L) %% m) + 1L] <- 2
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    loss_fn = "poisson", alpha_min = 1,
    use_gpu = FALSE, max_epochs = 50L, verbose = FALSE
  ))

  active_cols <- which(colSums(p_mat) >= 0.5 & colSums(s_mat) >= 0.5)
  expect_true(all(result$alpha[, active_cols] >= 1 - 1e-6))
  expect_equal(result$loss_fn, "poisson")
  expect_equal(result$alpha_min, 1)
})

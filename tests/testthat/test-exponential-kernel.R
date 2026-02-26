# ===========================================================
# Tests for exponential kernel option
# ===========================================================

# --- optim_control kernel parameter ---

test_that("optim_control accepts kernel = 'exponential'", {
  ctrl <- optim_control(kernel = "exponential")
  expect_equal(ctrl$kernel, "exponential")
  expect_equal(ctrl$alpha_min, 0)
  expect_equal(ctrl$alpha_init, 2)
})

test_that("optim_control defaults kernel to 'power'", {
  ctrl <- optim_control()
  expect_equal(ctrl$kernel, "power")
  expect_equal(ctrl$alpha_min, 1)
  expect_equal(ctrl$alpha_init, 2)
})

test_that("optim_control rejects invalid kernel", {
  expect_error(optim_control(kernel = "gaussian"), "power.*exponential")
})

test_that("optim_control exponential with explicit alpha_min", {
  ctrl <- optim_control(kernel = "exponential", alpha_min = 0.01)
  expect_equal(ctrl$alpha_min, 0.01)
  expect_equal(ctrl$alpha_init, 2)
})

test_that("optim_control exponential with explicit alpha_init", {
  ctrl <- optim_control(kernel = "exponential", alpha_init = 0.1)
  expect_equal(ctrl$alpha_init, 0.1)
})

test_that("optim_control prints kernel field", {
  ctrl <- optim_control(kernel = "exponential")
  out <- capture.output(print(ctrl))
  expect_true(any(grepl("kernel.*exponential", out)))
})


# --- compute_weight_matrix with exponential kernel ---

test_that("compute_weight_matrix exponential produces valid W", {
  skip_if_not_installed("torch")
  set.seed(77)
  n <- 5; m <- 3; k <- 2
  tt <- matrix(runif(n * m, 1, 60), nrow = n)
  pop <- matrix(rpois(n * k, 60) + 1, nrow = n)
  src <- matrix(rpois(m * k, 120) + 1, nrow = m)
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(0.05, n, k)

  W <- compute_weight_matrix(tt, alpha_mat, pop, src,
                              kernel = "exponential", verbose = FALSE)
  expect_true(is.matrix(W))
  expect_equal(dim(W), c(n, m))
  expect_true(all(W >= 0))
  expect_equal(colSums(W), rep(1, m), tolerance = 0.05)
})

test_that("compute_weight_matrix exponential ignores offset", {
  skip_if_not_installed("torch")
  set.seed(88)
  n <- 4; m <- 3; k <- 2
  tt <- matrix(runif(n * m, 1, 30), nrow = n)
  pop <- matrix(rpois(n * k, 50) + 1, nrow = n)
  src <- matrix(rpois(m * k, 80) + 1, nrow = m)
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(0.05, n, k)

  W1 <- compute_weight_matrix(tt, alpha_mat, pop, src,
                                offset = 0, kernel = "exponential",
                                verbose = FALSE)
  W2 <- compute_weight_matrix(tt, alpha_mat, pop, src,
                                offset = 5, kernel = "exponential",
                                verbose = FALSE)
  expect_equal(W1, W2, tolerance = 1e-6)
})

test_that("compute_weight_matrix exponential allows t=0", {
  skip_if_not_installed("torch")
  set.seed(99)
  n <- 4; m <- 3; k <- 2
  tt <- matrix(runif(n * m, 0, 30), nrow = n)
  tt[1, 1] <- 0  # explicit zero
  pop <- matrix(rpois(n * k, 50) + 1, nrow = n)
  src <- matrix(rpois(m * k, 80) + 1, nrow = m)
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(0.05, n, k)

  W <- compute_weight_matrix(tt, alpha_mat, pop, src,
                              kernel = "exponential", verbose = FALSE)
  expect_true(all(is.finite(W)))
})

test_that("compute_weight_matrix exponential rejects negative t", {
  skip_if_not_installed("torch")
  n <- 3; m <- 2; k <- 1
  tt <- matrix(c(1, -1, 2, 3, 4, 5), nrow = n)
  pop <- matrix(10, n, k)
  src <- matrix(10, m, k)
  alpha_mat <- matrix(0.05, n, k)

  expect_error(
    compute_weight_matrix(tt, alpha_mat, pop, src,
                          kernel = "exponential", verbose = FALSE),
    "non-negative"
  )
})


# --- optimize_alpha with exponential kernel ---

test_that("optimize_alpha exponential finds finite objective", {
  skip_if_not_installed("torch")
  set.seed(123)
  n <- 10; m <- 5; k <- 2
  t_mat <- matrix(30, n, m)
  for (i in seq_len(n)) t_mat[i, ((i - 1L) %% m) + 1L] <- 2
  p_mat <- matrix(runif(n * k, 10, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 100), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    optim = optim_control(kernel = "exponential", max_epochs = 50L),
    offset = 0,
    verbose = FALSE
  ))

  expect_s3_class(result, "interpElections_optim")
  expect_true(is.finite(result$value))
  expect_true(all(result$alpha >= -1e-6))
  expect_equal(result$kernel, "exponential")
})

test_that("optimize_alpha exponential alpha >= alpha_min", {
  skip_if_not_installed("torch")
  set.seed(42)
  n <- 5; m <- 3; k <- 1
  t_mat <- matrix(runif(n * m, 1, 20), nrow = n)
  p_mat <- matrix(runif(n * k, 10, 50), nrow = n)
  s_mat <- matrix(runif(m * k, 10, 50), nrow = m)

  result <- suppressWarnings(optimize_alpha(
    t_mat, p_mat, s_mat,
    optim = optim_control(kernel = "exponential", max_epochs = 30L),
    offset = 0, verbose = FALSE
  ))

  # alpha_min = 0 for exponential, so alpha >= 0
  expect_true(all(result$alpha >= -1e-6))
})


# --- W from optimize_alpha matches compute_weight_matrix ---

test_that("exponential W from optimize matches compute_weight_matrix", {
  skip_if_not_installed("torch")
  set.seed(2)
  n <- 6; m <- 4; k <- 2
  t_mat <- matrix(runif(n * m, 1, 30), nrow = n)
  pop <- matrix(rpois(n * k, 80) + 1, nrow = n)
  src <- matrix(rpois(m * k, 150) + 1, nrow = m)
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"

  result <- suppressWarnings(optimize_alpha(
    t_mat, pop, src,
    optim = optim_control(kernel = "exponential", max_epochs = 30L),
    offset = 0, verbose = FALSE
  ))

  W_recomputed <- compute_weight_matrix(
    t_mat, result$alpha, pop, src,
    offset = 0, kernel = "exponential", verbose = FALSE
  )

  expect_equal(result$W, W_recomputed, tolerance = 0.01)
})


# --- Backward compatibility ---

test_that("power kernel behavior unchanged with default settings", {
  skip_if_not_installed("torch")
  set.seed(11)
  n <- 5; m <- 3; k <- 2
  tt <- matrix(runif(n * m, 1, 20), nrow = n)
  pop <- matrix(rpois(n * k, 60) + 1, nrow = n)
  src <- matrix(rpois(m * k, 120) + 1, nrow = m)
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(1, n, k)

  W_default <- compute_weight_matrix(tt, alpha_mat, pop, src,
                                      verbose = FALSE)
  W_explicit <- compute_weight_matrix(tt, alpha_mat, pop, src,
                                       kernel = "power", verbose = FALSE)

  expect_equal(W_default, W_explicit)
})

test_that("optim_control NULL alpha_min resolves to power defaults", {
  ctrl <- optim_control()
  expect_equal(ctrl$alpha_min, 1)
  expect_equal(ctrl$alpha_init, 2)
  expect_equal(ctrl$kernel, "power")
})

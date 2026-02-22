# ===========================================================
# Tests for compute_weight_matrix()
# ===========================================================

test_that("compute_weight_matrix: produces n×m matrix with colSums ≈ 1", {
  skip_if_not_installed("torch")

  set.seed(11)
  n <- 5; m <- 3; k <- 2
  tt  <- matrix(runif(n * m, 1, 20), nrow = n)
  pop <- matrix(rpois(n * k, 60), nrow = n, ncol = k) + 1
  src <- matrix(rpois(m * k, 120), nrow = m, ncol = k) + 1
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(1, nrow = n, ncol = k)

  W <- compute_weight_matrix(tt, alpha_mat, pop, src, verbose = FALSE)

  expect_true(is.matrix(W))
  expect_equal(dim(W), c(n, m))
  expect_true(all(W >= 0))
  expect_equal(colSums(W), rep(1, m), tolerance = 0.05)
})

test_that("compute_weight_matrix: approximately satisfies Property 2 (population proportions)", {
  skip_if_not_installed("torch")

  set.seed(33)
  n <- 6; m <- 4; k <- 2
  tt  <- matrix(runif(n * m, 1, 30), nrow = n)
  pop <- matrix(rpois(n * k, 80), nrow = n, ncol = k) + 1
  src <- matrix(rpois(m * k, 150), nrow = m, ncol = k) + 1
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(1, nrow = n, ncol = k)

  W <- compute_weight_matrix(tt, alpha_mat, pop, src,
                              method = "sinkhorn", verbose = FALSE)

  c_agg   <- rowSums(src)
  V_total <- sum(src)
  T_i     <- rowSums(sweep(W, 2L, c_agg, `*`))
  r_norm  <- rowSums(pop) / sum(pop)
  expect_equal(T_i / V_total, r_norm, tolerance = 0.05)
})

test_that("compute_weight_matrix: preserves dimnames", {
  skip_if_not_installed("torch")

  set.seed(44)
  n <- 3; m <- 2; k <- 2
  tt <- matrix(runif(n * m, 1, 10), nrow = n,
               dimnames = list(c("A","B","C"), c("s1","s2")))
  pop <- matrix(rpois(n * k, 50) + 1, nrow = n)
  src <- matrix(rpois(m * k, 100) + 1, nrow = m)
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(1, n, k)

  W <- compute_weight_matrix(tt, alpha_mat, pop, src, verbose = FALSE)

  expect_equal(rownames(W), c("A", "B", "C"))
  expect_equal(colnames(W), c("s1", "s2"))
})

test_that("compute_weight_matrix: skips empty brackets", {
  skip_if_not_installed("torch")

  set.seed(55)
  n <- 4; m <- 3; k <- 3
  tt  <- matrix(runif(n * m, 1, 20), nrow = n)
  pop <- matrix(rpois(n * k, 50) + 1, nrow = n, ncol = k)
  src <- matrix(rpois(m * k, 80) + 1, nrow = m, ncol = k)
  pop[, 2] <- 0; src[, 2] <- 0   # bracket 2 empty
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(1, n, k)

  W <- compute_weight_matrix(tt, alpha_mat, pop, src, verbose = FALSE)
  expect_true(is.matrix(W))
  expect_equal(dim(W), c(n, m))
})

test_that("compute_weight_matrix: input validation", {
  skip_if_not_installed("torch")

  n <- 3; m <- 2; k <- 2
  tt  <- matrix(1:6, n, m)
  pop <- matrix(1:6, n, k)
  src <- matrix(1:4, m, k)
  alpha <- matrix(1, n, k)
  storage.mode(tt) <- "double"
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"

  # Not a matrix
  expect_error(
    compute_weight_matrix("a", alpha, pop, src),
    "numeric matrix"
  )
  # Wrong alpha rows
  expect_error(
    compute_weight_matrix(tt, matrix(1, n + 1, k), pop, src),
    "alpha has"
  )
  # Wrong pop rows
  expect_error(
    compute_weight_matrix(tt, alpha, matrix(1, n + 1, k), src),
    "pop_matrix has"
  )
  # Wrong source rows
  expect_error(
    compute_weight_matrix(tt, alpha, pop, matrix(1, m + 1, k)),
    "source_matrix has"
  )
  # Mismatched k
  expect_error(
    compute_weight_matrix(tt, alpha, pop, matrix(1, m, k + 1)),
    "source_matrix has"
  )
})

test_that("compute_weight_matrix: custom offset works", {
  skip_if_not_installed("torch")

  set.seed(99)
  n <- 4; m <- 3; k <- 2
  tt  <- matrix(runif(n * m, 0, 20), nrow = n)
  pop <- matrix(rpois(n * k, 50) + 1, nrow = n)
  src <- matrix(rpois(m * k, 80) + 1, nrow = m)
  storage.mode(pop) <- "double"
  storage.mode(src) <- "double"
  alpha_mat <- matrix(1, n, k)

  # offset = 0 requires all positive time_matrix
  tt_pos <- tt + 1

  W1 <- compute_weight_matrix(tt_pos, alpha_mat, pop, src,
                               offset = 0, verbose = FALSE)
  W2 <- compute_weight_matrix(tt, alpha_mat, pop, src,
                               offset = 1, verbose = FALSE)

  # Both should produce the same result (tt+1 with offset=0 == tt with offset=1)
  expect_equal(W1, W2, tolerance = 1e-4)
})

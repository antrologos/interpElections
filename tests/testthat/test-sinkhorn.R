# ===========================================================
# Tests for Sinkhorn/IPF weight balancing
# ===========================================================

# --- sinkhorn_balance() ---

test_that("sinkhorn_balance: row sums match targets", {
  W <- matrix(c(3, 1, 0.5, 2, 4, 1.5, 1, 2, 3), nrow = 3)
  row_targets <- c(2, 3, 2)
  col_targets <- c(2, 2, 3)  # sum = 7 = sum(row_targets)
  balanced <- sinkhorn_balance(W, row_targets, col_targets)
  expect_equal(rowSums(balanced), row_targets, tolerance = 1e-8)
})

test_that("sinkhorn_balance: column sums match targets", {
  W <- matrix(c(3, 1, 0.5, 2, 4, 1.5, 1, 2, 3), nrow = 3)
  row_targets <- c(2, 3, 2)
  col_targets <- c(2, 2, 3)
  balanced <- sinkhorn_balance(W, row_targets, col_targets)
  expect_equal(colSums(balanced), col_targets, tolerance = 1e-8)
})

test_that("sinkhorn_balance: non-negativity preserved", {
  W <- matrix(c(3, 0, 0.5, 2, 4, 0, 1, 2, 3), nrow = 3)
  balanced <- sinkhorn_balance(W, row_targets = c(1, 1, 1),
                                col_targets = c(1, 1, 1))
  expect_true(all(balanced >= 0))
})

test_that("sinkhorn_balance: zero structure preserved", {
  W <- matrix(c(3, 0, 0.5, 2, 4, 0, 1, 2, 3), nrow = 3)
  zeros_before <- W == 0
  balanced <- sinkhorn_balance(W, row_targets = c(1, 1, 1),
                                col_targets = c(1, 1, 1))
  # Where W was zero, result should also be zero
  expect_true(all(balanced[zeros_before] == 0))
})

test_that("sinkhorn_balance: converges for well-posed input", {
  W <- matrix(c(3, 1, 2, 4, 1, 2), nrow = 2)
  balanced <- sinkhorn_balance(W, row_targets = c(1.5, 1.5),
                                col_targets = c(1, 1, 1))
  expect_true(attr(balanced, "converged"))
  expect_true(attr(balanced, "iterations") > 0)
})

test_that("sinkhorn_balance: unreachable rows stay zero and warn", {
  W <- matrix(c(3, 0, 1, 0, 2, 0), nrow = 2)  # row 2 all zeros
  expect_warning(
    balanced <- sinkhorn_balance(W, row_targets = c(1, 1),
                                  col_targets = c(1, 1, 1)),
    "all-zero"
  )
  expect_equal(balanced[2, ], c(0, 0, 0))
  expect_equal(attr(balanced, "unreachable"), 2L)
})

test_that("sinkhorn_balance: single row works", {
  W <- matrix(c(2, 3, 5), nrow = 1)
  balanced <- sinkhorn_balance(W, row_targets = 3, col_targets = c(1, 1, 1))
  expect_equal(rowSums(balanced), 3, tolerance = 1e-8)
  expect_equal(colSums(balanced), c(1, 1, 1), tolerance = 1e-8)
})

test_that("sinkhorn_balance: target sum mismatch warns", {
  W <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_warning(
    sinkhorn_balance(W, row_targets = c(1, 1), col_targets = c(5, 5)),
    "differs from"
  )
})

test_that("sinkhorn_balance: non-convergence with max_iter=1 warns", {
  W <- matrix(runif(100), nrow = 10)
  row_targets <- rep(1, 10)
  col_targets <- rep(1, 10)
  expect_warning(
    result <- sinkhorn_balance(W, row_targets, col_targets, max_iter = 1),
    "did not converge"
  )
  expect_false(attr(result, "converged"))
})

test_that("sinkhorn_balance: input validation", {
  # Not a matrix
  expect_error(sinkhorn_balance("a"), "numeric matrix")
  # Empty matrix
  expect_error(sinkhorn_balance(matrix(numeric(0), nrow = 0, ncol = 0)),
               "must not be empty")
  # NA values
  expect_error(sinkhorn_balance(matrix(c(1, NA, 3, 4), nrow = 2)),
               "NA, NaN, or Inf")
  # Negative values
  expect_error(sinkhorn_balance(matrix(c(1, -1, 3, 4), nrow = 2)),
               "non-negative")
  # Wrong row_targets length
  expect_error(sinkhorn_balance(matrix(1:4, nrow = 2), row_targets = 1),
               "length 2")
  # Wrong col_targets length
  expect_error(sinkhorn_balance(matrix(1:4, nrow = 2), col_targets = 1),
               "length 2")
  # Negative targets
  expect_error(sinkhorn_balance(matrix(1:4, nrow = 2),
                                 row_targets = c(-1, 1)),
               "non-negative")
})


# --- sinkhorn_weights() ---

test_that("sinkhorn_weights: column sums approximately 1", {
  tt <- matrix(c(2, 5, 10, 3, 1, 8, 7, 4, 2), nrow = 3)
  alpha <- c(1, 1.5, 2)
  W <- sinkhorn_weights(tt, alpha)
  expect_equal(colSums(W), rep(1, 3), tolerance = 1e-8)
})

test_that("sinkhorn_weights: row sums match population-proportional targets", {
  tt <- matrix(c(2, 5, 10, 3, 1, 8, 7, 4, 2), nrow = 3)
  alpha <- c(1, 1.5, 2)
  pop <- c(100, 200, 300)
  m <- ncol(tt)
  r <- pop / sum(pop) * m
  W <- sinkhorn_weights(tt, alpha, row_targets = r)
  expect_equal(rowSums(W), r, tolerance = 1e-8)
})

test_that("sinkhorn_weights: remote tract gets population-proportional weight", {
  # Tract 3 is far from all sources (10, 8, 2) vs tract 1 (2, 3, 7)
  tt <- matrix(c(2, 5, 10, 3, 1, 8, 7, 4, 2), nrow = 3)
  alpha <- rep(2, 3)
  pop <- c(100, 100, 100)
  m <- ncol(tt)
  r <- pop / sum(pop) * m

  # With Sinkhorn, remote tract gets population-proportional weight
  W_sk <- sinkhorn_weights(tt, alpha, row_targets = r)

  # Sinkhorn weight sum for tract 3 should be close to target
  expect_equal(sum(W_sk[3, ]), r[3], tolerance = 1e-8)
  # All row sums should match targets
  expect_equal(rowSums(W_sk), r, tolerance = 1e-8)
})

test_that("sinkhorn_weights: preserves dimnames", {
  tt <- matrix(c(2, 5, 3, 1, 7, 4), nrow = 2,
               dimnames = list(c("A", "B"), c("s1", "s2", "s3")))
  W <- sinkhorn_weights(tt, alpha = c(1, 1))
  expect_equal(rownames(W), c("A", "B"))
  expect_equal(colnames(W), c("s1", "s2", "s3"))
})

test_that("sinkhorn_weights: same validation as idw_weights", {
  expect_error(sinkhorn_weights("a", 1), "numeric matrix")
  expect_error(sinkhorn_weights(matrix(1:4, nrow = 2), c(1, 1, 1)),
               "alpha has length")
  expect_error(sinkhorn_weights(matrix(1:4, nrow = 2), c(-1, 1)),
               "non-negative")
})


# --- sinkhorn_objective() ---

test_that("sinkhorn_objective: returns scalar, non-negative", {
  t_mat <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3)
  p_mat <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 3)
  s_mat <- matrix(c(50, 60, 70, 80), nrow = 2)
  alpha <- c(1, 1, 1)
  row_targets <- rep(ncol(t_mat) / nrow(t_mat), nrow(t_mat))

  val <- sinkhorn_objective(alpha, t_mat, p_mat, s_mat, row_targets)
  expect_type(val, "double")
  expect_length(val, 1)
  expect_true(val >= 0)
})

test_that("sinkhorn_objective: zero when interpolation is perfect", {
  # 2 tracts, 1 source, equal weights
  t_mat <- matrix(c(1, 1), nrow = 2)
  alpha <- c(1, 1)
  s_mat <- matrix(c(20), nrow = 1)
  p_mat <- matrix(c(10, 10), nrow = 2)
  row_targets <- c(0.5, 0.5)  # sum = 1 = col_target

  val <- sinkhorn_objective(alpha, t_mat, p_mat, s_mat, row_targets)
  expect_equal(val, 0, tolerance = 1e-6)
})

test_that("sinkhorn_objective: higher alpha yields different objective", {
  tt <- matrix(c(2, 5, 10, 3, 1, 8, 7, 4, 2), nrow = 3)
  p_mat <- matrix(c(30, 30, 30), nrow = 3)
  s_mat <- matrix(c(30, 30, 30), nrow = 3)
  row_targets <- rep(1, 3)

  val1 <- sinkhorn_objective(c(1, 1, 1), tt, p_mat, s_mat, row_targets)
  val2 <- sinkhorn_objective(c(5, 5, 5), tt, p_mat, s_mat, row_targets)

  # Different alphas generally produce different objectives
  expect_false(isTRUE(all.equal(val1, val2)))
})

test_that("sinkhorn_objective: rejects dimension mismatches", {
  t_mat <- matrix(1:6, nrow = 3, ncol = 2)
  p_mat <- matrix(1:6, nrow = 3, ncol = 2)
  s_mat <- matrix(1:4, nrow = 2, ncol = 2)
  row_targets <- rep(1, 3)

  # Wrong alpha length
  expect_error(
    sinkhorn_objective(c(1, 1), t_mat, p_mat, s_mat, row_targets),
    "alpha has length"
  )
  # Wrong row_targets length
  expect_error(
    sinkhorn_objective(c(1, 1, 1), t_mat, p_mat, s_mat, c(1, 1)),
    "row_targets"
  )
})

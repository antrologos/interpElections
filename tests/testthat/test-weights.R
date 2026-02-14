test_that("idw_weights columns sum to 1", {
  t_mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
  alpha <- c(1, 1.5, 2)
  W <- idw_weights(t_mat, alpha)
  expect_equal(colSums(W), rep(1, ncol(W)), tolerance = 1e-10)
})

test_that("idw_weights returns correct dimensions", {
  t_mat <- matrix(runif(20), nrow = 5, ncol = 4)
  alpha <- rep(1, 5)
  W <- idw_weights(t_mat, alpha)
  expect_equal(dim(W), c(5, 4))
})

test_that("idw_weights with higher alpha gives more weight to closer zones", {
  # Zone 1 is closer to point 1, zone 2 is farther
  t_mat <- matrix(c(1, 10), nrow = 2, ncol = 1)

  # Low alpha: more uniform weights
  W_low <- idw_weights(t_mat, c(0.5, 0.5))

  # High alpha: stronger decay
  W_high <- idw_weights(t_mat, c(3, 3))

  # With high alpha, the closer zone should get even more weight
  expect_true(W_high[1, 1] > W_low[1, 1])
})

test_that("idw_weights rejects negative alpha", {
  t_mat <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_error(idw_weights(t_mat, c(-1, 1)), "non-negative")
})

test_that("idw_weights offset parameter works", {
  t_mat <- matrix(c(0, 5, 10, 15), nrow = 2)
  alpha <- c(1, 1)
  # With offset=1 (default), zero travel time becomes 1
  W <- idw_weights(t_mat, alpha, offset = 1)
  expect_true(all(is.finite(W)))
  expect_equal(colSums(W), rep(1, 2), tolerance = 1e-10)
})

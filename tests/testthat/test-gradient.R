test_that("idw_gradient returns correct length", {
  t_mat <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3)
  p_mat <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 3)
  s_mat <- matrix(c(50, 60, 70, 80), nrow = 2)
  alpha <- c(1, 1, 1)

  g <- idw_gradient(alpha, t_mat, p_mat, s_mat)
  expect_type(g, "double")
  expect_length(g, 3)
})

test_that("idw_gradient matches numerical gradient", {
  skip_if_not_installed("numDeriv")

  t_mat <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3)
  p_mat <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 3)
  s_mat <- matrix(c(50, 60, 70, 80), nrow = 2)
  alpha <- c(1, 1.5, 0.5)

  analytical <- idw_gradient(alpha, t_mat, p_mat, s_mat)
  numerical <- numDeriv::grad(
    func = function(a) idw_objective(a, t_mat, p_mat, s_mat),
    x = alpha
  )
  expect_equal(analytical, numerical, tolerance = 1e-4)
})

test_that("idw_gradient handles multiple demographic groups", {
  skip_if_not_installed("numDeriv")

  set.seed(42)
  n <- 5
  m <- 3
  k <- 4
  t_mat <- matrix(runif(n * m, 1, 10), nrow = n)
  p_mat <- matrix(runif(n * k, 0, 100), nrow = n)
  s_mat <- matrix(runif(m * k, 0, 100), nrow = m)
  alpha <- runif(n, 0.5, 2)

  analytical <- idw_gradient(alpha, t_mat, p_mat, s_mat)
  numerical <- numDeriv::grad(
    func = function(a) idw_objective(a, t_mat, p_mat, s_mat),
    x = alpha
  )
  expect_equal(analytical, numerical, tolerance = 1e-3)
})

test_that("idw_gradient works with n=1 (single zone)", {
  skip_if_not_installed("numDeriv")

  t_mat <- matrix(c(3, 5), nrow = 1)
  p_mat <- matrix(c(100), nrow = 1)
  s_mat <- matrix(c(60, 50), nrow = 2)
  alpha <- c(1.2)

  analytical <- idw_gradient(alpha, t_mat, p_mat, s_mat)
  numerical <- numDeriv::grad(
    func = function(a) idw_objective(a, t_mat, p_mat, s_mat),
    x = alpha
  )
  expect_length(analytical, 1)
  expect_equal(analytical, numerical, tolerance = 1e-4)
})

test_that("idw_gradient works with n < m (more sources than zones)", {
  skip_if_not_installed("numDeriv")

  t_mat <- matrix(c(2, 3, 5, 4, 7, 6, 3, 8), nrow = 2)
  p_mat <- matrix(c(100, 200), nrow = 2)
  s_mat <- matrix(c(50, 60, 70, 80), nrow = 4)
  alpha <- c(1.0, 1.5)

  analytical <- idw_gradient(alpha, t_mat, p_mat, s_mat)
  numerical <- numDeriv::grad(
    func = function(a) idw_objective(a, t_mat, p_mat, s_mat),
    x = alpha
  )
  expect_length(analytical, 2)
  expect_equal(analytical, numerical, tolerance = 1e-4)
})

test_that("idw_gradient works with very small alpha (near zero)", {
  skip_if_not_installed("numDeriv")

  t_mat <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3)
  p_mat <- matrix(c(10, 20, 30), nrow = 3)
  s_mat <- matrix(c(50, 60), nrow = 2)
  alpha <- c(0.01, 0.01, 0.01)

  analytical <- idw_gradient(alpha, t_mat, p_mat, s_mat)
  numerical <- numDeriv::grad(
    func = function(a) idw_objective(a, t_mat, p_mat, s_mat),
    x = alpha
  )
  expect_equal(analytical, numerical, tolerance = 1e-3)
})

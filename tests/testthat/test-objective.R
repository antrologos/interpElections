test_that("idw_objective returns correct type and is non-negative", {
  t_mat <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3)
  p_mat <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 3)
  s_mat <- matrix(c(50, 60, 70, 80), nrow = 2)
  alpha <- c(1, 1, 1)

  val <- idw_objective(alpha, t_mat, p_mat, s_mat)
  expect_type(val, "double")
  expect_length(val, 1)
  expect_true(val >= 0)
})

test_that("idw_objective is zero when interpolation is perfect", {
  # If source_matrix = pop_matrix transposed through identity weights,

  # the objective should be minimized.
  # Simple case: 2x1 time matrix, 2x1 pop, 1x1 source
  t_mat <- matrix(c(1, 1), nrow = 2)
  alpha <- c(1, 1)
  # Equal weights -> source split equally
  s_mat <- matrix(c(20), nrow = 1)
  p_mat <- matrix(c(10, 10), nrow = 2)
  val <- idw_objective(alpha, t_mat, p_mat, s_mat)
  expect_equal(val, 0, tolerance = 1e-10)
})

test_that("idw_objective rejects dimension mismatches", {
  t_mat <- matrix(1:6, nrow = 3, ncol = 2)
  p_mat <- matrix(1:6, nrow = 3, ncol = 2)
  s_mat <- matrix(1:4, nrow = 2, ncol = 2)

  # Wrong alpha length

  expect_error(idw_objective(c(1, 1), t_mat, p_mat, s_mat), "alpha has length")

  # Wrong pop_matrix rows
  expect_error(
    idw_objective(c(1, 1), t_mat, matrix(1:4, nrow = 2, ncol = 2), s_mat),
    "pop_matrix has"
  )
})

test_that("idw_objective handles single demographic group", {
  t_mat <- matrix(c(2, 3, 4, 5), nrow = 2)
  p_mat <- matrix(c(10, 20), nrow = 2, ncol = 1)
  s_mat <- matrix(c(15, 15), nrow = 2, ncol = 1)
  alpha <- c(1, 1)
  val <- idw_objective(alpha, t_mat, p_mat, s_mat)
  expect_type(val, "double")
  expect_true(val >= 0)
})

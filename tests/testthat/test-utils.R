test_that("%||% works correctly", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(3 %||% 5, 3)
  expect_equal("a" %||% "b", "a")
  expect_null(NULL %||% NULL)
})

test_that(".col_standardize produces unit column sums", {
  mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  result <- interpElections:::.col_standardize(mat)
  expect_equal(colSums(result), c(1, 1), tolerance = 1e-10)
})

test_that(".col_standardize handles zero columns", {
  mat <- matrix(c(0, 0, 3, 4), nrow = 2, ncol = 2)
  result <- interpElections:::.col_standardize(mat)
  # First column is all zeros, should stay 0

  expect_equal(result[, 1], c(0, 0))
  expect_equal(sum(result[, 2]), 1, tolerance = 1e-10)
})

test_that(".apply_offset works correctly", {
  mat <- matrix(1:4, 2, 2)
  result <- interpElections:::.apply_offset(mat, 1)
  expect_equal(result, mat + 1)
})

test_that(".apply_offset rejects invalid offset", {
  mat <- matrix(1:4, 2, 2)
  expect_error(interpElections:::.apply_offset(mat, -1), "non-negative")
  expect_error(interpElections:::.apply_offset(mat, "a"), "non-negative")
  expect_error(interpElections:::.apply_offset(mat, c(1, 2)), "single")
})

test_that(".extract_args filters to target function formals", {
  dots <- list(alpha_init = 1, bad_arg = 2, max_steps = 10L)
  result <- interpElections:::.extract_args(dots, interpElections::optimize_alpha)
  expect_true("alpha_init" %in% names(result))
  expect_true("max_steps" %in% names(result))
  expect_false("bad_arg" %in% names(result))
})

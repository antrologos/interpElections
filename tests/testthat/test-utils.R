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

test_that("optim_control() and routing_control() create classed lists", {
  oc <- optim_control()
  expect_s3_class(oc, "interpElections_optim_control")
  expect_equal(oc$alpha_init, 2)
  expect_equal(oc$alpha_min, 1)

  rc <- routing_control()
  expect_s3_class(rc, "interpElections_routing_control")
  expect_equal(rc$mode, "WALK")
  expect_equal(rc$point_method, "pop_weighted")
  expect_equal(rc$max_trip_duration, 180L)
  expect_true(is.na(rc$fill_missing))
})

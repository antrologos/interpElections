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
  expect_equal(rc$mode, "AUTO")
  expect_equal(rc$point_method, "pop_weighted")
  expect_equal(rc$max_trip_duration, 120L)
  expect_equal(rc$fallback_max_trip_duration, 90L)
  expect_equal(rc$unreachable_threshold, 0.01)
  expect_true(is.na(rc$fill_missing))
})

test_that("routing_control() AUTO mode validation", {
  # AUTO cannot be combined with other modes

  expect_error(
    routing_control(mode = c("AUTO", "WALK")),
    "cannot be combined"
  )

  # Explicit WALK still works
  rc_walk <- routing_control(mode = "WALK")
  expect_equal(rc_walk$mode, "WALK")

  # Explicit CAR still works
  rc_car <- routing_control(mode = "CAR")
  expect_equal(rc_car$mode, "CAR")

  # Custom threshold
  rc <- routing_control(unreachable_threshold = 0.10)
  expect_equal(rc$unreachable_threshold, 0.10)

  # Custom fallback duration
  rc <- routing_control(fallback_max_trip_duration = 45L)
  expect_equal(rc$fallback_max_trip_duration, 45L)

  # Invalid threshold
  expect_error(
    routing_control(unreachable_threshold = -0.1),
    "between 0 and 1"
  )
  expect_error(
    routing_control(unreachable_threshold = 1.5),
    "between 0 and 1"
  )

  # Invalid fallback duration
  expect_error(
    routing_control(fallback_max_trip_duration = -10),
    "positive"
  )
})

test_that("routing_control() print method shows AUTO params", {
  rc <- routing_control()
  out <- capture.output(print(rc))
  expect_true(any(grepl("AUTO", out)))
  expect_true(any(grepl("fallback", out)))
  expect_true(any(grepl("threshold", out)))

  # Non-AUTO mode does not show fallback params
  rc_walk <- routing_control(mode = "WALK")
  out_walk <- capture.output(print(rc_walk))
  expect_false(any(grepl("fallback", out_walk)))
})

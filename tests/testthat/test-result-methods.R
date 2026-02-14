# Tests for S3 methods: summary, plot, as.data.frame, coef, residuals

# Helper: build a mock interpElections_result object
.mock_result <- function(
    n = 10, m = 5, k = 2, p = 3,
    keep_weights = FALSE, keep_time = FALSE,
    brazilian = FALSE
) {
  set.seed(42)

  pop_cols <- paste0("pop_", letters[seq_len(k)])
  src_cols <- paste0("src_", letters[seq_len(k)])
  interp_names <- paste0("VAR_", seq_len(p))

  # Mock tracts_sf
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5
    y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  for (col in pop_cols) tracts_df[[col]] <- rpois(n, 80)
  tracts_sf <- sf::st_sf(tracts_df, geometry = sfc)

  # Mock interpolated matrix
  interp_mat <- matrix(runif(n * p, 10, 200), n, p)
  colnames(interp_mat) <- interp_names

  # Join interpolated cols to tracts
  for (col in interp_names) {
    tracts_sf[[col]] <- interp_mat[, col]
  }

  # Mock sources data.frame
  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) sources[[col]] <- rpois(m, 160)
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  # Mock time matrix + weights
  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha <- runif(n, 0.5, 3)
  W <- idw_weights(tt, alpha, offset = 1)

  result <- list(
    interpolated = interp_mat,
    alpha = alpha,
    tracts_sf = tracts_sf,
    sources = sources,
    optimization = list(
      method = "cpu_lbfgsb",
      value = 123.45,
      convergence = 0L
    ),
    offset = 1,
    call = quote(interpolate_election()),
    zone_id = "zone_id",
    point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(zones = pop_cols, sources = src_cols),
    weights = if (keep_weights) W else NULL,
    time_matrix = if (keep_time) tt else NULL,
    sources_sf = NULL,
    code_muni = if (brazilian) "3550308" else NULL,
    year = if (brazilian) 2020L else NULL,
    census_year = if (brazilian) 2022L else NULL,
    what = if (brazilian) "candidates" else NULL,
    pop_data = if (brazilian) data.frame(x = 1) else NULL
  )
  class(result) <- "interpElections_result"
  result
}


# --- summary ---

test_that("summary.interpElections_result prints per-variable stats", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "interpElections result summary")
  expect_match(full, "Zones: 10")
  expect_match(full, "Sources: 5")
  expect_match(full, "Variables: 3")
  expect_match(full, "VAR_1")
  expect_match(full, "VAR_2")
  expect_match(full, "VAR_3")
  expect_match(full, "lightweight")
})

test_that("summary shows Brazilian metadata when present", {
  skip_if_not_installed("sf")
  obj <- .mock_result(brazilian = TRUE)

  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "3550308")
  expect_match(full, "election 2020")
  expect_match(full, "census 2022")
})

test_that("summary shows optimization info", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "cpu_lbfgsb")
  expect_match(full, "123.4500")
  expect_match(full, "Convergence: 0")
})

test_that("summary without optimization shows alpha range", {
  skip_if_not_installed("sf")
  obj <- .mock_result()
  obj$optimization <- NULL

  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "user-supplied")
})


# --- plot ---

test_that("plot.interpElections_result plots first variable by default", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  # Should not error
  expect_no_error(plot(obj))
})

test_that("plot.interpElections_result plots named variable", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  expect_no_error(plot(obj, var = "VAR_2"))
})

test_that("plot errors on missing variable", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  expect_error(plot(obj, var = "NONEXISTENT"), "not found")
})

test_that("plot errors when tracts_sf is NULL", {
  skip_if_not_installed("sf")
  obj <- .mock_result()
  obj$tracts_sf <- NULL

  expect_error(plot(obj), "No tracts_sf")
})


# --- as.data.frame ---

test_that("as.data.frame drops geometry and returns data frame", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  df <- as.data.frame(obj)
  expect_true(is.data.frame(df))
  expect_false(inherits(df, "sf"))
  expect_true("zone_id" %in% names(df))
  expect_true("VAR_1" %in% names(df))
  expect_equal(nrow(df), 10)
})

test_that("as.data.frame falls back to matrix when no tracts_sf", {
  skip_if_not_installed("sf")
  obj <- .mock_result()
  obj$tracts_sf <- NULL

  df <- as.data.frame(obj)
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 3)  # VAR_1, VAR_2, VAR_3
})


# --- coef ---

test_that("coef returns alpha vector", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  alpha <- coef(obj)
  expect_equal(alpha, obj$alpha)
  expect_equal(length(alpha), 10)
})


# --- residuals ---

test_that("residuals works with weights present", {
  skip_if_not_installed("sf")
  obj <- .mock_result(keep_weights = TRUE)

  resid <- residuals(obj)
  expect_true(is.matrix(resid))
  expect_equal(nrow(resid), 10)
  expect_equal(ncol(resid), 2)  # k = 2 calibration brackets
  expect_equal(colnames(resid), c("pop_a", "pop_b"))
})

test_that("residuals works with time_matrix (recomputes weights)", {
  skip_if_not_installed("sf")
  obj <- .mock_result(keep_time = TRUE)

  resid <- residuals(obj)
  expect_true(is.matrix(resid))
  expect_equal(nrow(resid), 10)
})

test_that("residuals errors without weights or time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  expect_error(residuals(obj), "Cannot compute residuals")
})

test_that("residuals with weights matches manual computation", {
  skip_if_not_installed("sf")
  obj <- .mock_result(keep_weights = TRUE)

  src_mat <- as.matrix(
    obj$sources[, obj$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"
  tracts_df <- sf::st_drop_geometry(obj$tracts_sf)
  pop_mat <- as.matrix(
    tracts_df[, obj$calib_cols$zones, drop = FALSE]
  )
  storage.mode(pop_mat) <- "double"

  expected <- obj$weights %*% src_mat - pop_mat
  colnames(expected) <- obj$calib_cols$zones
  actual <- residuals(obj)

  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("residuals errors when calib_cols is NULL", {
  skip_if_not_installed("sf")
  obj <- .mock_result(keep_weights = TRUE)
  obj$calib_cols <- NULL

  expect_error(residuals(obj), "No calibration columns")
})

test_that("residuals works with single calibration bracket (k=1)", {
  skip_if_not_installed("sf")
  obj <- .mock_result(n = 6, m = 4, k = 1, p = 2,
                      keep_weights = TRUE)

  resid <- residuals(obj)
  expect_true(is.matrix(resid))
  expect_equal(nrow(resid), 6)
  expect_equal(ncol(resid), 1)
})

test_that("residuals via time_matrix matches residuals via weights", {
  skip_if_not_installed("sf")
  obj_w <- .mock_result(keep_weights = TRUE, keep_time = TRUE)

  # Get residuals from weights directly
  resid_w <- residuals(obj_w)

  # Get residuals from time_matrix path (remove weights)
  obj_t <- obj_w
  obj_t$weights <- NULL
  resid_t <- residuals(obj_t)

  expect_equal(resid_t, resid_w, tolerance = 1e-10)
})


# --- Brazilian-specific S3 method tests ---

test_that("as.data.frame works for Brazilian result", {
  skip_if_not_installed("sf")
  obj <- .mock_result(brazilian = TRUE)

  df <- as.data.frame(obj)
  expect_true(is.data.frame(df))
  expect_false(inherits(df, "sf"))
  # Should have zone IDs and interpolated vars
  expect_true("zone_id" %in% names(df))
  expect_true("VAR_1" %in% names(df))
})

test_that("summary always shows alpha quantiles", {
  skip_if_not_installed("sf")

  # With optimization
  obj <- .mock_result()
  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")
  expect_match(full, "Q1=")
  expect_match(full, "Q3=")

  # Without optimization
  obj$optimization <- NULL
  out2 <- capture.output(summary(obj))
  full2 <- paste(out2, collapse = "\n")
  expect_match(full2, "Q1=")
  expect_match(full2, "Q3=")
})

test_that("plot error truncates long variable list", {
  skip_if_not_installed("sf")
  # Create result with many variables
  obj <- .mock_result(p = 20)

  expect_error(
    plot(obj, var = "NONEXISTENT"),
    "and 15 more"
  )
})

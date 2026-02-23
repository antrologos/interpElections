# Tests for compare_baselines() and leave_one_out()

.mock_baseline_result <- function(n = 10, m = 5, k = 2) {
  set.seed(42)
  pop_cols <- paste0("pop_", letters[seq_len(k)])
  src_cols <- paste0("src_", letters[seq_len(k)])
  interp_names <- paste0("VAR_", 1:3)

  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  for (col in pop_cols) tracts_df[[col]] <- rpois(n, 80)
  tracts_sf <- sf::st_sf(tracts_df,
                           geometry = sf::st_sfc(polys, crs = 4326))

  interp_mat <- matrix(runif(n * 3, 10, 200), n, 3)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in src_cols) sources[[col]] <- rpois(m, 160)
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha <- runif(n, 1, 4)
  K <- (tt + 1)^(-alpha); cs <- colSums(K); cs[cs == 0] <- 1
  W <- t(t(K) / cs)

  result <- list(
    interpolated = interp_mat, alpha = alpha,
    tracts_sf = tracts_sf, sources = sources,
    optimization = list(method = "mock", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election()),
    tract_id = "zone_id", point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = pop_cols, sources = src_cols),
    weights = W, time_matrix = tt,
    electoral_sf = NULL,
    code_muni = NULL, muni_boundary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


# --- compare_baselines ---

test_that("compare_baselines returns data frame", {
  skip_if_not_installed("sf")
  obj <- .mock_baseline_result()

  out <- compare_baselines(obj, methods = c("nearest", "uniform"))
  expect_true(is.data.frame(out))
  expect_true("method" %in% names(out))
  expect_true("rmse" %in% names(out))
  expect_true("deviance" %in% names(out))
  # Should have optimized + 2 baselines
  expect_equal(nrow(out), 3)
})

test_that("compare_baselines includes optimized IDW", {
  skip_if_not_installed("sf")
  obj <- .mock_baseline_result()

  out <- compare_baselines(obj, methods = "nearest")
  expect_true("Optimized IDW" %in% out$method)
  expect_true("Nearest station" %in% out$method)
})

test_that("compare_baselines returns NULL without time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_baseline_result()
  obj$time_matrix <- NULL

  expect_message(
    out <- compare_baselines(obj),
    "time_matrix"
  )
  expect_null(out)
})


# --- leave_one_out ---

test_that("leave_one_out returns data frame", {
  skip_if_not_installed("sf")
  obj <- .mock_baseline_result()

  out <- leave_one_out(obj, max_stations = 3, verbose = FALSE)
  expect_true(is.data.frame(out))
  expect_true("station_index" %in% names(out))
  expect_true("rmse" %in% names(out))
  expect_equal(nrow(out), 3)
})

test_that("leave_one_out respects max_stations", {
  skip_if_not_installed("sf")
  obj <- .mock_baseline_result()

  out <- leave_one_out(obj, max_stations = 2, verbose = FALSE)
  expect_equal(nrow(out), 2)
})

test_that("leave_one_out returns NULL without time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_baseline_result()
  obj$time_matrix <- NULL

  expect_message(
    out <- leave_one_out(obj, verbose = FALSE),
    "time_matrix"
  )
  expect_null(out)
})

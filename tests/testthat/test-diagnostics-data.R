# Tests for plot_travel_times()

.mock_tt_result <- function(n = 10, m = 5) {
  set.seed(42)
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  tracts_sf <- sf::st_sf(
    data.frame(zone_id = paste0("Z", seq_len(n))),
    geometry = sf::st_sfc(polys, crs = 4326)
  )

  pts <- sf::st_sfc(lapply(seq_len(m), function(j) {
    sf::st_point(c(runif(1, 0, 5), runif(1, 0, 2)))
  }), crs = 4326)
  electoral_sf <- sf::st_sf(
    data.frame(point_id = paste0("P", seq_len(m))),
    geometry = pts
  )

  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  interp_mat <- matrix(runif(n * 3, 10, 200), n, 3)
  colnames(interp_mat) <- paste0("VAR_", 1:3)

  result <- list(
    interpolated = interp_mat,
    alpha = runif(n, 1, 4),
    tracts_sf = tracts_sf,
    sources = data.frame(point_id = paste0("P", seq_len(m))),
    optimization = list(method = "mock", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election()),
    tract_id = "zone_id", point_id = "point_id",
    interp_cols = paste0("VAR_", 1:3),
    calib_cols = NULL,
    weights = NULL, time_matrix = tt,
    electoral_sf = electoral_sf,
    code_muni = NULL, muni_boundary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


test_that("plot_travel_times histogram returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_tt_result()

  p <- plot_travel_times(obj, type = "histogram")
  expect_s3_class(p, "ggplot")
})

test_that("plot_travel_times heatmap returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_tt_result()

  p <- plot_travel_times(obj, type = "heatmap")
  expect_s3_class(p, "ggplot")
})

test_that("plot_travel_times map returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_tt_result()

  p <- plot_travel_times(obj, type = "map", tract = 1)
  expect_s3_class(p, "ggplot")
})

test_that("plot_travel_times returns NULL without time_matrix", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_tt_result()
  obj$time_matrix <- NULL

  expect_message(p <- plot_travel_times(obj), "No time_matrix")
  expect_null(p)
})

test_that("plot_travel_times map picks random tract when NULL", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_tt_result()

  expect_message(
    p <- plot_travel_times(obj, type = "map"),
    "No tract specified"
  )
  expect_s3_class(p, "ggplot")
})

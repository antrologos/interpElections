# Tests for weight diagnostics: weight_summary, plot_weights, plot_connections

# --- Shared mock ---

.mock_weight_result <- function(n = 10, m = 5) {
  set.seed(42)
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(x0, y0, x0+1, y0, x0+1, y0+1, x0, y0+1, x0, y0), ncol = 2, byrow = TRUE)))
  })
  tracts_sf <- sf::st_sf(
    data.frame(zone_id = paste0("Z", seq_len(n))),
    geometry = sf::st_sfc(polys, crs = 4326)
  )

  # Station points
  pts <- sf::st_sfc(lapply(seq_len(m), function(j) {
    sf::st_point(c(runif(1, 0, 5), runif(1, 0, 2)))
  }), crs = 4326)
  electoral_sf <- sf::st_sf(data.frame(point_id = paste0("P", seq_len(m))), geometry = pts)

  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha <- runif(n, 1, 4)
  K <- (tt + 1)^(-alpha); cs <- colSums(K); cs[cs == 0] <- 1
  W <- t(t(K) / cs)

  interp_mat <- matrix(runif(n * 3, 10, 200), n, 3)
  colnames(interp_mat) <- paste0("VAR_", 1:3)

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in paste0("VAR_", 1:3)) sources[[col]] <- rpois(m, 50)

  result <- list(
    interpolated = interp_mat, alpha = alpha, tracts_sf = tracts_sf,
    sources = sources, optimization = list(method = "mock", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election()),
    tract_id = "zone_id", point_id = "point_id",
    interp_cols = paste0("VAR_", 1:3),
    calib_cols = NULL, weights = W, time_matrix = tt,
    electoral_sf = electoral_sf,
    code_muni = NULL, muni_boundary = NULL
  )
  class(result) <- "interpElections_result"
  result
}


# --- weight_summary tests ---

test_that("weight_summary returns data frame with correct columns", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)

  expect_s3_class(ws, "data.frame")
  expected_cols <- c("tract_id", "dominant_source", "dominant_weight",
                     "top3_weight", "entropy", "effective_n_sources",
                     "herfindahl", "mean_travel_time_weighted")
  expect_true(all(expected_cols %in% names(ws)))
})

test_that("weight_summary has correct number of rows", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result(n = 8, m = 4)
  ws <- weight_summary(obj)
  expect_equal(nrow(ws), 8)
})

test_that("weight_summary tract_id matches tracts_sf", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  expected_ids <- sf::st_drop_geometry(obj$tracts_sf)[["zone_id"]]
  expect_equal(ws$tract_id, expected_ids)
})

test_that("weight_summary dominant_source is valid index", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result(n = 10, m = 5)
  ws <- weight_summary(obj)
  expect_true(all(ws$dominant_source >= 1L))
  expect_true(all(ws$dominant_source <= 5L))
})

test_that("weight_summary dominant_weight equals max weight per row", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  W <- obj$weights
  expected <- apply(W, 1, max)
  expect_equal(ws$dominant_weight, expected, tolerance = 1e-10)
})

test_that("weight_summary top3_weight >= dominant_weight", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  expect_true(all(ws$top3_weight >= ws$dominant_weight - 1e-10))
})

test_that("weight_summary herfindahl is between 0 and 1", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  expect_true(all(ws$herfindahl >= 0))
  expect_true(all(ws$herfindahl <= 1 + 1e-10))
})

test_that("weight_summary effective_n_sources >= 1", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  expect_true(all(ws$effective_n_sources >= 1 - 1e-10))
})

test_that("weight_summary entropy is non-negative", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  expect_true(all(ws$entropy >= -1e-10))
})

test_that("weight_summary mean_travel_time_weighted is computed with time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  expect_true(all(is.finite(ws$mean_travel_time_weighted)))
  expect_true(all(ws$mean_travel_time_weighted > 0))
})

test_that("weight_summary mean_travel_time_weighted is NA without time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  obj$time_matrix <- NULL
  ws <- weight_summary(obj)
  expect_true(all(is.na(ws$mean_travel_time_weighted)))
})

test_that("weight_summary mean_travel_time_weighted handles NA in time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  # Introduce NA into time_matrix (simulating unreachable pairs)
  obj$time_matrix[1, 2] <- NA
  obj$time_matrix[3, 4] <- NA
  # Set corresponding weights to zero (as the kernel would)
  obj$weights[1, 2] <- 0
  obj$weights[3, 4] <- 0
  ws <- weight_summary(obj)
  expect_true(all(is.finite(ws$mean_travel_time_weighted)))
})

test_that("weight_summary errors without weights", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  obj$weights <- NULL
  obj$optimization <- list(method = "mock")
  expect_error(weight_summary(obj), "Weight matrix")
})

test_that("weight_summary errors on non-result object", {
  expect_error(weight_summary(list()), "interpElections_result")
})


# --- plot_weights entropy tests ---

test_that("plot_weights entropy returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, type = "entropy")
  expect_s3_class(p, "gg")
})

test_that("plot_weights entropy has correct title", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, type = "entropy")
  expect_true(grepl("Effective number", p$labels$title))
})

test_that("plot_weights entropy with continuous breaks", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, type = "entropy", breaks = "continuous")
  expect_s3_class(p, "gg")
})


# --- plot_weights dominant tests ---

test_that("plot_weights dominant returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, type = "dominant")
  expect_s3_class(p, "gg")
})

test_that("plot_weights dominant has correct title", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, type = "dominant")
  expect_true(grepl("Dominant station", p$labels$title))
})

test_that("plot_weights dominant overlays stations when electoral_sf available", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, type = "dominant")
  # Should have more than 1 layer (base + stations)
  expect_true(length(p$layers) >= 2)
})


# --- plot_weights catchment tests ---

test_that("plot_weights catchment with NULL tract returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, type = "catchment")
  expect_s3_class(p, "gg")
})

test_that("plot_weights catchment with tract index returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, tract = 3, type = "catchment")
  expect_s3_class(p, "gg")
})

test_that("plot_weights catchment with tract ID returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, tract = "Z5", type = "catchment")
  expect_s3_class(p, "gg")
})

test_that("plot_weights catchment title includes tract label", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, tract = 3, type = "catchment")
  expect_true(grepl("Z3", p$labels$title))
})

test_that("plot_weights catchment respects threshold", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_weights(obj, tract = 1, type = "catchment", threshold = 0.5)
  expect_s3_class(p, "gg")
})


# --- plot_weights error handling ---

test_that("plot_weights errors on non-result object", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  expect_error(plot_weights(list()), "interpElections_result")
})

test_that("plot_weights errors without weights", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  obj$weights <- NULL
  obj$optimization <- list(method = "mock")
  expect_error(plot_weights(obj, type = "entropy"), "Weight matrix")
})

test_that("plot_weights errors without tracts_sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  obj$tracts_sf <- NULL
  expect_error(plot_weights(obj, type = "entropy"), "tracts_sf")
})


# --- plot_connections overview tests ---

test_that("plot_connections overview returns ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj)
  expect_s3_class(p, "gg")
})

test_that("plot_connections overview has correct title", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj)
  expect_true(grepl("connection", p$labels$title, ignore.case = TRUE))
})

test_that("plot_connections overview shows all tracts by default", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj, show_all_tracts = TRUE)
  expect_s3_class(p, "gg")
})

test_that("plot_connections overview without background tracts", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj, show_all_tracts = FALSE)
  expect_s3_class(p, "gg")
})


# --- plot_connections with top_k and threshold ---

test_that("plot_connections respects top_k", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj, top_k = 2)
  expect_s3_class(p, "gg")
})

test_that("plot_connections respects threshold", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj, threshold = 0.5)
  expect_s3_class(p, "gg")
})


# --- plot_connections error handling ---

test_that("plot_connections errors on non-result object", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  expect_error(plot_connections(list()), "interpElections_result")
})

test_that("plot_connections errors without weights", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  obj$weights <- NULL
  obj$optimization <- list(method = "mock")
  expect_error(plot_connections(obj), "Weight matrix")
})

test_that("plot_connections errors without electoral_sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  obj$electoral_sf <- NULL
  expect_error(plot_connections(obj), "electoral_sf")
})

test_that("plot_connections errors without tracts_sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  obj$tracts_sf <- NULL
  expect_error(plot_connections(obj), "tracts_sf")
})

test_that("plot_connections overview has fill scale (tracts colored)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj)
  # Should have a fill scale for effective sources
  expect_true(!is.null(p$scales$get_scales("fill")))
})

test_that("plot_connections overview subtitle includes filter info", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_weight_result()
  p <- plot_connections(obj, top_k = 3, threshold = 0.05)
  expect_true(grepl("top_k", p$labels$subtitle))
  expect_true(grepl("threshold", p$labels$subtitle))
})


# --- .resolve_tract_indices tests ---

test_that(".resolve_tract_indices resolves numeric index", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result(n = 10, m = 5)
  idx <- .resolve_tract_indices(3, obj)
  expect_equal(idx, 3L)
})

test_that(".resolve_tract_indices resolves character tract ID", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  idx <- .resolve_tract_indices("Z5", obj)
  expect_equal(idx, 5L)
})

test_that(".resolve_tract_indices resolves multiple indices", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result(n = 10, m = 5)
  idx <- .resolve_tract_indices(c(1, 5, 10), obj)
  expect_equal(idx, c(1L, 5L, 10L))
})

test_that(".resolve_tract_indices errors on out-of-range index", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result(n = 10, m = 5)
  expect_error(.resolve_tract_indices(15, obj), "out of range")
})


# --- .get_station_ids / .get_tract_ids tests ---

test_that(".get_station_ids returns real IDs from electoral_sf", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ids <- .get_station_ids(obj)
  expect_equal(ids, paste0("P", 1:5))
})

test_that(".get_station_ids falls back to sequence", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  obj$electoral_sf <- NULL
  ids <- .get_station_ids(obj)
  expect_equal(ids, as.character(1:5))
})

test_that(".get_tract_ids returns real IDs from tracts_sf", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ids <- .get_tract_ids(obj)
  expect_equal(ids, paste0("Z", 1:10))
})

test_that(".get_tract_ids falls back to sequence", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  obj$tracts_sf <- NULL
  ids <- .get_tract_ids(obj)
  expect_equal(ids, as.character(1:10))
})

test_that("tract popup contains real station IDs", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  ws <- weight_summary(obj)
  popups <- .build_connection_tract_popup(obj, obj$weights, ws)
  expect_true(any(grepl("P1", popups) | grepl("P2", popups)))
})

test_that("station popup contains real station IDs", {
  skip_if_not_installed("sf")
  obj <- .mock_weight_result()
  popups <- .build_connection_station_popup(obj, obj$weights)
  expect_true(any(grepl("P1", popups) | grepl("P2", popups)))
})

test_that("interactive connections returns htmlwidget", {
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  obj <- .mock_weight_result()
  W <- obj$weights
  centroids <- sf::st_coordinates(
    sf::st_centroid(sf::st_geometry(obj$tracts_sf))
  )
  station_coords <- sf::st_coordinates(obj$electoral_sf)
  m <- .plot_connections_interactive(
    obj, W, centroids, station_coords[, 1:2],
    top_k = NULL, threshold = 0.01,
    show_all_tracts = TRUE, palette = "YlOrRd"
  )
  expect_s3_class(m, "htmlwidget")
})

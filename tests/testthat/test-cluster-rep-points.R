# Tests for cluster-based representative point selection
# (pop_weighted method with patches + road proximity)

# Helper: build a 2-cluster raster inside a tract polygon
.make_two_cluster_setup <- function() {
  # Tract polygon: large enough (>= 1 km2 at the equator)
  tract_poly <- sf::st_polygon(list(matrix(c(
    0, 0,  1, 0,  1, 1,  0, 1,  0, 0
  ), ncol = 2, byrow = TRUE)))
  tracts <- sf::st_sf(
    data.frame(id = "T1"),
    geometry = sf::st_sfc(tract_poly, crs = 4326)
  )

  # Raster with two clusters:
  #  Cluster A: 3 cells near (0.2, 0.2) with total pop 120
  #  Cluster B: 1 cell near (0.8, 0.8) with pop 100
  # The gap between them makes them disconnected patches
  r <- terra::rast(
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0

  # Cluster A: cells in rows 8-9, cols 2-3 (near bottom-left)
  # Using row/col: row 1 is top, row 10 is bottom
  r[9, 2] <- 50   # (x~0.15, y~0.15)
  r[9, 3] <- 40   # (x~0.25, y~0.15)
  r[8, 2] <- 30   # (x~0.15, y~0.25)

  # Cluster B: single cell in row 2, col 9 (top-right)
  r[2, 9] <- 100  # (x~0.85, y~0.85)

  list(tracts = tracts, raster = r)
}


# --- Core cluster selection tests ---

test_that("pop_weighted selects the largest cluster by total population", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  setup <- .make_two_cluster_setup()

  pts <- interpElections:::compute_representative_points(
    setup$tracts, method = "pop_weighted",
    pop_raster = setup$raster,
    min_area_for_pop_weight = 0,  # force raster for all tracts
    tract_id = "id",
    verbose = FALSE
  )

  coords <- sf::st_coordinates(pts)
  # Cluster A (total 120) is near (0.15-0.25, 0.15-0.25)
  # Cluster B (total 100) is near (0.85, 0.85)
  # Point should land in cluster A (larger total population)
  expect_true(coords[1, "X"] < 0.5,
              info = "X coordinate should be in the left half (cluster A)")
  expect_true(coords[1, "Y"] < 0.5,
              info = "Y coordinate should be in the bottom half (cluster A)")
})


test_that("pop_weighted picks max-pop cell in the selected cluster", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  setup <- .make_two_cluster_setup()

  pts <- interpElections:::compute_representative_points(
    setup$tracts, method = "pop_weighted",
    pop_raster = setup$raster,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    verbose = FALSE
  )

  coords <- sf::st_coordinates(pts)
  # The highest cell in cluster A is at (x~0.15, y~0.15) with pop=50
  # Cell centers: r[9,2] → x=0.15, y=0.15
  expect_equal(unname(coords[1, "X"]), 0.15, tolerance = 0.05)
  expect_equal(unname(coords[1, "Y"]), 0.15, tolerance = 0.05)
})


test_that("single cluster with single cell degenerates to old behavior", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tract_poly <- sf::st_polygon(list(matrix(c(
    0, 0,  1, 0,  1, 1,  0, 1,  0, 0
  ), ncol = 2, byrow = TRUE)))
  tracts <- sf::st_sf(
    data.frame(id = "T1"),
    geometry = sf::st_sfc(tract_poly, crs = 4326)
  )

  r <- terra::rast(
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0
  r[5, 5] <- 42  # Single populated cell

  pts <- interpElections:::compute_representative_points(
    tracts, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    verbose = FALSE
  )

  coords <- sf::st_coordinates(pts)
  # Should point to the single populated cell center
  expect_equal(unname(coords[1, "X"]), 0.45, tolerance = 0.05)
  expect_equal(unname(coords[1, "Y"]), 0.55, tolerance = 0.05)
})


test_that("zero-population tract falls back to point_on_surface", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  tract_poly <- sf::st_polygon(list(matrix(c(
    0, 0,  1, 0,  1, 1,  0, 1,  0, 0
  ), ncol = 2, byrow = TRUE)))
  tracts <- sf::st_sf(
    data.frame(id = "T1"),
    geometry = sf::st_sfc(tract_poly, crs = 4326)
  )

  r <- terra::rast(
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0  # all zero

  expect_warning(
    pts <- interpElections:::compute_representative_points(
      tracts, method = "pop_weighted",
      pop_raster = r,
      min_area_for_pop_weight = 0,
      tract_id = "id",
      verbose = FALSE
    ),
    "no population"
  )

  expect_equal(nrow(pts), 1)
  no_pop <- attr(pts, "no_pop_tracts")
  expect_true("T1" %in% no_pop)
})


# --- Diagnostic attribute tests ---

test_that("pop_weighted_diagnostics attribute has expected structure", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  setup <- .make_two_cluster_setup()

  pts <- interpElections:::compute_representative_points(
    setup$tracts, method = "pop_weighted",
    pop_raster = setup$raster,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    verbose = FALSE
  )

  diag <- attr(pts, "pop_weighted_diagnostics")
  expect_false(is.null(diag))
  expect_type(diag, "list")
  expect_equal(length(diag), 1L)
  expect_equal(names(diag), "T1")

  d <- diag[["T1"]]
  expect_equal(d$tract_id, "T1")
  expect_true(d$n_patches >= 2L)
  expect_true(is.data.frame(d$patch_pop))
  expect_true(is.numeric(d$selected_patch))
  expect_named(d$selected_xy, c("x", "y"))
  expect_true(is.numeric(d$selected_pop))
  expect_type(d$has_roads, "logical")
  expect_type(d$near_road, "logical")
  expect_false(d$has_roads)
  expect_false(d$near_road)
})


test_that("diagnostics report multiple clusters correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  setup <- .make_two_cluster_setup()

  pts <- interpElections:::compute_representative_points(
    setup$tracts, method = "pop_weighted",
    pop_raster = setup$raster,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    verbose = FALSE
  )

  diag <- attr(pts, "pop_weighted_diagnostics")
  d <- diag[["T1"]]

  # Should find at least 2 patches
  expect_true(d$n_patches >= 2L)
  # The selected patch should be the one with higher total population
  pp <- d$patch_pop
  val_col <- setdiff(names(pp), "patches")[1]
  best_idx <- which.max(pp[[val_col]])
  expect_equal(d$selected_patch, pp$patches[best_idx])
})


# --- Road proximity tests ---

test_that("road proximity prefers near-road cell within largest cluster", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Tract polygon
  tract_poly <- sf::st_polygon(list(matrix(c(
    0, 0,  1, 0,  1, 1,  0, 1,  0, 0
  ), ncol = 2, byrow = TRUE)))
  tracts <- sf::st_sf(
    data.frame(id = "T1"),
    geometry = sf::st_sfc(tract_poly, crs = 4326)
  )

  # Raster: single cluster with 2 cells
  r <- terra::rast(
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0
  # Cell A: high population, far from road
  r[5, 2] <- 100  # (x~0.15, y~0.55)
  # Cell B: lower population, close to road
  r[5, 3] <- 60   # (x~0.25, y~0.55) — adjacent to A → same cluster

  # Road near cell B (at x=0.3)
  road <- sf::st_sf(
    data.frame(highway = "residential"),
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0.3, 0, 0.3, 1), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )

  pts <- interpElections:::compute_representative_points(
    tracts, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    osm_roads = road,
    verbose = FALSE
  )

  diag <- attr(pts, "pop_weighted_diagnostics")
  d <- diag[["T1"]]

  # Should have roads
  expect_true(d$has_roads)
  # At equator, 200m buffer ~ 0.0018 degrees, so the road at x=0.3 doesn't
  # actually reach cells at x=0.15 or x=0.25 (which are ~11km and ~5.5km away).
  # So near_road should be FALSE (fallback to max-pop in cluster).
  expect_false(d$near_road)
  # The point should be at the highest-pop cell (0.15, 0.55)
  coords <- sf::st_coordinates(pts)
  expect_equal(unname(coords[1, "X"]), 0.15, tolerance = 0.05)
})


test_that("no roads means has_roads = FALSE and near_road = FALSE", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  setup <- .make_two_cluster_setup()

  pts <- interpElections:::compute_representative_points(
    setup$tracts, method = "pop_weighted",
    pop_raster = setup$raster,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    osm_roads = NULL,
    verbose = FALSE
  )

  diag <- attr(pts, "pop_weighted_diagnostics")
  d <- diag[["T1"]]
  expect_false(d$has_roads)
  expect_false(d$near_road)
})


test_that("empty roads sf is handled gracefully", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  setup <- .make_two_cluster_setup()

  # Empty roads data frame
  empty_roads <- sf::st_sf(
    data.frame(highway = character(0)),
    geometry = sf::st_sfc(crs = 4326)
  )

  pts <- interpElections:::compute_representative_points(
    setup$tracts, method = "pop_weighted",
    pop_raster = setup$raster,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    osm_roads = empty_roads,
    verbose = FALSE
  )

  # Should work normally without roads
  expect_equal(nrow(pts), 1)
  diag <- attr(pts, "pop_weighted_diagnostics")
  d <- diag[["T1"]]
  expect_false(d$has_roads)
})


# --- .read_osm_roads tests ---

test_that(".read_osm_roads returns NULL for missing directory", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  result <- interpElections:::.read_osm_roads(tmp, verbose = FALSE)
  expect_null(result)
})

test_that(".read_osm_roads returns NULL for empty directory", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  result <- interpElections:::.read_osm_roads(tmp, verbose = FALSE)
  expect_null(result)
})


# --- .diag_rep_point_clusters tests ---

test_that(".diag_rep_point_clusters returns skip when no diagnostics", {
  result <- list()
  class(result) <- "interpElections_result"
  result$pop_weighted_diagnostics <- NULL

  chk <- interpElections:::.diag_rep_point_clusters(result)
  expect_equal(chk$status, "skip")
})

test_that(".diag_rep_point_clusters summarizes diagnostics correctly", {
  result <- list()
  class(result) <- "interpElections_result"
  result$pop_weighted_diagnostics <- list(
    T1 = list(
      tract_id = "T1", n_patches = 3L,
      has_roads = TRUE, near_road = TRUE
    ),
    T2 = list(
      tract_id = "T2", n_patches = 1L,
      has_roads = TRUE, near_road = FALSE
    ),
    T3 = list(
      tract_id = "T3", n_patches = 2L,
      has_roads = FALSE, near_road = FALSE
    )
  )

  chk <- interpElections:::.diag_rep_point_clusters(result)
  expect_equal(chk$status, "pass")
  expect_equal(chk$value$n_tracts, 3L)
  expect_equal(chk$value$n_multi_cluster, 2L)  # T1 and T3
  expect_equal(chk$value$n_with_roads, 2L)     # T1 and T2
  expect_equal(chk$value$n_near_road, 1L)       # T1 only
})

test_that(".diag_rep_point_clusters warns when >50% have no roads", {
  result <- list()
  class(result) <- "interpElections_result"
  result$pop_weighted_diagnostics <- list(
    T1 = list(tract_id = "T1", n_patches = 1L, has_roads = FALSE, near_road = FALSE),
    T2 = list(tract_id = "T2", n_patches = 1L, has_roads = FALSE, near_road = FALSE),
    T3 = list(tract_id = "T3", n_patches = 1L, has_roads = TRUE, near_road = FALSE)
  )

  chk <- interpElections:::.diag_rep_point_clusters(result)
  expect_equal(chk$status, "warn")
  expect_match(chk$detail, "without road data")
})


# --- plot_representative_points tests ---

test_that("plot_representative_points returns NULL without diagnostics", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")

  tracts <- sf::st_sf(
    data.frame(id = "Z1"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0),
                                  ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  pts <- suppressWarnings(sf::st_point_on_surface(tracts))
  attr(pts, "point_method") <- "point_on_surface"

  expect_message(
    p <- plot_representative_points(pts, tracts_sf = tracts),
    "No cluster diagnostics"
  )
  expect_null(p)
})


test_that("plot_representative_points summary mode returns ggplot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  setup <- .make_two_cluster_setup()

  pts <- interpElections:::compute_representative_points(
    setup$tracts, method = "pop_weighted",
    pop_raster = setup$raster,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    verbose = FALSE
  )

  p <- plot_representative_points(pts, tracts_sf = setup$tracts)
  expect_s3_class(p, "gg")
})


# --- Multiple tracts test ---

test_that("pop_weighted handles multiple tracts with diagnostics", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  polys <- list(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0),
                                ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2,0, 3,0, 3,1, 2,1, 2,0),
                                ncol = 2, byrow = TRUE)))
  )
  tracts <- sf::st_sf(
    data.frame(id = c("T1", "T2")),
    geometry = sf::st_sfc(polys, crs = 4326)
  )

  r <- terra::rast(
    xmin = -0.5, xmax = 3.5, ymin = -0.5, ymax = 1.5,
    resolution = 0.1, crs = "EPSG:4326"
  )
  terra::values(r) <- 0
  # Populate both tract areas
  # col 8: x = -0.5 + 7.5*0.1 = 0.25 (inside T1: x in [0,1])
  # col 28: x = -0.5 + 27.5*0.1 = 2.25 (inside T2: x in [2,3])
  # row 10: y = 1.5 - 9.5*0.1 = 0.55 (inside both: y in [0,1])
  r[10, 8]  <- 80   # in T1
  r[10, 28] <- 90   # in T2

  pts <- interpElections:::compute_representative_points(
    tracts, method = "pop_weighted",
    pop_raster = r,
    min_area_for_pop_weight = 0,
    tract_id = "id",
    verbose = FALSE
  )

  expect_equal(nrow(pts), 2)
  diag <- attr(pts, "pop_weighted_diagnostics")
  expect_equal(length(diag), 2L)
  expect_true("T1" %in% names(diag))
  expect_true("T2" %in% names(diag))
})

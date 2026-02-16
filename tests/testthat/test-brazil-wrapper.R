# Tests for interpolate_election_br(), .br_resolve_muni(), .br_match_calibration()

test_that(".br_resolve_muni resolves Sao Paulo correctly", {
  info <- interpElections:::.br_resolve_muni("3550308")
  expect_equal(info$code_muni_ibge, "3550308")
  expect_equal(info$code_muni_tse, "71072")
  expect_equal(info$uf, "SP")
  expect_equal(info$nome_municipio, "SÃO PAULO")
})

test_that(".br_resolve_muni resolves Boa Vista correctly", {
  info <- interpElections:::.br_resolve_muni("1400100")
  expect_equal(info$code_muni_ibge, "1400100")
  expect_equal(info$code_muni_tse, "03018")
  expect_equal(info$uf, "RR")
})

test_that(".br_resolve_muni resolves Rio de Janeiro correctly", {
  info <- interpElections:::.br_resolve_muni("3304557")
  expect_equal(info$code_muni_ibge, "3304557")
  expect_equal(info$uf, "RJ")
})

test_that(".br_resolve_muni errors on invalid code", {
  expect_error(
    interpElections:::.br_resolve_muni("9999999"),
    "not found"
  )
})


# --- Name-based resolution tests ---

test_that(".br_resolve_muni resolves by name (uppercase, accented)", {
  info <- interpElections:::.br_resolve_muni("SÃO PAULO")
  expect_equal(info$code_muni_ibge, "3550308")
  expect_equal(info$uf, "SP")
})

test_that(".br_resolve_muni resolves by name (case-insensitive)", {
  info <- interpElections:::.br_resolve_muni("sao paulo")
  expect_equal(info$code_muni_ibge, "3550308")
  expect_equal(info$uf, "SP")
})

test_that(".br_resolve_muni resolves by name (accent-insensitive)", {
  info1 <- interpElections:::.br_resolve_muni("Sao Paulo")
  info2 <- interpElections:::.br_resolve_muni("São Paulo")
  expect_equal(info1$code_muni_ibge, "3550308")
  expect_equal(info2$code_muni_ibge, "3550308")
})

test_that(".br_resolve_muni resolves by name (whitespace-trimmed)", {
  info <- interpElections:::.br_resolve_muni("  Sao Paulo  ")
  expect_equal(info$code_muni_ibge, "3550308")
})

test_that(".br_resolve_muni with uf disambiguates duplicate names", {
  xwalk <- interpElections::muni_crosswalk
  norm_names <- toupper(trimws(xwalk$nome))
  dup_names <- names(which(table(norm_names) > 1))
  skip_if(length(dup_names) == 0, "No duplicate municipality names found")

  test_name <- dup_names[1]
  test_rows <- xwalk[toupper(trimws(xwalk$nome)) == test_name, ]

  # Without uf -> should error listing the matches
  expect_error(
    interpElections:::.br_resolve_muni(test_name),
    "matches.*municipalities"
  )

  # With uf -> should resolve to the correct one
  for (i in seq_len(nrow(test_rows))) {
    info <- interpElections:::.br_resolve_muni(test_name, uf = test_rows$uf[i])
    expect_equal(info$code_muni_ibge, test_rows$code_ibge[i])
    expect_equal(info$uf, test_rows$uf[i])
  }
})

test_that(".br_resolve_muni errors on unknown name", {
  expect_error(
    interpElections:::.br_resolve_muni("NONEXISTENT CITY XYZ"),
    "not found"
  )
})

test_that(".br_resolve_muni errors on valid name + wrong uf", {
  expect_error(
    interpElections:::.br_resolve_muni("Sao Paulo", uf = "RJ"),
    "not found in state"
  )
})

test_that(".br_resolve_muni warns and ignores uf for IBGE code input", {
  expect_message(
    info <- interpElections:::.br_resolve_muni("3550308", uf = "RJ"),
    "ignored"
  )
  expect_equal(info$code_muni_ibge, "3550308")
  expect_equal(info$uf, "SP")
})

test_that(".br_resolve_muni handles numeric input as IBGE code", {
  info <- interpElections:::.br_resolve_muni(3550308)
  expect_equal(info$code_muni_ibge, "3550308")
})

test_that(".br_resolve_muni character IBGE code works", {
  info <- interpElections:::.br_resolve_muni("3550308")
  expect_equal(info$code_muni_ibge, "3550308")
  expect_equal(info$uf, "SP")
})

test_that(".br_resolve_muni uf is case-insensitive", {
  xwalk <- interpElections::muni_crosswalk
  norm_names <- toupper(trimws(xwalk$nome))
  dup_names <- names(which(table(norm_names) > 1))
  skip_if(length(dup_names) == 0)

  test_name <- dup_names[1]
  test_rows <- xwalk[toupper(trimws(xwalk$nome)) == test_name, ]

  info <- interpElections:::.br_resolve_muni(test_name, uf = tolower(test_rows$uf[1]))
  expect_equal(info$code_muni_ibge, test_rows$code_ibge[1])
})

test_that("interpolate_election_br has uf parameter", {
  expect_true("uf" %in% names(formals(interpolate_election_br)))
  expect_null(formals(interpolate_election_br)$uf)
})


test_that("census year auto-selection works correctly", {
  # Test the auto-selection logic inline (it's inside interpolate_election_br)
  auto_select <- function(year) {
    if (year <= 2004) 2000L
    else if (year <= 2016) 2010L
    else 2022L
  }

  expect_equal(auto_select(2000), 2000L)
  expect_equal(auto_select(2004), 2000L)
  expect_equal(auto_select(2008), 2010L)
  expect_equal(auto_select(2012), 2010L)
  expect_equal(auto_select(2016), 2010L)
  expect_equal(auto_select(2020), 2022L)
  expect_equal(auto_select(2024), 2022L)
})


# --- .br_match_calibration tests ---

# Helper to build mock tracts/electoral sf for calibration matching
.mock_tracts_for_calib <- function(n, census_year) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  df <- data.frame(id = seq_len(n))

  if (census_year %in% c(2000, 2010)) {
    df$pop_18_20 <- rpois(n, 30)
    df$pop_21_24 <- rpois(n, 40)
    df$pop_25_29 <- rpois(n, 50)
    df$pop_30_39 <- rpois(n, 80)
    df$pop_40_49 <- rpois(n, 70)
    df$pop_50_59 <- rpois(n, 60)
    df$pop_60_69 <- rpois(n, 40)
  } else {
    # Census 2022
    df$pop_15_19 <- rpois(n, 50)
    df$pop_20_24 <- rpois(n, 50)
    df$pop_25_29 <- rpois(n, 50)
    df$pop_30_39 <- rpois(n, 80)
    df$pop_40_49 <- rpois(n, 70)
    df$pop_50_59 <- rpois(n, 60)
    df$pop_60_69 <- rpois(n, 40)
  }
  sf::st_sf(df, geometry = sfc)
}

.mock_electoral_for_calib <- function(m) {
  pts <- sf::st_sfc(
    lapply(seq_len(m), function(i) sf::st_point(c(runif(1), runif(1)))),
    crs = 4326
  )
  df <- data.frame(
    id = seq_len(m),
    votantes_18_19 = rpois(m, 20),
    votantes_20 = rpois(m, 10),
    votantes_21_24 = rpois(m, 40),
    votantes_25_29 = rpois(m, 50),
    votantes_30_34 = rpois(m, 40),
    votantes_35_39 = rpois(m, 40),
    votantes_40_44 = rpois(m, 35),
    votantes_45_49 = rpois(m, 35),
    votantes_50_54 = rpois(m, 30),
    votantes_55_59 = rpois(m, 30),
    votantes_60_64 = rpois(m, 20),
    votantes_65_69 = rpois(m, 20)
  )
  sf::st_sf(df, geometry = pts)
}


test_that(".br_match_calibration for census 2010 produces 7 matched groups", {
  skip_if_not_installed("sf")

  set.seed(10)
  tracts <- .mock_tracts_for_calib(8, 2010)
  elec <- .mock_electoral_for_calib(5)

  result <- interpElections:::.br_match_calibration(2010, tracts, elec)

  expect_equal(length(result$calib_zones), 7)
  expect_equal(length(result$calib_sources), 7)

  # 7 calibration zone columns
  expect_equal(result$calib_zones, c(
    "pop_18_20", "pop_21_24", "pop_25_29",
    "pop_30_39", "pop_40_49", "pop_50_59", "pop_60_69"
  ))

  # 7 calibration source columns (some aggregated)
  expect_equal(result$calib_sources, c(
    "votantes_18_20", "votantes_21_24", "votantes_25_29",
    "votantes_30_39", "votantes_40_49", "votantes_50_59",
    "votantes_60_69"
  ))

  # Aggregated votantes columns should exist in returned electoral_sf
  elec_df <- sf::st_drop_geometry(result$electoral_sf)
  expect_true("votantes_18_20" %in% names(elec_df))
  expect_true("votantes_30_39" %in% names(elec_df))
  expect_true("votantes_40_49" %in% names(elec_df))
  expect_true("votantes_50_59" %in% names(elec_df))
  expect_true("votantes_60_69" %in% names(elec_df))

  # votantes_18_20 should be aggregated from votantes_18_19 + votantes_20
  orig_elec <- sf::st_drop_geometry(elec)
  expect_equal(
    elec_df$votantes_18_20,
    orig_elec$votantes_18_19 + orig_elec$votantes_20
  )
})

test_that(".br_match_calibration for census 2000 produces same groups as 2010", {
  skip_if_not_installed("sf")

  set.seed(20)
  tracts <- .mock_tracts_for_calib(6, 2000)
  elec <- .mock_electoral_for_calib(4)

  result <- interpElections:::.br_match_calibration(2000, tracts, elec)

  expect_equal(result$calib_zones, c(
    "pop_18_20", "pop_21_24", "pop_25_29",
    "pop_30_39", "pop_40_49", "pop_50_59", "pop_60_69"
  ))
})

test_that(".br_match_calibration for census 2022 splits 18-19 and 20-24 brackets", {
  skip_if_not_installed("sf")

  set.seed(30)
  tracts <- .mock_tracts_for_calib(8, 2022)
  elec <- .mock_electoral_for_calib(5)

  result <- interpElections:::.br_match_calibration(2022, tracts, elec)

  # 7 groups for 2022 (pop_18_19 proxy + pop_20_24)
  expect_equal(length(result$calib_zones), 7)
  expect_equal(length(result$calib_sources), 7)

  expect_equal(result$calib_zones, c(
    "pop_18_19", "pop_20_24", "pop_25_29",
    "pop_30_39", "pop_40_49", "pop_50_59", "pop_60_69"
  ))

  expect_equal(result$calib_sources, c(
    "votantes_18_19", "votantes_20_24", "votantes_25_29",
    "votantes_30_39", "votantes_40_49", "votantes_50_59",
    "votantes_60_69"
  ))

  # pop_18_19 should be pop_15_19 * 2/5 (uniform distribution proxy)
  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  orig_df <- sf::st_drop_geometry(tracts)
  expect_equal(
    tracts_df$pop_18_19,
    orig_df$pop_15_19 * 2 / 5
  )

  # votantes_20_24 should be sum of votantes_20 and votantes_21_24
  elec_df <- sf::st_drop_geometry(result$electoral_sf)
  orig_elec <- sf::st_drop_geometry(elec)
  expect_equal(
    elec_df$votantes_20_24,
    orig_elec$votantes_20 + orig_elec$votantes_21_24
  )
})

test_that(".br_match_calibration aggregation is numerically correct", {
  skip_if_not_installed("sf")

  set.seed(50)
  tracts <- .mock_tracts_for_calib(4, 2010)
  elec <- .mock_electoral_for_calib(3)

  result <- interpElections:::.br_match_calibration(2010, tracts, elec)
  elec_df <- sf::st_drop_geometry(result$electoral_sf)
  orig_df <- sf::st_drop_geometry(elec)

  # votantes_18_20 = votantes_18_19 + votantes_20
  expect_equal(
    elec_df$votantes_18_20,
    orig_df$votantes_18_19 + orig_df$votantes_20
  )

  # votantes_30_39 = votantes_30_34 + votantes_35_39
  expect_equal(
    elec_df$votantes_30_39,
    orig_df$votantes_30_34 + orig_df$votantes_35_39
  )

  # votantes_60_69 = votantes_60_64 + votantes_65_69
  expect_equal(
    elec_df$votantes_60_69,
    orig_df$votantes_60_64 + orig_df$votantes_65_69
  )
})

test_that(".safe_sum handles missing columns gracefully", {
  df <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))

  # Both present
  expect_equal(interpElections:::.safe_sum(df, c("a", "b")), c(5, 7, 9))
  # Only one present
  expect_equal(interpElections:::.safe_sum(df, c("a", "nonexistent")), c(1, 2, 3))
  # None present
  expect_equal(interpElections:::.safe_sum(df, c("x", "y")), c(0, 0, 0))
})

test_that("print.interpElections_result works for Brazilian result", {
  # Create a minimal mock object with unified class
  mat <- matrix(1:6, 3, 2)
  colnames(mat) <- c("CAND_13", "CAND_22")
  obj <- list(
    interpolated = mat,
    alpha = c(1, 1.5, 2),
    sources = data.frame(id = 1:4, CAND_13 = 1:4, CAND_22 = 5:8),
    optimization = list(method = "cpu_lbfgsb", value = 100),
    interp_cols = c("CAND_13", "CAND_22"),
    code_muni = "1400100",
    year = 2008L,
    census_year = 2010L
  )
  class(obj) <- "interpElections_result"
  expect_output(print(obj), "Brazilian election")
  expect_output(print(obj), "1400100")
  expect_output(print(obj), "census 2010")
  expect_output(print(obj), "result\\$tracts_sf")
})

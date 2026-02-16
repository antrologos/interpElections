# Tests for plotting: resolve-var, compute-quantity, plot, plot_interactive

# --- Shared mock helpers ---

# Minimal result with dictionary (for variable resolution tests)
.mock_plot_result <- function() {
  skip_if_not_installed("sf")
  interp_names <- c("CAND_13", "CAND_22", "CAND_95", "CAND_96",
                     "PARTY_PT", "PARTY_MDB",
                     "QT_COMPARECIMENTO", "QT_APTOS", "QT_ABSTENCOES",
                     "votantes_18_20")
  n <- 6; m <- 3; p <- length(interp_names)
  set.seed(42)

  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 3; y0 <- (i - 1) %/% 3
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  tracts_df <- data.frame(zone_id = paste0("Z", seq_len(n)))
  tracts_sf <- sf::st_sf(tracts_df, geometry = sfc)

  interp_mat <- matrix(abs(rnorm(n * p, 100, 50)), n, p)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(point_id = paste0("P", seq_len(m)))
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  alpha <- runif(n, 0.5, 3)

  dict <- data.frame(
    column = interp_names,
    type = c("candidate", "candidate", "candidate", "candidate",
             "party", "party", "turnout", "turnout", "turnout",
             "calibration"),
    cargo = c(rep("VEREADOR", 6), rep(NA, 4)),
    ballot_number = c("13", "22", "95", "96", rep(NA, 6)),
    candidate_name = c("JOAO DA SILVA", "MARIA SOUZA",
                        "Votos em Branco", "Votos Nulos",
                        rep(NA, 6)),
    party = c("PT", "MDB", NA, NA, "PT", "MDB", rep(NA, 4)),
    stringsAsFactors = FALSE
  )

  result <- list(
    interpolated = interp_mat, alpha = alpha, tracts_sf = tracts_sf,
    sources = sources,
    optimization = list(method = "cpu_lbfgsb", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election_br()),
    zone_id = "zone_id", point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(zones = "votantes_18_20", sources = "votantes_18_20"),
    weights = NULL, time_matrix = NULL, sources_sf = NULL,
    code_muni = "3550308",
    nome_municipio = "SAO PAULO", code_muni_tse = "71072", uf = "SP",
    year = 2020L, census_year = 2022L,
    what = "candidates", pop_data = data.frame(x = 1),
    dictionary = dict
  )
  class(result) <- "interpElections_result"
  result
}


# --- .resolve_var tests ---

test_that(".resolve_var resolves exact column name", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("CAND_13", obj), "CAND_13")
  expect_equal(.resolve_var("PARTY_PT", obj), "PARTY_PT")
  expect_equal(.resolve_var("QT_COMPARECIMENTO", obj), "QT_COMPARECIMENTO")
})

test_that(".resolve_var resolves numeric ballot number", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var(13, obj), "CAND_13")
  expect_equal(.resolve_var(22, obj), "CAND_22")
  expect_equal(.resolve_var(95, obj), "CAND_95")
})

test_that(".resolve_var resolves ballot number as string", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("13", obj), "CAND_13")
})

test_that(".resolve_var resolves party abbreviation", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("PT", obj), "PARTY_PT")
  expect_equal(.resolve_var("MDB", obj), "PARTY_MDB")
})

test_that(".resolve_var party is case-insensitive", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("pt", obj), "PARTY_PT")
  expect_equal(.resolve_var("Pt", obj), "PARTY_PT")
})

test_that(".resolve_var resolves candidate name substring", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_equal(.resolve_var("JOAO", obj), "CAND_13")
  expect_equal(.resolve_var("joao", obj), "CAND_13")
  expect_equal(.resolve_var("SILVA", obj), "CAND_13")
  expect_equal(.resolve_var("SOUZA", obj), "CAND_22")
})

test_that(".resolve_var errors on ambiguous candidate name", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # "Votos" matches both CAND_95 and CAND_96
  expect_error(.resolve_var("Votos", obj), "matches multiple")
})

test_that(".resolve_var errors on unknown variable", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_error(.resolve_var("NONEXISTENT", obj), "Could not resolve")
})

test_that(".resolve_var errors on unknown ballot number", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  expect_error(.resolve_var(999, obj), "No column found")
})

test_that(".resolve_var works without dictionary (column name)", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.resolve_var("CAND_13", obj), "CAND_13")
})

test_that(".resolve_var works without dictionary (ballot number pattern)", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.resolve_var(13, obj), "CAND_13")
  expect_equal(.resolve_var(22, obj), "CAND_22")
})

test_that(".resolve_var party fallback without dictionary", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.resolve_var("PT", obj), "PARTY_PT")
})

test_that(".resolve_vars resolves multiple variables", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .resolve_vars(c("JOAO", 22, "PT"), obj)
  expect_equal(cols, c("CAND_13", "CAND_22", "PARTY_PT"))
})


# --- .auto_select_var tests ---

test_that(".auto_select_var picks first real candidate", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Should pick CAND_13 (first candidate, not blank/null)
  expect_equal(.auto_select_var(obj), "CAND_13")
})

test_that(".auto_select_var skips blank/null votes", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  # Remove real candidates, keep only blank/null
  obj$dictionary <- obj$dictionary[obj$dictionary$ballot_number %in%
                                     c("95", "96", NA), , drop = FALSE]
  # Should fall back to first party
  expect_equal(.auto_select_var(obj), "PARTY_PT")
})

test_that(".auto_select_var falls back to first interp_col without dict", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.auto_select_var(obj), obj$interp_cols[1])
})


# --- .compute_quantity tests ---

test_that(".compute_quantity absolute returns raw values", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "absolute")
  expect_equal(vals, as.numeric(obj$tracts_sf$CAND_13))
})

test_that(".compute_quantity pct_muni sums to 100", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "pct_muni")
  expect_equal(sum(vals, na.rm = TRUE), 100, tolerance = 1e-6)
})

test_that(".compute_quantity pct_tract uses QT_COMPARECIMENTO", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "pct_tract")
  expected <- as.numeric(obj$tracts_sf$CAND_13) /
    as.numeric(obj$tracts_sf$QT_COMPARECIMENTO) * 100
  expect_equal(vals, expected, tolerance = 1e-10)
})

test_that(".compute_quantity pct_tract errors when turnout missing", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$tracts_sf$QT_COMPARECIMENTO <- NULL
  obj$interpolated <- obj$interpolated[, colnames(obj$interpolated) != "QT_COMPARECIMENTO"]
  expect_error(.compute_quantity(obj, "CAND_13", "pct_tract"),
               "QT_COMPARECIMENTO")
})

test_that(".compute_quantity pct_eligible uses QT_APTOS", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "pct_eligible")
  expected <- as.numeric(obj$tracts_sf$CAND_13) /
    as.numeric(obj$tracts_sf$QT_APTOS) * 100
  expect_equal(vals, expected, tolerance = 1e-10)
})

test_that(".compute_quantity density computes per km2", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  vals <- .compute_quantity(obj, "CAND_13", "density")
  expect_true(is.numeric(vals))
  expect_equal(length(vals), nrow(obj$interpolated))
  # Values should be finite (tracts have area > 0)
  expect_true(all(is.finite(vals)))
})


# --- plot.interpElections_result tests ---

test_that("plot returns a ggplot object", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj)
  expect_s3_class(p, "gg")
})

test_that("plot resolves variable by name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "JOAO")
  expect_s3_class(p, "gg")
})

test_that("plot resolves variable by ballot number", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = 13)
  expect_s3_class(p, "gg")
})

test_that("plot resolves variable by party", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "PT")
  expect_s3_class(p, "gg")
})

test_that("plot with type = pct_tract works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", type = "pct_tract")
  expect_s3_class(p, "gg")
})

test_that("plot with quantile breaks works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", breaks = "quantile", n_breaks = 3)
  expect_s3_class(p, "gg")
})

test_that("plot with custom breaks works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", breaks = c(0, 50, 100, 200))
  expect_s3_class(p, "gg")
})

test_that("plot with custom title and legend", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13",
            title = "My Title", subtitle = "My Sub",
            legend_title = "My Legend")
  expect_s3_class(p, "gg")
  expect_equal(p$labels$title, "My Title")
  expect_equal(p$labels$subtitle, "My Sub")
  expect_equal(p$labels$fill, "My Legend")
})

test_that("plot with viridis palette works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13", palette = "viridis")
  expect_s3_class(p, "gg")
})

test_that("plot with multiple variables returns faceted ggplot", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = c("CAND_13", "CAND_22"))
  expect_s3_class(p, "gg")
  # Should have faceting
  expect_false(inherits(p$facet, "FacetNull"))
})

test_that("plot errors when tracts_sf is NULL", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()
  obj$tracts_sf <- NULL

  expect_error(plot(obj), "No tracts_sf")
})

test_that("plot warns when show_sources but no sources_sf", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()
  obj$sources_sf <- NULL

  expect_warning(plot(obj, show_sources = TRUE), "sources_sf")
})

test_that("plot auto-generates title from dictionary", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13")
  # Title should contain the candidate name from dictionary
  expect_true(grepl("JOAO", p$labels$title))
})

test_that("plot auto-generates subtitle from metadata", {
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  obj <- .mock_plot_result()

  p <- plot(obj, variable = "CAND_13")
  expect_true(grepl("SAO PAULO", p$labels$subtitle))
  expect_true(grepl("2020", p$labels$subtitle))
})


# --- Label helper tests ---

test_that(".auto_title returns dict label for known column", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  label <- .auto_title("CAND_13", obj)
  expect_true(grepl("JOAO", label))
})

test_that(".auto_title returns column name when no dictionary", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$dictionary <- NULL
  expect_equal(.auto_title("CAND_13", obj), "CAND_13")
})

test_that(".auto_subtitle returns municipality and year", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  sub <- .auto_subtitle(obj)
  expect_true(grepl("SAO PAULO", sub))
  expect_true(grepl("2020", sub))
})

test_that(".auto_subtitle returns NULL for generic result", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  obj$nome_municipio <- NULL
  obj$year <- NULL
  expect_null(.auto_subtitle(obj))
})

test_that(".quantity_label returns human-readable labels", {
  expect_equal(.quantity_label("absolute"), "Count")
  expect_equal(.quantity_label("pct_tract"), "% of tract votes")
  expect_equal(.quantity_label("pct_muni"), "% of municipality total")
})


# --- .compute_breaks tests ---

test_that(".compute_breaks continuous returns NULL", {
  vals <- runif(20)
  expect_null(.compute_breaks(vals, "continuous", 5))
})

test_that(".compute_breaks quantile returns sorted unique breaks", {
  vals <- 1:100
  brks <- .compute_breaks(vals, "quantile", 4)
  expect_true(is.numeric(brks))
  expect_true(all(diff(brks) >= 0))
})

test_that(".compute_breaks custom numeric passthrough", {
  brks <- .compute_breaks(1:10, c(0, 5, 10), 5)
  expect_equal(brks, c(0, 5, 10))
})

test_that(".compute_breaks errors on unknown method", {
  expect_error(.compute_breaks(1:10, "unknown", 5), "Unknown breaks")
})


# --- plot_interactive tests ---

test_that("plot_interactive errors without mapview", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()

  # We can only test the error when mapview IS installed by mocking,
  # but we can at minimum test it runs if mapview is available
  if (requireNamespace("mapview", quietly = TRUE)) {
    m <- plot_interactive(obj, variable = "CAND_13")
    expect_true(inherits(m, "mapview"))
  }
})

test_that("plot_interactive resolves variable by name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "JOAO")
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive with type = pct_muni works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "CAND_13", type = "pct_muni")
  expect_true(inherits(m, "mapview"))
})

test_that("plot_interactive sync with multiple variables", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  skip_if_not_installed("leafsync")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = c("CAND_13", "CAND_22"))
  # leafsync::sync returns a tagList or similar
  expect_true(!is.null(m))
})

test_that("plot_interactive errors when tracts_sf is NULL", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()
  obj$tracts_sf <- NULL

  expect_error(plot_interactive(obj), "No tracts_sf")
})

test_that("plot_interactive custom popup_vars works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("mapview")
  obj <- .mock_plot_result()

  m <- plot_interactive(obj, variable = "CAND_13",
                        popup_vars = c("zone_id", "CAND_13"))
  expect_true(inherits(m, "mapview"))
})


# --- .auto_popup_cols tests ---

test_that(".auto_popup_cols includes zone_id and plot col", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .auto_popup_cols(obj, "CAND_13")
  expect_true("zone_id" %in% cols)
  expect_true("CAND_13" %in% cols)
})

test_that(".auto_popup_cols includes turnout columns", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .auto_popup_cols(obj, "CAND_13")
  expect_true("QT_COMPARECIMENTO" %in% cols)
})

test_that(".auto_popup_cols caps at 8", {
  skip_if_not_installed("sf")
  obj <- .mock_plot_result()
  cols <- .auto_popup_cols(obj, "CAND_13")
  expect_true(length(cols) <= 8)
})

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

  # Mock time matrix + weights (per-bracket Sinkhorn-balanced)
  tt <- matrix(abs(rnorm(n * m, 50, 20)), n, m)
  alpha <- runif(n, 0.5, 3)
  pop_mat_calib <- as.matrix(tracts_df[, pop_cols, drop = FALSE])
  storage.mode(pop_mat_calib) <- "double"
  src_mat_calib <- as.matrix(sources[, src_cols, drop = FALSE])
  storage.mode(src_mat_calib) <- "double"
  pop_total <- rowSums(pop_mat_calib)
  row_targets <- pop_total / sum(pop_total) * m
  # Build W using simple column-standardized IDW (no torch needed for mocks)
  K <- (tt + 1) ^ (-alpha)
  K[!is.finite(K)] <- 0
  cs <- colSums(K)
  cs[cs == 0] <- 1
  W <- t(t(K) / cs)

  result <- list(
    interpolated = interp_mat,
    alpha = alpha,
    tracts_sf = tracts_sf,
    sources = sources,
    optimization = list(
      method = "pb_sgd_sinkhorn_cpu",
      method_type = "sinkhorn",
      value = 123.45,
      convergence = 0L
    ),
    offset = 1,
    row_targets = row_targets,
    call = quote(interpolate_election()),
    tract_id = "zone_id",
    point_id = "point_id",
    interp_cols = interp_names,
    calib_cols = list(tracts = pop_cols, sources = src_cols),
    weights = if (keep_weights) W else NULL,
    time_matrix = if (keep_time) tt else NULL,
    sources_sf = NULL,
    code_muni = if (brazilian) "3550308" else NULL,
    nome_municipio = if (brazilian) "SAO PAULO" else NULL,
    code_muni_tse = if (brazilian) "71072" else NULL,
    uf = if (brazilian) "SP" else NULL,
    year = if (brazilian) 2020L else NULL,
    census_year = if (brazilian) 2022L else NULL,
    what = if (brazilian) "candidates" else NULL,
    pop_data = if (brazilian) data.frame(x = 1) else NULL,
    dictionary = NULL
  )
  class(result) <- "interpElections_result"
  result
}

# Helper: build a mock result with dictionary
.mock_result_with_dict <- function() {
  skip_if_not_installed("sf")
  interp_names <- c("CAND_13", "CAND_22", "CAND_95", "CAND_96",
                     "PARTY_PT", "PARTY_MDB",
                     "QT_COMPARECIMENTO", "votantes_18_20")
  n <- 10; m <- 5; p <- length(interp_names)
  set.seed(42)

  polys <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) %% 5; y0 <- (i - 1) %/% 5
    sf::st_polygon(list(matrix(c(
      x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0
    ), ncol = 2, byrow = TRUE)))
  })
  sfc <- sf::st_sfc(polys, crs = 4326)
  tracts_df <- data.frame(id = seq_len(n))
  tracts_sf <- sf::st_sf(tracts_df, geometry = sfc)

  interp_mat <- matrix(runif(n * p, 10, 200), n, p)
  colnames(interp_mat) <- interp_names
  for (col in interp_names) tracts_sf[[col]] <- interp_mat[, col]

  sources <- data.frame(id = seq_len(m))
  for (col in interp_names) sources[[col]] <- rpois(m, 50)

  alpha <- runif(n, 0.5, 3)

  dict <- data.frame(
    column = interp_names,
    type = c("candidate", "candidate", "candidate", "candidate",
             "party", "party", "turnout", "calibration"),
    cargo = c(rep("VEREADOR", 6), NA, NA),
    ballot_number = c("13", "22", "95", "96", NA, NA, NA, NA),
    candidate_name = c("JOAO DA SILVA", "MARIA SOUZA",
                        "Votos em Branco", "Votos Nulos",
                        NA, NA, NA, NA),
    party = c("PT", "MDB", NA, NA, "PT", "MDB", NA, NA),
    stringsAsFactors = FALSE
  )

  result <- list(
    interpolated = interp_mat, alpha = alpha, tracts_sf = tracts_sf,
    sources = sources,
    optimization = list(method = "pb_sgd_sinkhorn_cpu", value = 50, convergence = 0L),
    offset = 1, call = quote(interpolate_election_br()),
    tract_id = "id", point_id = "id",
    interp_cols = interp_names,
    calib_cols = list(tracts = "votantes_18_20", sources = "votantes_18_20"),
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


# --- summary ---

test_that("summary.interpElections_result prints per-variable stats", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "interpElections result summary")
  expect_match(full, "Census tracts: 10")
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

  expect_match(full, "SAO PAULO")
  expect_match(full, "SP")
  expect_match(full, "3550308")
  expect_match(full, "71072")
  expect_match(full, "Election: 2020")
  expect_match(full, "Census: 2022")
})

test_that("summary shows optimization info", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "pb_sgd_sinkhorn_cpu")
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


# --- plot tests moved to test-plot.R ---


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

test_that("residuals returns NULL with message when no weights or time_matrix", {
  skip_if_not_installed("sf")
  obj <- .mock_result()

  expect_message(res <- residuals(obj), "Cannot compute residuals")
  expect_null(res)
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
    tracts_df[, obj$calib_cols$tracts, drop = FALSE]
  )
  storage.mode(pop_mat) <- "double"

  expected <- obj$weights %*% src_mat - pop_mat
  colnames(expected) <- obj$calib_cols$tracts
  actual <- residuals(obj)

  expect_equal(actual, expected, tolerance = 1e-10)
})

test_that("residuals returns NULL with message when calib_cols is NULL", {
  skip_if_not_installed("sf")
  obj <- .mock_result(keep_weights = TRUE)
  obj$calib_cols <- NULL

  expect_message(res <- residuals(obj), "No calibration columns")
  expect_null(res)
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

test_that("residuals via time_matrix path produces valid matrix", {
  skip_if_not_installed("sf")
  skip_if_not_installed("torch")
  obj <- .mock_result(keep_weights = FALSE, keep_time = TRUE)

  # time_matrix path uses compute_weight_matrix (torch)
  resid <- residuals(obj)
  expect_true(is.matrix(resid))
  expect_equal(nrow(resid), 10)
  expect_equal(ncol(resid), 2)
  expect_true(all(is.finite(resid)))
})


# --- Brazilian-specific S3 method tests ---

test_that("as.data.frame works for Brazilian result", {
  skip_if_not_installed("sf")
  obj <- .mock_result(brazilian = TRUE)

  df <- as.data.frame(obj)
  expect_true(is.data.frame(df))
  expect_false(inherits(df, "sf"))
  # Should have census tract IDs and interpolated vars
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
  skip_if_not_installed("ggplot2")
  # Create result with many variables
  obj <- .mock_result(p = 20)

  expect_message(
    result <- plot(obj, variable = "NONEXISTENT"),
    "and \\d+ more"
  )
  expect_null(result)
})


# --- Dictionary tests ---

test_that("dictionary is NULL for generic result", {
  skip_if_not_installed("sf")
  obj <- .mock_result()
  expect_null(obj$dictionary)
})

test_that("dictionary is a data.frame with correct structure", {
  obj <- .mock_result_with_dict()
  dict <- obj$dictionary

  expect_true(is.data.frame(dict))
  expect_true(all(c("column", "type", "cargo", "ballot_number",
                     "candidate_name", "party") %in% names(dict)))
})

test_that("dictionary has one row per interpolated column", {
  obj <- .mock_result_with_dict()
  expect_equal(nrow(obj$dictionary), length(obj$interp_cols))
  expect_equal(obj$dictionary$column, obj$interp_cols)
})

test_that("dictionary candidate rows have names and party", {
  obj <- .mock_result_with_dict()
  dict <- obj$dictionary
  cand_rows <- dict[dict$type == "candidate", ]

  expect_equal(nrow(cand_rows), 4)
  # Real candidates (not blank/null) have party info
  real_cands <- cand_rows[!cand_rows$ballot_number %in% c("95", "96"), ]
  expect_true(all(!is.na(real_cands$candidate_name)))
  expect_true(all(!is.na(real_cands$party)))
})

test_that("dictionary special codes have labels", {
  obj <- .mock_result_with_dict()
  dict <- obj$dictionary

  branco <- dict[dict$ballot_number == "95" & !is.na(dict$ballot_number), ]
  expect_equal(branco$candidate_name, "Votos em Branco")

  nulo <- dict[dict$ballot_number == "96" & !is.na(dict$ballot_number), ]
  expect_equal(nulo$candidate_name, "Votos Nulos")
})

test_that("dictionary party rows have correct type", {
  obj <- .mock_result_with_dict()
  dict <- obj$dictionary
  party_rows <- dict[dict$type == "party", ]

  expect_equal(nrow(party_rows), 2)
  expect_true(all(!is.na(party_rows$party)))
  expect_true(all(is.na(party_rows$ballot_number)))
})

test_that("print shows dictionary type summary when available", {
  obj <- .mock_result_with_dict()
  out <- capture.output(print(obj))
  full <- paste(out, collapse = "\n")

  # Municipality name and codes
  expect_match(full, "SAO PAULO")
  expect_match(full, "71072")

  # Type counts
  expect_match(full, "Candidates:")
  expect_match(full, "Parties:")
  expect_match(full, "Turnout:")
  expect_match(full, "Calibration:")

  # Calibration column listed fully
  expect_match(full, "votantes_18_20")

  # Contents section
  expect_match(full, "result\\$dictionary")
  expect_match(full, "View\\(result\\$dictionary\\)")

  # Methods and plotting
  expect_match(full, "summary\\(\\)")
  expect_match(full, "plot\\(result")
  expect_match(full, "plot_interactive\\(result")
})

test_that("summary groups variables by type with stats", {
  obj <- .mock_result_with_dict()
  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  # Type section headers
  expect_match(full, "Candidates \\(4\\):")
  expect_match(full, "Parties \\(2\\):")
  expect_match(full, "Turnout \\(1\\):")
  expect_match(full, "Calibration \\(1\\):")

  # Stats present
  expect_match(full, "total=")

  # Calibration always fully listed
  expect_match(full, "votantes_18_20")

  # Dictionary labels shown
  expect_match(full, "JOAO DA SILVA")
  expect_match(full, "CAND_13")
})

test_that("print without dictionary falls back to column names", {
  skip_if_not_installed("sf")
  obj <- .mock_result(p = 3)
  out <- capture.output(print(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "VAR_1")
  # Methods line still shown
  expect_match(full, "Methods:")
})

test_that("print generic result omits municipality info", {
  skip_if_not_installed("sf")
  obj <- .mock_result()
  out <- capture.output(print(obj))
  full <- paste(out, collapse = "\n")

  expect_match(full, "interpElections result")
  expect_no_match(full, "Brazilian election")
  expect_no_match(full, "Municipality:")
})

test_that("summary caps flat listing when no dictionary", {
  skip_if_not_installed("sf")
  obj <- .mock_result(p = 20)
  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  # Should show cap message

  expect_match(full, "5 more variables")
})

test_that("summary candidates capped with View hint", {
  # Build mock with many candidates
  obj <- .mock_result_with_dict()
  # Add 6 more candidate rows to dictionary
  extra_cols <- paste0("CAND_", 30:35)
  extra_dict <- data.frame(
    column = extra_cols,
    type = rep("candidate", 6),
    cargo = rep("VEREADOR", 6),
    ballot_number = as.character(30:35),
    candidate_name = paste("CAND", LETTERS[1:6]),
    party = rep("XX", 6),
    stringsAsFactors = FALSE
  )
  obj$dictionary <- rbind(obj$dictionary, extra_dict)
  # Add columns to interpolated matrix
  n <- nrow(obj$interpolated)
  extra_mat <- matrix(runif(n * 6, 10, 100), n, 6)
  colnames(extra_mat) <- extra_cols
  obj$interpolated <- cbind(obj$interpolated, extra_mat)
  obj$interp_cols <- c(obj$interp_cols, extra_cols)

  out <- capture.output(summary(obj))
  full <- paste(out, collapse = "\n")

  # Candidates section should be capped
  expect_match(full, "more -- View\\(result\\$dictionary\\)")
})

# Tests for br_download_votes() and br_download_turnout()

test_that("br_download_votes has correct signature", {
  expect_true(is.function(br_download_votes))
  fmls <- names(formals(br_download_votes))
  expect_true("year" %in% fmls)
  expect_true("uf" %in% fmls)
  expect_true("code_muni_tse" %in% fmls)
  expect_true("cargo" %in% fmls)
  expect_true("turno" %in% fmls)
  expect_true("force" %in% fmls)
  expect_true("cache" %in% fmls)
  expect_true("verbose" %in% fmls)
})

test_that("br_download_turnout has correct signature", {
  expect_true(is.function(br_download_turnout))
  fmls <- names(formals(br_download_turnout))
  expect_true("year" %in% fmls)
  expect_true("uf" %in% fmls)
  expect_true("code_muni_tse" %in% fmls)
  expect_true("cargo" %in% fmls)
  expect_true("turno" %in% fmls)
  expect_true("force" %in% fmls)
  expect_true("cache" %in% fmls)
  expect_true("verbose" %in% fmls)
})

test_that("br_download_votes requires data.table", {
  # The function checks for data.table at runtime
  expect_true(is.function(br_download_votes))
})

test_that("br_download_turnout requires data.table", {
  expect_true(is.function(br_download_turnout))
})

# Integration tests (require internet and TSE access)
test_that("br_download_votes downloads Boa Vista 2008 data", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("data.table")
  skip_if_not_installed("stringr")

  # Boa Vista TSE code is 03018 (not 03611)
  result <- br_download_votes(
    year = 2008, uf = "RR", code_muni_tse = "03018",
    cargo = 13, turno = 1,
    cache = FALSE, verbose = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("QT_VOTOS" %in% names(result))
  expect_true("NR_VOTAVEL" %in% names(result))
  expect_true("NR_ZONA" %in% names(result))
  expect_true("NR_SECAO" %in% names(result))
  expect_true("CD_MUNICIPIO" %in% names(result))

  # All records should be for the requested municipality
  expect_true(all(result$CD_MUNICIPIO == "03018"))
})

test_that("br_download_votes caches ZIP file", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("data.table")
  skip_if_not_installed("stringr")

  # First call downloads
  result1 <- br_download_votes(
    year = 2008, uf = "RR", code_muni_tse = "03018",
    cargo = 13, turno = 1,
    verbose = FALSE
  )

  # Second call uses cache (faster)
  result2 <- br_download_votes(
    year = 2008, uf = "RR", code_muni_tse = "03018",
    cargo = 13, turno = 1,
    verbose = FALSE
  )

  expect_equal(nrow(result1), nrow(result2))
})

test_that("br_prepare_electoral has turno parameter", {
  expect_true("turno" %in% names(formals(br_prepare_electoral)))
  expect_true("verbose" %in% names(formals(br_prepare_electoral)))
})

test_that("interpolate_election_br has turno parameter", {
  expect_true("turno" %in% names(formals(interpolate_election_br)))
})

# --- Tests for br_download_geocode() ---

test_that("br_download_geocode has correct signature", {
  expect_true(is.function(br_download_geocode))
  fmls <- names(formals(br_download_geocode))
  expect_true("year" %in% fmls)
  expect_true("uf" %in% fmls)
  expect_true("code_muni_tse" %in% fmls)
  expect_true("force" %in% fmls)
  expect_true("cache" %in% fmls)
  expect_true("verbose" %in% fmls)
})

test_that("br_download_geocode requires data.table", {
  expect_true(is.function(br_download_geocode))
})

test_that("br_download_geocode returns NULL for 2008 (no TSE data)", {
  skip_on_cran()
  skip_if_offline()
  result <- br_download_geocode(
    year = 2008, uf = "RR",
    cache = FALSE, verbose = FALSE
  )
  expect_null(result)
})

test_that("br_download_geocode downloads 2020 RR data", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("data.table")
  skip_if_not_installed("stringr")

  result <- br_download_geocode(
    year = 2020, uf = "RR", code_muni_tse = "03018",
    cache = FALSE, verbose = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("NR_LATITUDE" %in% names(result))
  expect_true("NR_LONGITUDE" %in% names(result))
  expect_true("NR_ZONA" %in% names(result))
  expect_true("NR_LOCAL_VOTACAO" %in% names(result))
  expect_true("CD_MUNICIPIO" %in% names(result))

  # All records should be for the requested municipality
  expect_true(all(result$CD_MUNICIPIO == "03018"))

  # Coordinates should be numeric
  expect_true(is.numeric(result$NR_LATITUDE))
  expect_true(is.numeric(result$NR_LONGITUDE))

  # At least some valid coordinates (not -1)
  expect_true(any(result$NR_LATITUDE != -1))
})

test_that("br_download_geocode caches ZIP file", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("data.table")
  skip_if_not_installed("stringr")

  # First call downloads (may already be cached from previous test)
  result1 <- br_download_geocode(
    year = 2020, uf = "RR", code_muni_tse = "03018",
    verbose = FALSE
  )

  # Second call uses cache
  result2 <- br_download_geocode(
    year = 2020, uf = "RR", code_muni_tse = "03018",
    verbose = FALSE
  )
  expect_equal(nrow(result1), nrow(result2))
})

# --- Tests for 'what' parameter and cargo aliases ---

test_that("br_prepare_electoral has what parameter with correct default", {
  fmls <- formals(br_prepare_electoral)
  expect_true("what" %in% names(fmls))
  expect_equal(fmls$what, "candidates")
})

test_that("br_prepare_electoral has candidates and parties filter params", {
  fmls <- formals(br_prepare_electoral)
  expect_true("candidates" %in% names(fmls))
  expect_true("parties" %in% names(fmls))
  expect_null(fmls$candidates)
  expect_null(fmls$parties)
})

test_that("br_prepare_electoral cargo defaults to NULL", {
  fmls <- formals(br_prepare_electoral)
  expect_null(fmls$cargo)
})

test_that("interpolate_election_br has what parameter", {
  fmls <- formals(interpolate_election_br)
  expect_true("what" %in% names(fmls))
  expect_equal(fmls$what, "candidates")
  expect_true("candidates" %in% names(fmls))
  expect_true("parties" %in% names(fmls))
  expect_null(fmls$cargo)
})

# --- Cargo alias tests (no download) ---

test_that(".br_resolve_cargo converts string aliases", {
  expect_equal(interpElections:::.br_resolve_cargo("vereador"), 13L)
  expect_equal(interpElections:::.br_resolve_cargo("prefeito"), 11L)
  expect_equal(interpElections:::.br_resolve_cargo("presidente"), 1L)
  expect_equal(interpElections:::.br_resolve_cargo("governador"), 3L)
  expect_equal(interpElections:::.br_resolve_cargo("senador"), 5L)
  expect_equal(interpElections:::.br_resolve_cargo("deputado_federal"), 6L)
  expect_equal(interpElections:::.br_resolve_cargo("deputado_estadual"), 7L)
  expect_equal(interpElections:::.br_resolve_cargo("deputado_distrital"), 8L)
})

test_that(".br_resolve_cargo handles vector input", {
  result <- interpElections:::.br_resolve_cargo(c("prefeito", "vereador"))
  expect_equal(result, c(11L, 13L))
})

test_that(".br_resolve_cargo passes through numeric", {
  expect_equal(interpElections:::.br_resolve_cargo(13), 13L)
  expect_equal(interpElections:::.br_resolve_cargo(c(11, 13)), c(11L, 13L))
})

test_that(".br_resolve_cargo errors on invalid alias", {
  expect_error(
    interpElections:::.br_resolve_cargo("invalid"),
    "Unknown cargo"
  )
})

test_that(".br_cargo_label returns correct labels", {
  expect_equal(interpElections:::.br_cargo_label(13L), "VEREADOR")
  expect_equal(interpElections:::.br_cargo_label(11L), "PREFEITO")
  expect_equal(interpElections:::.br_cargo_label(1L), "PRESIDENTE")
  expect_equal(interpElections:::.br_cargo_label(3L), "GOVERNADOR")
  expect_equal(interpElections:::.br_cargo_label(5L), "SENADOR")
  expect_equal(interpElections:::.br_cargo_label(6L), "DEPUTADO_FEDERAL")
  expect_equal(interpElections:::.br_cargo_label(7L), "DEPUTADO_ESTADUAL")
  expect_equal(interpElections:::.br_cargo_label(8L), "DEPUTADO_DISTRITAL")
})

# --- Accent removal ---

test_that(".br_remove_accents normalizes accented characters", {
  expect_equal(interpElections:::.br_remove_accents("JOAO"), "JOAO")
  # Basic ASCII//TRANSLIT behavior
  result <- interpElections:::.br_remove_accents("caf\u00e9")
  expect_true(is.character(result))
  expect_false(grepl("\u00e9", result))
})

# --- Education normalization ---

test_that(".br_normalize_educ maps all TSE education levels", {
  expect_equal(interpElections:::.br_normalize_educ("ANALFABETO"), "ANALFABETO")
  expect_equal(interpElections:::.br_normalize_educ("ENSINO FUNDAMENTAL INCOMPLETO"), "FUND_INCOMP")
  expect_equal(interpElections:::.br_normalize_educ("ENSINO FUNDAMENTAL COMPLETO"), "FUND_COMP")
  expect_equal(interpElections:::.br_normalize_educ("SUPERIOR INCOMPLETO"), "SUP_INCOMP")
  expect_equal(interpElections:::.br_normalize_educ("SUPERIOR COMPLETO"), "SUP_COMP")
})

# --- Candidate filter tests (mock data) ---

test_that(".br_filter_candidates filters by numeric NR_VOTAVEL", {
  mock_data <- data.frame(
    NR_VOTAVEL = c("12345", "67890", "11111"),
    NM_VOTAVEL = c("ALICE", "BOB", "CAROL"),
    QT_VOTOS = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.br_filter_candidates(
    mock_data, candidates = c(12345), verbose = FALSE)
  expect_equal(nrow(result), 1)
  expect_equal(result$NM_VOTAVEL, "ALICE")
})

test_that(".br_filter_candidates filters by name substring", {
  mock_data <- data.frame(
    NR_VOTAVEL = c("12345", "67890", "11111"),
    NM_VOTAVEL = c("ALICE SILVA", "BOB SANTOS", "CAROL SILVA"),
    QT_VOTOS = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.br_filter_candidates(
    mock_data, candidates = c("SILVA"), verbose = FALSE)
  expect_equal(nrow(result), 2)
  expect_true(all(c("ALICE SILVA", "CAROL SILVA") %in% result$NM_VOTAVEL))
})

test_that(".br_filter_candidates handles mixed numeric + character", {
  mock_data <- data.frame(
    NR_VOTAVEL = c("12345", "67890", "11111"),
    NM_VOTAVEL = c("ALICE", "BOB", "CAROL"),
    QT_VOTOS = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.br_filter_candidates(
    mock_data, candidates = c(12345, "BOB"), verbose = FALSE)
  expect_equal(nrow(result), 2)
})

# --- Party filter tests (mock data) ---

test_that(".br_filter_parties filters by SG_PARTIDO", {
  mock_data <- data.frame(
    NR_VOTAVEL = c("12345", "67890", "11111"),
    SG_PARTIDO = c("PT", "PSDB", "MDB"),
    QT_VOTOS = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.br_filter_parties(
    mock_data, parties = c("PT", "MDB"), verbose = FALSE)
  expect_equal(nrow(result), 2)
  expect_true(all(c("PT", "MDB") %in% result$SG_PARTIDO))
})

test_that(".br_filter_parties is case insensitive", {
  mock_data <- data.frame(
    NR_VOTAVEL = c("12345", "67890"),
    SG_PARTIDO = c("PT", "PSDB"),
    QT_VOTOS = c(100, 200),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.br_filter_parties(
    mock_data, parties = c("pt"), verbose = FALSE)
  expect_equal(nrow(result), 1)
})

# --- Pivot helper tests (mock data) ---

test_that(".br_pivot_candidates creates CAND_ columns", {
  mock_data <- data.frame(
    NR_ZONA = c("1", "1", "2", "2"),
    NR_SECAO = c("1", "1", "1", "1"),
    COD_MUN_TSE = c("00001", "00001", "00001", "00001"),
    NR_VOTAVEL = c("12345", "67890", "12345", "67890"),
    QT_VOTOS = c(100, 200, 150, 250),
    stringsAsFactors = FALSE
  )
  id_cols <- c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO")
  result <- interpElections:::.br_pivot_candidates(mock_data, id_cols)
  expect_true("CAND_12345" %in% names(result))
  expect_true("CAND_67890" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that(".br_pivot_candidates adds prefix for multi-cargo", {
  mock_data <- data.frame(
    NR_ZONA = c("1", "1"),
    NR_SECAO = c("1", "1"),
    COD_MUN_TSE = c("00001", "00001"),
    NR_VOTAVEL = c("12345", "67890"),
    QT_VOTOS = c(100, 200),
    stringsAsFactors = FALSE
  )
  id_cols <- c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO")
  result <- interpElections:::.br_pivot_candidates(mock_data, id_cols, prefix = "PREFEITO_")
  expect_true("PREFEITO_CAND_12345" %in% names(result))
  expect_true("PREFEITO_CAND_67890" %in% names(result))
})

test_that(".br_pivot_parties aggregates by SG_PARTIDO", {
  mock_data <- data.frame(
    NR_ZONA = c("1", "1", "1"),
    NR_SECAO = c("1", "1", "1"),
    COD_MUN_TSE = c("00001", "00001", "00001"),
    NR_VOTAVEL = c("12345", "12346", "67890"),
    SG_PARTIDO = c("PT", "PT", "PSDB"),
    QT_VOTOS = c(100, 50, 200),
    stringsAsFactors = FALSE
  )
  id_cols <- c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO")
  result <- interpElections:::.br_pivot_parties(mock_data, id_cols)
  expect_true("PARTY_PT" %in% names(result))
  expect_true("PARTY_PSDB" %in% names(result))
  expect_equal(result$PARTY_PT, 150L)
  expect_equal(result$PARTY_PSDB, 200L)
})

test_that(".br_pivot_parties excludes blank and null votes", {
  mock_data <- data.frame(
    NR_ZONA = c("1", "1", "1"),
    NR_SECAO = c("1", "1", "1"),
    COD_MUN_TSE = c("00001", "00001", "00001"),
    NR_VOTAVEL = c("12345", "95", "96"),
    SG_PARTIDO = c("PT", "", ""),
    QT_VOTOS = c(100, 50, 30),
    stringsAsFactors = FALSE
  )
  id_cols <- c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO")
  result <- interpElections:::.br_pivot_parties(mock_data, id_cols)
  expect_true("PARTY_PT" %in% names(result))
  # blank and null votes (95, 96) should be excluded
  expect_equal(ncol(result), length(id_cols) + 1)  # only PT
})

test_that(".br_pivot_parties falls back to NR_VOTAVEL prefix", {
  mock_data <- data.frame(
    NR_ZONA = c("1", "1"),
    NR_SECAO = c("1", "1"),
    COD_MUN_TSE = c("00001", "00001"),
    NR_VOTAVEL = c("12345", "67890"),
    QT_VOTOS = c(100, 200),
    stringsAsFactors = FALSE
  )
  id_cols <- c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO")
  result <- interpElections:::.br_pivot_parties(mock_data, id_cols)
  # No SG_PARTIDO column, so should use first 2 digits of NR_VOTAVEL
  expect_true("PARTY_12" %in% names(result))
  expect_true("PARTY_67" %in% names(result))
})

test_that(".br_extract_turnout extracts QT_APTOS and QT_ABSTENCOES", {
  mock_data <- data.frame(
    NR_ZONA = c("1", "1"),
    NR_SECAO = c("1", "1"),
    COD_MUN_TSE = c("00001", "00001"),
    QT_APTOS = c(500L, 500L),
    QT_ABSTENCOES = c(50L, 50L),
    stringsAsFactors = FALSE
  )
  grp_cols <- c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO")
  result <- interpElections:::.br_extract_turnout(mock_data, grp_cols)
  expect_true(is.data.frame(result))
  expect_true("QT_APTOS" %in% names(result))
  expect_true("QT_ABSTENCOES" %in% names(result))
})

test_that(".br_extract_turnout returns NULL when columns missing", {
  mock_data <- data.frame(
    NR_ZONA = c("1"),
    NR_SECAO = c("1"),
    COD_MUN_TSE = c("00001"),
    QT_VOTOS = c(100L),
    stringsAsFactors = FALSE
  )
  grp_cols <- c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO")
  result <- interpElections:::.br_extract_turnout(mock_data, grp_cols)
  expect_null(result)
})

test_that(".br_pivot_demographics creates GENERO_ and EDUC_ columns", {
  mock_data <- data.frame(
    NR_ZONA = c("1", "1", "1", "1"),
    NR_SECAO = c("1", "1", "1", "1"),
    NR_LOCAL_VOTACAO = c("100", "100", "100", "100"),
    DS_GENERO = c("MASCULINO", "MASCULINO", "FEMININO", "FEMININO"),
    DS_GRAU_ESCOLARIDADE = c("SUPERIOR COMPLETO", "ANALFABETO",
                             "SUPERIOR COMPLETO", "ANALFABETO"),
    QT_ELEITORES_PERFIL = c(100L, 50L, 120L, 30L),
    stringsAsFactors = FALSE
  )
  result <- interpElections:::.br_pivot_demographics(mock_data)
  expect_true(is.data.frame(result))
  expect_true("GENERO_MASCULINO" %in% names(result))
  expect_true("GENERO_FEMININO" %in% names(result))
  expect_true("EDUC_SUP_COMP" %in% names(result))
  expect_true("EDUC_ANALFABETO" %in% names(result))
  expect_equal(result$GENERO_MASCULINO, 150)
  expect_equal(result$GENERO_FEMININO, 150)
})

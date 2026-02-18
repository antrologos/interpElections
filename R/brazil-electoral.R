#' Prepare Brazilian electoral data at the polling-location level
#'
#' Downloads TSE voter profile, attendance, and candidate vote data, merges
#' them with geocoded polling station coordinates, and aggregates to the
#' voting-location level. Returns a data frame ready for interpolation.
#'
#' Most users should use [interpolate_election_br()] instead, which calls
#' this function internally and then runs the full interpolation pipeline.
#' Use `br_prepare_electoral()` directly when you need the raw
#' polling-location data without interpolation.
#'
#' When `votacao_path` and `comparecimento_path` are NULL, data is
#' auto-downloaded from the official TSE open data portal using
#' [br_download_votes()] and [br_download_turnout()].
#'
#' @param code_muni_ibge Character. 7-digit IBGE municipality code
#'   (e.g., `"3550308"` for Sao Paulo).
#' @param code_muni_tse Character. 5-digit TSE municipality code
#'   (e.g., `"71072"` for Sao Paulo). Typically obtained via
#'   [interpolate_election_br()], which resolves this automatically.
#' @param uf Character. Two-letter state abbreviation (e.g., `"SP"`,
#'   `"RJ"`, `"MG"`).
#' @param year Integer. Election year. See [interpolate_election_br()]
#'   for the distinction between municipal and general election years.
#' @param cargo Integer, character, or NULL. Which electoral office(s) to
#'   include. Accepts human-readable aliases (case-insensitive), TSE
#'   numeric codes, or a vector of either:
#'
#'   | Alias | TSE code | Election type |
#'   | --- | --- | --- |
#'   | `"presidente"` | 1 | General |
#'   | `"governador"` | 3 | General |
#'   | `"senador"` | 5 | General |
#'   | `"deputado_federal"` | 6 | General |
#'   | `"deputado_estadual"` | 7 | General |
#'   | `"deputado_distrital"` | 8 | General (DF only) |
#'   | `"prefeito"` | 11 | Municipal |
#'   | `"vereador"` | 13 | Municipal |
#'
#'   When `NULL` (the default), all offices present in the data file are
#'   included. For general election years, this automatically downloads
#'   the national file for presidential data.
#'
#'   When multiple cargos are selected, output columns are prefixed
#'   (e.g., `PREFEITO_CAND_*`, `VEREADOR_CAND_*`).
#' @param turno Integer. Election round: `1` (first round, the default)
#'   or `2` (runoff). The runoff only exists for presidente, governador,
#'   and prefeito (in cities with >200k voters), and only when no
#'   candidate wins outright in the first round.
#' @param what Character vector. Controls what data columns are produced
#'   in the output. One or more of:
#'
#'   - `"candidates"` **(default)**: One column per candidate with their
#'     vote counts, named `CAND_<ballot_number>`. Also includes
#'     `QT_COMPARECIMENTO` (total turnout). Special ballot numbers:
#'     95 = blank votes (em branco), 96 = null votes (nulo).
#'   - `"parties"`: One column per party with total votes, named
#'     `PARTY_<abbreviation>` (e.g., `PARTY_PT`, `PARTY_MDB`).
#'     Also includes `QT_COMPARECIMENTO`.
#'   - `"turnout"`: Turnout statistics: `QT_COMPARECIMENTO` (voters who
#'     showed up), `QT_APTOS` (eligible voters), and `QT_ABSTENCOES`
#'     (abstentions), when available in the data.
#'   - `"demographics"`: Voter profile by gender (`GENERO_FEMININO`,
#'     `GENERO_MASCULINO`, `GENERO_NAO_INFORMADO`) and education level
#'     (`EDUC_ANALFABETO`, `EDUC_LE_ESCREVE`, `EDUC_FUND_INCOMP`,
#'     `EDUC_FUND_COMP`, `EDUC_MEDIO_INCOMP`, `EDUC_MEDIO_COMP`,
#'     `EDUC_SUP_INCOMP`, `EDUC_SUP_COMP`, `EDUC_NAO_INFORMADO`).
#'     Note: demographics come from the voter registration profile, not
#'     from vote data, so no `cargo` filter applies.
#'
#'   Values can be combined:
#'   `what = c("candidates", "parties", "turnout", "demographics")`.
#' @param candidates Character or numeric vector, or NULL. Filter
#'   specific candidates (only applies when `"candidates" %in% what`):
#'
#'   - **Numeric values** match the candidate's ballot number exactly.
#'     Example: `candidates = c(13, 22)`.
#'   - **Character values** perform accent-normalized, case-insensitive
#'     substring matching against the candidate's registered name.
#'     Example: `candidates = "LULA"` matches
#'     "LUIZ INACIO LULA DA SILVA".
#'   - **NULL** (the default): all candidates are kept.
#' @param parties Character vector or NULL. Filter specific parties
#'   (only applies when `"parties" %in% what`). Uses official TSE party
#'   abbreviations, matched case-insensitively. Example:
#'   `parties = c("PT", "PL", "MDB")`.
#'   **NULL** (the default): all parties are kept.
#' @param perfil_path Character or NULL. Path to a local voter profile
#'   CSV file. If NULL (the default), downloads from TSE.
#' @param comparecimento_path Character or NULL. Path to attendance
#'   parquet file. If NULL (the default), turnout is computed from the
#'   vote data itself.
#' @param votacao_path Character or NULL. Path to candidate votes parquet
#'   file. If NULL (the default), downloads from TSE.
#' @param geocode_path Character or NULL. Path to a CSV with geocoded
#'   polling station coordinates (columns: `nr_zona`,
#'   `nr_local_votacao`, `lat`, `long`). If NULL, coordinates are
#'   obtained from TSE and Danny Hidalgo's geocoding project.
#' @param cache Logical. If TRUE (default), downloaded files are stored
#'   persistently. See [get_interpElections_cache_dir()].
#' @param force Logical. Re-download even if cached file exists.
#'   Default: FALSE.
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return A data frame with one row per **voting location** (not per
#'   polling section). Contains the following column groups:
#'
#'   **Always present:**
#'   - `lat`, `long`: Coordinates of the voting location.
#'   - `id`: Sequential integer ID.
#'   - `votantes_18_20`, ..., `votantes_65_69`: Registered voters per
#'     age bracket at this location (used as calibration data).
#'   - `vot_hom_alf_*`, `vot_hom_nalf_*`, `vot_mul_alf_*`,
#'     `vot_mul_nalf_*`: Cross-tabulated voters by gender, literacy,
#'     and age bracket (used for "full" calibration mode). 48 columns
#'     total (12 age groups x 4 gender-literacy categories).
#'
#'   **Conditional on `what`:**
#'   - `"candidates"`: `CAND_<number>` columns + `QT_COMPARECIMENTO`.
#'   - `"parties"`: `PARTY_<abbrev>` columns + `QT_COMPARECIMENTO`.
#'   - `"turnout"`: `QT_COMPARECIMENTO`, `QT_APTOS`, `QT_ABSTENCOES`.
#'   - `"demographics"`: `GENERO_*` and `EDUC_*` columns.
#'
#'   When multiple `cargo` values are selected, candidate and party
#'   columns are prefixed (e.g., `PREFEITO_CAND_45`, `VEREADOR_PARTY_PT`).
#'
#' @details
#' ## Data sources
#'
#' All data is downloaded from the TSE open data portal at
#' `https://cdn.tse.jus.br/estatistica/sead/odsele/`.
#'
#' Presidential vote data (cargo 1) is published in a separate national
#' file (`votacao_secao_<year>_BR.zip`, ~250 MB) rather than in per-state
#' files. This function detects when presidential data is needed and
#' downloads the national file automatically.
#'
#' Geocoded polling station coordinates come from two sources: official
#' TSE data (available from ~2020 onwards, with some missing coordinates)
#' and Danny Hidalgo's geocoding project
#' (<https://github.com/fdhidalgo/geocode_br_polling_stations>), which
#' covers earlier years. TSE coordinates take priority when available.
#'
#' ## Dependencies
#'
#' Requires `dplyr`, `tidyr`, `data.table`, and `stringr` packages.
#' When reading local parquet files, the `arrow` package is also required.
#'
#' @examples
#' \dontrun{
#' # ── Basic usage ────────────────────────────────────────────────
#' # All candidates in the 2020 municipal election for Boa Vista
#' elec <- br_prepare_electoral(
#'   code_muni_ibge = "1400100",
#'   code_muni_tse  = "01120",
#'   uf = "RR",
#'   year = 2020,
#'   what = c("candidates", "turnout")
#' )
#'
#' # ── Specific cargo ─────────────────────────────────────────────
#' # Only the presidential race in 2022
#' elec <- br_prepare_electoral(
#'   code_muni_ibge = "3170701",
#'   code_muni_tse  = "54135",
#'   uf = "MG",
#'   year = 2022,
#'   cargo = "presidente"
#' )
#'
#' # ── Filter specific candidates ─────────────────────────────────
#' # Only Lula and Bolsonaro in the 2022 presidential race
#' elec <- br_prepare_electoral(
#'   code_muni_ibge = "3170701",
#'   code_muni_tse  = "54135",
#'   uf = "MG",
#'   year = 2022,
#'   cargo = "presidente",
#'   candidates = c(13, 22)
#' )
#'
#' # ── Party vote totals ──────────────────────────────────────────
#' elec <- br_prepare_electoral(
#'   code_muni_ibge = "3170701",
#'   code_muni_tse  = "54135",
#'   uf = "MG",
#'   year = 2020,
#'   cargo = "vereador",
#'   what = "parties",
#'   parties = c("PT", "MDB", "PL")
#' )
#'
#' # ── Voter demographics ─────────────────────────────────────────
#' elec <- br_prepare_electoral(
#'   code_muni_ibge = "3170701",
#'   code_muni_tse  = "54135",
#'   uf = "MG",
#'   year = 2022,
#'   what = "demographics"
#' )
#' # -> GENERO_FEMININO, GENERO_MASCULINO, EDUC_SUP_COMP, ...
#' }
#'
#' @family Brazil helpers
#'
#' @seealso [interpolate_election_br()] for the one-step wrapper,
#'   [br_download_votes()], [br_download_turnout()],
#'   [interpElections_cache()]
#'
#' @export
br_prepare_electoral <- function(
    code_muni_ibge,
    code_muni_tse,
    uf,
    year,
    cargo = NULL,
    turno = 1L,
    what = "candidates",
    candidates = NULL,
    parties = NULL,
    perfil_path = NULL,
    comparecimento_path = NULL,
    votacao_path = NULL,
    geocode_path = NULL,
    cache = TRUE,
    force = FALSE,
    verbose = TRUE
) {
  for (pkg in c("dplyr", "tidyr", "data.table", "stringr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("The '%s' package is required for br_prepare_electoral()", pkg),
           call. = FALSE)
    }
  }

  # Validate 'what' parameter
  valid_what <- c("candidates", "parties", "turnout", "demographics")
  if (!all(what %in% valid_what)) {
    bad <- setdiff(what, valid_what)
    stop(sprintf("Invalid 'what' values: %s. Valid options: %s",
         paste(bad, collapse = ", "), paste(valid_what, collapse = ", ")),
         call. = FALSE)
  }

  # Resolve cargo aliases
  if (!is.null(cargo)) {
    cargo <- .br_resolve_cargo(cargo)
  }

  # --- Check processed data cache ---
  # Only use cache when no user-provided file paths are given
  user_files <- !is.null(perfil_path) || !is.null(comparecimento_path) ||
                !is.null(votacao_path) || !is.null(geocode_path)
  electoral_cache_name <- NULL
  if (isTRUE(cache) && !user_files) {
    electoral_cache_name <- .electoral_cache_key(
      code_muni_ibge, year, cargo, turno, what, candidates, parties
    )
    if (!isTRUE(force)) {
      cached_result <- .load_from_cache(
        electoral_cache_name, .cache_subdirs()$electoral
      )
      if (!is.null(cached_result)) {
        if (verbose) message("  Cached: electoral data for ",
                             code_muni_ibge, " (", year, ")")
        return(cached_result)
      }
    }
  }
  code_muni_ibge <- as.character(code_muni_ibge)
  code_muni_tse <- stringr::str_pad(as.character(code_muni_tse), 5, "left", "0")
  uf <- toupper(uf)

  # --- Voter Profile ---
  if (verbose) message("  Loading voter profile...")
  if (is.null(perfil_path)) {
    url_perfil <- sprintf(
      "https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_secao/perfil_eleitor_secao_%d_%s.zip",
      year, uf
    )
    zip_name <- basename(url_perfil)

    zip_path <- .interpElections_download(
      url = url_perfil, filename = zip_name, subdir = .cache_subdirs()$profile,
      cache = cache, force = force, verbose = verbose
    )

    csv_files <- utils::unzip(zip_path, list = TRUE)$Name
    csv_file <- grep("\\.csv$", csv_files, value = TRUE,
                     ignore.case = TRUE)
    extract_dir <- tempfile("interpElections_perfil_")
    dir.create(extract_dir, recursive = TRUE)
    on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
    if (length(csv_file) > 0) {
      utils::unzip(zip_path, files = csv_file[1],
                   exdir = extract_dir, overwrite = TRUE)
      perfil_path <- file.path(extract_dir, csv_file[1])
    } else {
      utils::unzip(zip_path, exdir = extract_dir)
      perfil_path <- file.path(
        extract_dir,
        sprintf("perfil_eleitor_secao_%d_%s.csv", year, uf)
      )
    }
  }

  dados_perfil <- data.table::fread(
    perfil_path, encoding = "Latin-1", sep = ";"
  )
  dados_perfil <- dplyr::mutate(dados_perfil,
    CD_MUNICIPIO = stringr::str_pad(.data$CD_MUNICIPIO, 5, "left", "0")
  )
  dados_perfil <- dplyr::filter(dados_perfil, .data$CD_MUNICIPIO == code_muni_tse)

  # Convert key columns to character early (needed for demographics and age)
  dados_perfil <- dplyr::mutate(dados_perfil,
    dplyr::across(
      dplyr::all_of(c("CD_MUNICIPIO", "NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO")),
      as.character
    )
  )

  # Demographics pivot (on ALL voters, before age filtering)
  demographics_wide <- NULL
  if ("demographics" %in% what) {
    if (verbose) message("  Computing voter demographics...")
    demographics_wide <- .br_pivot_demographics(dados_perfil)
  }

  # Map age brackets (for calibration — only 18-69 age range)
  dados_perfil <- dplyr::mutate(dados_perfil,
    DS_FAIXA_ETARIA = stringr::str_trim(.data$DS_FAIXA_ETARIA),
    faixa_idade = dplyr::case_when(
      .data$DS_FAIXA_ETARIA %in% c("18 anos", "19 anos") ~ "votantes_18_19",
      .data$DS_FAIXA_ETARIA == "20 anos" ~ "votantes_20",
      .data$DS_FAIXA_ETARIA == "21 a 24 anos" ~ "votantes_21_24",
      .data$DS_FAIXA_ETARIA == "25 a 29 anos" ~ "votantes_25_29",
      .data$DS_FAIXA_ETARIA == "30 a 34 anos" ~ "votantes_30_34",
      .data$DS_FAIXA_ETARIA == "35 a 39 anos" ~ "votantes_35_39",
      .data$DS_FAIXA_ETARIA == "40 a 44 anos" ~ "votantes_40_44",
      .data$DS_FAIXA_ETARIA == "45 a 49 anos" ~ "votantes_45_49",
      .data$DS_FAIXA_ETARIA == "50 a 54 anos" ~ "votantes_50_54",
      .data$DS_FAIXA_ETARIA == "55 a 59 anos" ~ "votantes_55_59",
      .data$DS_FAIXA_ETARIA == "60 a 64 anos" ~ "votantes_60_64",
      .data$DS_FAIXA_ETARIA == "65 a 69 anos" ~ "votantes_65_69",
      TRUE ~ NA_character_
    )
  )
  dados_perfil <- dplyr::filter(dados_perfil, !is.na(.data$faixa_idade))

  perfil_wide <- dados_perfil |>
    dplyr::count(
      .data$NM_MUNICIPIO, COD_MUN_TSE = .data$CD_MUNICIPIO,
      .data$NR_ZONA, .data$NR_SECAO, .data$NR_LOCAL_VOTACAO,
      .data$faixa_idade,
      wt = .data$QT_ELEITORES_PERFIL
    ) |>
    tidyr::pivot_wider(names_from = "faixa_idade", values_from = "n",
                       values_fill = 0)

  # Cross-tabulated gender x literacy x age columns (for full calibration)
  if ("DS_GENERO" %in% names(dados_perfil) &&
      "DS_GRAU_ESCOLARIDADE" %in% names(dados_perfil)) {
    crosstab_wide <- dados_perfil |>
      dplyr::mutate(
        genero = dplyr::case_when(
          grepl("MASCULINO", .data$DS_GENERO) ~ "hom",
          grepl("FEMININO", .data$DS_GENERO)  ~ "mul",
          TRUE ~ NA_character_
        ),
        alfab = dplyr::if_else(
          grepl("ANALFABETO", toupper(trimws(.data$DS_GRAU_ESCOLARIDADE))),
          "nalf", "alf"
        ),
        crosstab_col = paste0("vot_", .data$genero, "_", .data$alfab, "_",
                              sub("^votantes_", "", .data$faixa_idade))
      ) |>
      dplyr::filter(!is.na(.data$genero)) |>
      dplyr::count(
        .data$NM_MUNICIPIO, COD_MUN_TSE = .data$CD_MUNICIPIO,
        .data$NR_ZONA, .data$NR_SECAO, .data$NR_LOCAL_VOTACAO,
        .data$crosstab_col,
        wt = .data$QT_ELEITORES_PERFIL
      ) |>
      tidyr::pivot_wider(names_from = "crosstab_col", values_from = "n",
                         values_fill = 0)

    perfil_wide <- dplyr::left_join(perfil_wide, crosstab_wide,
      by = c("NM_MUNICIPIO", "COD_MUN_TSE", "NR_ZONA", "NR_SECAO",
             "NR_LOCAL_VOTACAO"))
  }

  # --- Vote Data ---
  needs_votes <- any(c("candidates", "parties", "turnout") %in% what)
  if (verbose && needs_votes) message("  Loading vote data...")
  dados_votos <- NULL
  dados_partidos <- NULL
  turnout_from_votes <- NULL
  turnout_extra <- NULL
  dict_rows <- list()

  if (!is.null(votacao_path)) {
    # Read from user-provided parquet file
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("The 'arrow' package is required to read parquet files", call. = FALSE)
    }
    tse_votos_all <- arrow::read_parquet(votacao_path)
    tse_votos_all <- dplyr::filter(tse_votos_all,
      .data$COD_MUN_IBGE == code_muni_ibge)
    # Normalize column names to TSE format
    if ("CODIGO_CARGO" %in% names(tse_votos_all)) {
      tse_votos_all$CD_CARGO <- tse_votos_all$CODIGO_CARGO
    }
    if ("NUM_ZONA" %in% names(tse_votos_all) && !"NR_ZONA" %in% names(tse_votos_all)) {
      tse_votos_all$NR_ZONA <- tse_votos_all$NUM_ZONA
    }
    if ("NUM_SECAO" %in% names(tse_votos_all) && !"NR_SECAO" %in% names(tse_votos_all)) {
      tse_votos_all$NR_SECAO <- tse_votos_all$NUM_SECAO
    }
    if ("NUMERO_CANDIDATO" %in% names(tse_votos_all) && !"NR_VOTAVEL" %in% names(tse_votos_all)) {
      tse_votos_all$NR_VOTAVEL <- tse_votos_all$NUMERO_CANDIDATO
    }
    if ("QTDE_VOTOS" %in% names(tse_votos_all) && !"QT_VOTOS" %in% names(tse_votos_all)) {
      tse_votos_all$QT_VOTOS <- tse_votos_all$QTDE_VOTOS
    }
    if ("NUM_LOCAL_VOTACAO" %in% names(tse_votos_all) && !"NR_LOCAL_VOTACAO" %in% names(tse_votos_all)) {
      tse_votos_all$NR_LOCAL_VOTACAO <- tse_votos_all$NUM_LOCAL_VOTACAO
    }
    if ("COD_MUN_TSE" %in% names(tse_votos_all) && !"CD_MUNICIPIO" %in% names(tse_votos_all)) {
      tse_votos_all$CD_MUNICIPIO <- tse_votos_all$COD_MUN_TSE
    }
  } else if (needs_votes) {
    # Auto-download from TSE
    # Presidente (cargo 1) lives in the national (BR) file, not per-state
    if (verbose) message("  Downloading from TSE...")
    presidente_code <- 1L
    is_federal_year <- (year %% 4L == 2L)
    has_presidente <- !is.null(cargo) && presidente_code %in% cargo
    auto_presidente <- is.null(cargo) && is_federal_year

    if (has_presidente || auto_presidente) {
      if (verbose) {
        message("  Downloading presidential data (national file, ~250 MB)...")
      }
      # National file is large (~250 MB); temporarily increase timeout
      old_timeout <- getOption("interpElections.timeout")
      options(interpElections.timeout = max(
        getOption("interpElections.timeout", 300L), 900L
      ))
      on.exit(options(interpElections.timeout = old_timeout), add = TRUE)
      tse_presidente <- br_download_votes(
        year = year, uf = "BR", code_muni_tse = code_muni_tse,
        cargo = presidente_code, turno = turno,
        force = force,
        cache = cache, verbose = verbose
      )

      other_cargos <- if (!is.null(cargo)) setdiff(cargo, presidente_code) else NULL
      need_state <- is.null(cargo) || length(other_cargos) > 0
      if (need_state) {
        download_other <- if (!is.null(other_cargos) && length(other_cargos) == 1) {
          other_cargos
        } else {
          NULL
        }
        if (verbose) message("  Downloading state-level data...")
        tse_state <- br_download_votes(
          year = year, uf = uf, code_muni_tse = code_muni_tse,
          cargo = download_other, turno = turno,
          force = force,
          cache = cache, verbose = verbose
        )
        common_cols <- intersect(names(tse_presidente), names(tse_state))
        tse_votos_all <- rbind(
          tse_presidente[, common_cols, drop = FALSE],
          tse_state[, common_cols, drop = FALSE]
        )
      } else {
        tse_votos_all <- tse_presidente
      }
    } else {
      download_cargo <- if (!is.null(cargo) && length(cargo) == 1) cargo else NULL
      tse_votos_all <- br_download_votes(
        year = year, uf = uf, code_muni_tse = code_muni_tse,
        cargo = download_cargo, turno = turno,
        force = force,
        cache = cache, verbose = verbose
      )
    }
  } else {
    tse_votos_all <- data.frame()
  }

  if (nrow(tse_votos_all) > 0) {
    # Ensure key columns are character
    for (col in c("NR_ZONA", "NR_SECAO", "CD_MUNICIPIO")) {
      if (col %in% names(tse_votos_all))
        tse_votos_all[[col]] <- as.character(tse_votos_all[[col]])
    }
    if ("NR_LOCAL_VOTACAO" %in% names(tse_votos_all)) {
      tse_votos_all$NR_LOCAL_VOTACAO <- as.character(tse_votos_all$NR_LOCAL_VOTACAO)
    }

    # Discover which cargos exist
    cargo_col <- intersect(c("CD_CARGO", "CODIGO_CARGO"), names(tse_votos_all))
    if (length(cargo_col) > 0) {
      cargo_col <- cargo_col[1]
      if (is.null(cargo)) {
        cargo <- sort(unique(as.integer(tse_votos_all[[cargo_col]])))
        if (verbose) {
          cargo_labels <- vapply(cargo, function(c) .br_cargo_label(c), character(1))
          message(sprintf("  Cargos found: %s",
            paste(cargo_labels, collapse = ", ")))
        }
      }
    } else {
      # No cargo column — treat as single unknown cargo
      if (is.null(cargo)) cargo <- 0L
    }
    multi_cargo <- length(cargo) > 1

    # Section group columns
    grp_cols <- intersect(
      c("NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO"), names(tse_votos_all)
    )
    id_cols <- c("COD_MUN_TSE", grp_cols)

    # Compute turnout from the first cargo's votes
    first_cargo <- cargo[1]
    if (length(cargo_col) > 0) {
      first_votos <- tse_votos_all[
        as.integer(tse_votos_all[[cargo_col]]) == first_cargo, ]
    } else {
      first_votos <- tse_votos_all
    }
    tse_votos_all$COD_MUN_TSE <- as.character(
      tse_votos_all[[intersect(c("CD_MUNICIPIO", "COD_MUN_TSE"), names(tse_votos_all))[1]]]
    )
    first_votos$COD_MUN_TSE <- as.character(
      first_votos[[intersect(c("CD_MUNICIPIO", "COD_MUN_TSE"), names(first_votos))[1]]]
    )

    turnout_from_votes <- first_votos |>
      dplyr::mutate(QT_VOTOS = as.integer(.data$QT_VOTOS)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarise(
        QT_COMPARECIMENTO = sum(.data$QT_VOTOS, na.rm = TRUE),
        .groups = "drop"
      )

    # Extract QT_APTOS / QT_ABSTENCOES if turnout requested
    if ("turnout" %in% what) {
      turnout_extra <- .br_extract_turnout(first_votos, id_cols)
    }

    # Build party mapping if SG_PARTIDO is not available
    party_map <- NULL
    if (!("SG_PARTIDO" %in% names(tse_votos_all))) {
      if (verbose) message("  SG_PARTIDO not in vote data; downloading party legends...")
      party_map <- tryCatch(
        .br_build_party_map(year, cache = cache, force = force, verbose = verbose),
        error = function(e) {
          if (verbose) message("  Could not download party legends: ", e$message)
          NULL
        }
      )
    }

    # Process each cargo
    dados_votos_list <- list()
    dados_partidos_list <- list()

    for (cargo_i in cargo) {
      if (length(cargo_col) > 0) {
        tse_votos_i <- tse_votos_all[
          as.integer(tse_votos_all[[cargo_col]]) == cargo_i, ]
      } else {
        tse_votos_i <- tse_votos_all
      }
      tse_votos_i$COD_MUN_TSE <- as.character(
        tse_votos_i[[intersect(c("CD_MUNICIPIO", "COD_MUN_TSE"), names(tse_votos_i))[1]]]
      )

      if (nrow(tse_votos_i) == 0) next

      prefix <- if (multi_cargo) paste0(.br_cargo_label(cargo_i), "_") else ""
      cargo_label <- .br_cargo_label(cargo_i)

      if ("candidates" %in% what) {
        votos_filtered <- if (!is.null(candidates)) {
          .br_filter_candidates(tse_votos_i, candidates, verbose)
        } else {
          tse_votos_i
        }
        if (nrow(votos_filtered) > 0) {
          dados_votos_list[[as.character(cargo_i)]] <-
            .br_pivot_candidates(votos_filtered, id_cols, prefix)

          # Extract candidate metadata for dictionary
          dict_rows <- .br_extract_candidate_dict(
            votos_filtered, prefix, cargo_label, dict_rows,
            party_map = party_map)
        }
      }

      if ("parties" %in% what) {
        votos_for_parties <- if (!is.null(parties)) {
          .br_filter_parties(tse_votos_i, parties, verbose)
        } else {
          tse_votos_i
        }
        if (nrow(votos_for_parties) > 0) {
          dados_partidos_list[[as.character(cargo_i)]] <-
            .br_pivot_parties(votos_for_parties, id_cols, prefix)

          # Extract party metadata for dictionary
          dict_rows <- .br_extract_party_dict(
            votos_for_parties, prefix, cargo_label, dict_rows,
            party_map = party_map)
        }
      }
    }

    # Merge all cargo-specific data.frames
    if (length(dados_votos_list) > 0) {
      dados_votos <- Reduce(function(a, b) {
        dplyr::full_join(a, b, by = id_cols)
      }, dados_votos_list)
    }
    if (length(dados_partidos_list) > 0) {
      dados_partidos <- Reduce(function(a, b) {
        dplyr::full_join(a, b, by = id_cols)
      }, dados_partidos_list)
    }
  } else {
    if (verbose && needs_votes) {
      message("  No vote records found for this municipality")
    }
  }

  # --- Attendance ---
  if (verbose) message("  Loading attendance data...")
  dados_comp <- NULL

  if (!is.null(comparecimento_path)) {
    # Read from user-provided parquet file
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("The 'arrow' package is required to read parquet files", call. = FALSE)
    }
    dados_comp <- arrow::read_parquet(comparecimento_path)
    dados_comp <- dados_comp |>
      dplyr::filter(.data$COD_MUN_IBGE == code_muni_ibge,
                    .data$NUM_TURNO == turno) |>
      dplyr::mutate(QT_COMPARECIMENTO = as.integer(.data$QTDE_COMPARECIMENTO)) |>
      dplyr::distinct(
        .data$COD_MUN_TSE,
        .data$NUM_ZONA, .data$NUM_LOCAL_VOTACAO, .data$NUM_SECAO,
        .data$QT_COMPARECIMENTO
      )
  } else if (!is.null(turnout_from_votes) && nrow(turnout_from_votes) > 0) {
    # Use turnout computed from vote data (avoids large nationwide download)
    if (verbose) message("  Computing turnout from vote data...")
    dados_comp <- turnout_from_votes |>
      dplyr::mutate(QT_COMPARECIMENTO = as.integer(.data$QT_COMPARECIMENTO))

    # Merge turnout extras (QT_APTOS, QT_ABSTENCOES) if available
    if (!is.null(turnout_extra) && nrow(turnout_extra) > 0) {
      comp_id_cols <- intersect(names(dados_comp), names(turnout_extra))
      comp_id_cols <- setdiff(comp_id_cols, c("QT_APTOS", "QT_ABSTENCOES"))
      dados_comp <- dplyr::left_join(dados_comp, turnout_extra, by = comp_id_cols)
    }

    dados_comp <- dplyr::distinct(dados_comp)
  } else {
    if (verbose) message("  (no attendance data available)")
  }

  # --- Geocoded Polling Stations ---
  if (verbose) message("  Loading geocoded polling stations...")
  if (!is.null(geocode_path)) {
    # User override: read custom geocoded file directly
    geocode_locs <- utils::read.csv(geocode_path)
    # Expect columns: nr_zona, nr_local_votacao (or nr_locvot), lat, long
    if ("nr_locvot" %in% names(geocode_locs) && !"nr_local_votacao" %in% names(geocode_locs)) {
      geocode_locs$nr_local_votacao <- geocode_locs$nr_locvot
    }
    geocode_locs$nr_zona <- as.character(as.integer(geocode_locs$nr_zona))
    geocode_locs$nr_local_votacao <- as.character(as.integer(geocode_locs$nr_local_votacao))
    geocode_locs$lat <- as.numeric(geocode_locs$lat)
    geocode_locs$long <- as.numeric(geocode_locs$long)
  } else {
    geocode_locs <- .br_geocode_locations(
      year = year,
      code_muni_tse = code_muni_tse,
      code_muni_ibge = code_muni_ibge,
      uf = uf,
      cache = cache,
      force = force,
      verbose = verbose
    )
  }

  # --- Merge ---
  if (verbose) message("  Merging datasets...")
  merged <- perfil_wide

  # Summarise profile by section
  merged <- merged |>
    dplyr::group_by(
      .data$NM_MUNICIPIO, .data$COD_MUN_TSE,
      .data$NR_ZONA, .data$NR_LOCAL_VOTACAO, .data$NR_SECAO
    ) |>
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), sum), .groups = "drop")

  # Normalize zone/location IDs for consistent joins
  merged <- dplyr::mutate(merged,
    NR_ZONA = as.character(as.integer(.data$NR_ZONA)),
    NR_LOCAL_VOTACAO = as.character(as.integer(.data$NR_LOCAL_VOTACAO))
  )

  # Helper to normalize and join a data.frame into merged
  .join_to_merged <- function(merged, df, value_pattern) {
    if (is.null(df) || nrow(df) == 0) return(merged)

    # Detect which key format the df uses
    has_num <- "NUM_ZONA" %in% names(df)
    has_nr <- "NR_ZONA" %in% names(df)

    if (has_num) {
      # Normalize NUM_* keys
      df <- dplyr::mutate(df,
        NUM_ZONA = as.character(as.integer(.data$NUM_ZONA)),
        NUM_SECAO = as.character(as.integer(.data$NUM_SECAO))
      )
      if ("NUM_LOCAL_VOTACAO" %in% names(df)) {
        df <- dplyr::mutate(df,
          NUM_LOCAL_VOTACAO = as.character(as.integer(.data$NUM_LOCAL_VOTACAO)))
      }
      join_keys <- c("COD_MUN_TSE", "NR_ZONA" = "NUM_ZONA",
                      "NR_SECAO" = "NUM_SECAO")
      if ("NUM_LOCAL_VOTACAO" %in% names(df)) {
        join_keys <- c(join_keys, "NR_LOCAL_VOTACAO" = "NUM_LOCAL_VOTACAO")
      }
    } else if (has_nr) {
      # Normalize NR_* keys
      df <- dplyr::mutate(df,
        NR_ZONA = as.character(as.integer(.data$NR_ZONA)),
        NR_SECAO = as.character(as.integer(.data$NR_SECAO))
      )
      if ("NR_LOCAL_VOTACAO" %in% names(df)) {
        df <- dplyr::mutate(df,
          NR_LOCAL_VOTACAO = as.character(as.integer(.data$NR_LOCAL_VOTACAO)))
      }
      join_keys <- intersect(
        c("COD_MUN_TSE", "NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO"),
        intersect(names(merged), names(df))
      )
    } else {
      return(merged)
    }

    if (!"COD_MUN_TSE" %in% names(df)) {
      df$COD_MUN_TSE <- code_muni_tse
    }

    # Select only keys + value columns
    value_cols <- grep(value_pattern, names(df), value = TRUE)
    all_keys_in_df <- if (has_num) {
      intersect(c("COD_MUN_TSE", "NUM_ZONA", "NUM_LOCAL_VOTACAO", "NUM_SECAO"),
        names(df))
    } else {
      intersect(c("COD_MUN_TSE", "NR_ZONA", "NR_LOCAL_VOTACAO", "NR_SECAO"),
        names(df))
    }
    df <- df[, c(all_keys_in_df, value_cols), drop = FALSE]
    df <- dplyr::distinct(df)

    dplyr::left_join(merged, df, by = join_keys)
  }

  # Add attendance
  if (!is.null(dados_comp)) {
    merged <- .join_to_merged(merged, dados_comp,
      "^(QT_COMPARECIMENTO|QT_APTOS|QT_ABSTENCOES)$")
  }

  # Add candidate votes
  if (!is.null(dados_votos)) {
    merged <- .join_to_merged(merged, dados_votos, "CAND_")
  }

  # Add party votes
  if (!is.null(dados_partidos)) {
    merged <- .join_to_merged(merged, dados_partidos, "PARTY_")
  }

  # Add demographics
  if (!is.null(demographics_wide)) {
    # demographics_wide has NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO (from perfil)
    demographics_wide <- dplyr::mutate(demographics_wide,
      NR_ZONA = as.character(as.integer(.data$NR_ZONA)),
      NR_LOCAL_VOTACAO = as.character(as.integer(.data$NR_LOCAL_VOTACAO)),
      NR_SECAO = as.character(as.integer(.data$NR_SECAO))
    )
    demo_value_cols <- grep("^(GENERO_|EDUC_)", names(demographics_wide),
      value = TRUE)
    demo_keys <- intersect(
      c("NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO"),
      intersect(names(merged), names(demographics_wide))
    )
    merged <- dplyr::left_join(merged,
      demographics_wide[, c(demo_keys, demo_value_cols), drop = FALSE],
      by = demo_keys)
  }

  # Add coordinates via geocoded polling station data
  merged <- dplyr::left_join(merged, geocode_locs,
    by = c("NR_ZONA" = "nr_zona", "NR_LOCAL_VOTACAO" = "nr_local_votacao")
  )

  # Report geocoding match statistics
  n_total <- nrow(merged)
  n_geocoded <- sum(!is.na(merged$lat) & !is.na(merged$long))
  n_missing <- n_total - n_geocoded
  if (verbose) {
    message(sprintf(
      "  Geocoding: %d/%d sections matched (%.1f%%), %d without coordinates",
      n_geocoded, n_total, 100 * n_geocoded / max(n_total, 1), n_missing
    ))
  }
  if (n_geocoded == 0) {
    stop("No polling stations could be geocoded. Check municipality code and year.",
         call. = FALSE)
  }

  # Aggregate by lat/long (voting location level)
  if (verbose) message("  Aggregating by voting location...")
  drop_cols <- c("NM_MUNICIPIO", "ANO_ELEICAO", "COD_MUN_IBGE",
                 "COD_MUN_TSE", "NR_ZONA", "NR_LOCAL_VOTACAO", "NR_SECAO",
                 "ano", "source",
                 "genero_col", "educ_col", "party_key", "party_votes")
  drop_cols <- intersect(drop_cols, names(merged))

  result <- merged |>
    dplyr::select(-dplyr::all_of(drop_cols)) |>
    dplyr::group_by(.data$lat, .data$long) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(
      id = seq_len(dplyr::n()),
      lat = as.numeric(.data$lat),
      long = as.numeric(.data$long)
    ) |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$long))

  # Attach column dictionary as attribute
  if (length(dict_rows) > 0) {
    col_dict <- do.call(rbind, dict_rows)
    row.names(col_dict) <- NULL
    attr(result, "column_dictionary") <- col_dict
  }

  # Cache processed result for future re-use
  if (!is.null(electoral_cache_name)) {
    tryCatch(
      .save_to_cache(result, electoral_cache_name,
                     .cache_subdirs()$electoral),
      error = function(e) {
        if (verbose) {
          warning("Failed to cache electoral data: ",
                  conditionMessage(e), call. = FALSE)
        }
      }
    )
    if (verbose) message("  Cached processed electoral data for future use")
  }

  result
}


# --- Internal: processed data cache key ---

#' @noRd
.electoral_cache_key <- function(code_muni_ibge, year, cargo, turno,
                                  what, candidates, parties) {
  key_parts <- list(
    muni       = as.character(code_muni_ibge),
    year       = as.integer(year),
    cargo      = sort(as.integer(cargo %||% 0L)),
    turno      = as.integer(turno),
    what       = sort(what),
    candidates = sort(as.character(candidates %||% "")),
    parties    = sort(toupper(as.character(parties %||% "")))
  )
  hash <- substr(.digest_simple(key_parts), 1, 16)
  sprintf("electoral_%s_%d_%s.rds", code_muni_ibge, year, hash)
}


# --- Internal: geocode polling locations from TSE + Hidalgo fallback ---

.br_geocode_locations <- function(
    year,
    code_muni_tse,
    code_muni_ibge,
    uf,
    cache    = TRUE,
    force    = FALSE,
    verbose  = TRUE
) {
  # Normalize helper: strip leading zeros for consistent joins
  norm_id <- function(x) as.character(as.integer(x))

  # --- Source 1: TSE official geocoded data ---
  tse_locs <- NULL
  tse_data <- br_download_geocode(
    year = year, uf = uf, code_muni_tse = code_muni_tse,
    cache = cache, force = force, verbose = verbose
  )

  if (!is.null(tse_data) && nrow(tse_data) > 0) {
    # Filter to turno 1 to avoid duplicate rows
    if ("NR_TURNO" %in% names(tse_data)) {
      tse_data <- tse_data[tse_data$NR_TURNO == 1L, ]
    }

    # Deduplicate to unique polling locations (multiple sections share coords)
    tse_locs <- tse_data[
      !duplicated(paste(tse_data$NR_ZONA, tse_data$NR_LOCAL_VOTACAO)),
      c("NR_ZONA", "NR_LOCAL_VOTACAO", "NR_LATITUDE", "NR_LONGITUDE")
    ]

    # Normalize IDs
    tse_locs$nr_zona <- norm_id(tse_locs$NR_ZONA)
    tse_locs$nr_local_votacao <- norm_id(tse_locs$NR_LOCAL_VOTACAO)
    tse_locs$lat <- tse_locs$NR_LATITUDE
    tse_locs$long <- tse_locs$NR_LONGITUDE
    tse_locs <- tse_locs[, c("nr_zona", "nr_local_votacao", "lat", "long")]

    # Separate valid and missing coordinates
    tse_valid <- tse_locs[tse_locs$lat != -1 & tse_locs$long != -1, ]
    tse_missing <- tse_locs[tse_locs$lat == -1 | tse_locs$long == -1, ]

    if (verbose) {
      message(sprintf("    TSE: %d locations (%d with coords, %d missing)",
                      nrow(tse_locs), nrow(tse_valid), nrow(tse_missing)))
    }
  } else {
    tse_valid <- data.frame(
      nr_zona = character(0), nr_local_votacao = character(0),
      lat = numeric(0), long = numeric(0)
    )
    tse_missing <- data.frame(
      nr_zona = character(0), nr_local_votacao = character(0),
      lat = numeric(0), long = numeric(0)
    )
    if (verbose && is.null(tse_data)) {
      message("    TSE geocoded data not available, using Hidalgo only")
    }
  }

  # --- Source 2: Hidalgo geocoded data (fallback) ---
  hidalgo_url <- "https://github.com/fdhidalgo/geocode_br_polling_stations/releases/download/v0.13-beta/geocoded_polling_stations.csv.gz"

  hidalgo_locs <- data.frame(
    nr_zona = character(0), nr_local_votacao = character(0),
    lat = numeric(0), long = numeric(0)
  )

  need_hidalgo <- nrow(tse_missing) > 0 || nrow(tse_valid) == 0
  if (need_hidalgo) {
    hidalgo_path <- tryCatch(
      .interpElections_download(
        url = hidalgo_url,
        filename = "geocoded_polling_stations.csv.gz",
        subdir = .cache_subdirs()$hidalgo,
        cache = cache, force = force, verbose = verbose
      ),
      error = function(e) {
        if (verbose) {
          message("  Warning: could not download Hidalgo data: ",
                  conditionMessage(e))
        }
        NULL
      }
    )

    if (!is.null(hidalgo_path) && file.exists(hidalgo_path)) {
      hidalgo_raw <- utils::read.csv(hidalgo_path)
      hidalgo_raw <- hidalgo_raw[
        hidalgo_raw$ano == year &
        hidalgo_raw$cod_localidade_ibge == as.numeric(code_muni_ibge),
      ]
      if (nrow(hidalgo_raw) > 0) {
        hidalgo_locs <- data.frame(
          nr_zona = norm_id(hidalgo_raw$nr_zona),
          nr_local_votacao = norm_id(hidalgo_raw$nr_locvot),
          lat = as.numeric(hidalgo_raw$lat),
          long = as.numeric(hidalgo_raw$long),
          stringsAsFactors = FALSE
        )
        # Remove rows with NA coordinates
        hidalgo_locs <- hidalgo_locs[
          !is.na(hidalgo_locs$lat) & !is.na(hidalgo_locs$long),
        ]
        # Deduplicate
        hidalgo_locs <- hidalgo_locs[
          !duplicated(paste(hidalgo_locs$nr_zona, hidalgo_locs$nr_local_votacao)),
        ]
      }
      if (verbose) {
        message(sprintf("    Hidalgo: %d locations available", nrow(hidalgo_locs)))
      }
    }
  }

  # --- Merge: TSE valid + Hidalgo for TSE missing + Hidalgo-only extras ---
  tse_valid$source <- if (nrow(tse_valid) > 0) "tse" else character(0)

  # Fill in TSE missing coordinates from Hidalgo
  filled_from_hidalgo <- data.frame(
    nr_zona = character(0), nr_local_votacao = character(0),
    lat = numeric(0), long = numeric(0), source = character(0)
  )
  if (nrow(tse_missing) > 0 && nrow(hidalgo_locs) > 0) {
    tse_missing_key <- paste(tse_missing$nr_zona, tse_missing$nr_local_votacao)
    hidalgo_key <- paste(hidalgo_locs$nr_zona, hidalgo_locs$nr_local_votacao)
    match_idx <- match(tse_missing_key, hidalgo_key)
    matched <- !is.na(match_idx)

    if (any(matched)) {
      filled_from_hidalgo <- data.frame(
        nr_zona = tse_missing$nr_zona[matched],
        nr_local_votacao = tse_missing$nr_local_votacao[matched],
        lat = hidalgo_locs$lat[match_idx[matched]],
        long = hidalgo_locs$long[match_idx[matched]],
        source = "hidalgo",
        stringsAsFactors = FALSE
      )
    }
  }

  # Add Hidalgo-only locations (not in TSE at all)
  hidalgo_extra <- data.frame(
    nr_zona = character(0), nr_local_votacao = character(0),
    lat = numeric(0), long = numeric(0), source = character(0)
  )
  if (nrow(hidalgo_locs) > 0) {
    all_tse_keys <- c(
      paste(tse_valid$nr_zona, tse_valid$nr_local_votacao),
      paste(tse_missing$nr_zona, tse_missing$nr_local_votacao)
    )
    hidalgo_key <- paste(hidalgo_locs$nr_zona, hidalgo_locs$nr_local_votacao)
    in_hidalgo_only <- !(hidalgo_key %in% all_tse_keys)
    if (any(in_hidalgo_only)) {
      hidalgo_extra <- data.frame(
        nr_zona = hidalgo_locs$nr_zona[in_hidalgo_only],
        nr_local_votacao = hidalgo_locs$nr_local_votacao[in_hidalgo_only],
        lat = hidalgo_locs$lat[in_hidalgo_only],
        long = hidalgo_locs$long[in_hidalgo_only],
        source = "hidalgo",
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine all sources
  result <- rbind(tse_valid, filled_from_hidalgo, hidalgo_extra)

  if (verbose) {
    n_tse <- sum(result$source == "tse")
    n_hid <- sum(result$source == "hidalgo")
    message(sprintf("    Geocoded: %d locations (%d TSE, %d Hidalgo)",
                    nrow(result), n_tse, n_hid))
  }

  result
}


# --- Internal: cargo code aliases ---
#' @noRd
.br_resolve_cargo <- function(cargo) {
  if (is.numeric(cargo) || is.integer(cargo)) return(as.integer(cargo))
  lookup <- c(
    prefeito = 11L, vereador = 13L,
    presidente = 1L, governador = 3L, senador = 5L,
    deputado_federal = 6L, deputado_estadual = 7L, deputado_distrital = 8L
  )
  codes <- lookup[tolower(cargo)]
  if (any(is.na(codes))) {
    bad <- cargo[is.na(codes)]
    stop(sprintf("Unknown cargo: %s. Valid aliases: %s",
         paste(bad, collapse = ", "), paste(names(lookup), collapse = ", ")),
         call. = FALSE)
  }
  unname(codes)
}

#' @noRd
.br_cargo_label <- function(cargo_code) {
  labels <- c(
    "1" = "PRESIDENTE", "3" = "GOVERNADOR", "5" = "SENADOR",
    "6" = "DEPUTADO_FEDERAL", "7" = "DEPUTADO_ESTADUAL",
    "8" = "DEPUTADO_DISTRITAL",
    "11" = "PREFEITO", "13" = "VEREADOR"
  )
  lbl <- unname(labels[as.character(cargo_code)])
  ifelse(is.na(lbl), paste0("CARGO", cargo_code), lbl)
}

#' @noRd
.br_remove_accents <- function(x) {
  # Try UTF-8 first, then native encoding; iconv with //TRANSLIT
  # can return NA on some Windows builds — fall back to original.
  result <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  failed <- is.na(result) & !is.na(x)
  if (any(failed)) {
    result[failed] <- iconv(x[failed], to = "ASCII//TRANSLIT")
  }
  # If still NA, keep the original string (grepl will still try to match)
  still_na <- is.na(result) & !is.na(x)
  if (any(still_na)) result[still_na] <- x[still_na]
  result
}

#' @noRd
.br_normalize_educ <- function(x) {
  x <- toupper(trimws(x))
  dplyr::case_when(
    grepl("ANALFABETO", x)              ~ "ANALFABETO",
    grepl("LE E ESCREVE", .br_remove_accents(x)) ~ "LE_ESCREVE",
    grepl("FUNDAMENTAL.+INCOMPLETO", x) ~ "FUND_INCOMP",
    grepl("FUNDAMENTAL.+COMPLETO", x)   ~ "FUND_COMP",
    grepl("MEDIO.+INCOMPLETO", .br_remove_accents(x)) ~ "MEDIO_INCOMP",
    grepl("MEDIO.+COMPLETO", .br_remove_accents(x))   ~ "MEDIO_COMP",
    grepl("SUPERIOR.+INCOMPLETO", x)    ~ "SUP_INCOMP",
    grepl("SUPERIOR.+COMPLETO", x)      ~ "SUP_COMP",
    grepl("NAO.+INFORMADO", .br_remove_accents(x)) ~ "NAO_INFORMADO",
    TRUE ~ gsub("[^A-Z0-9]", "_", .br_remove_accents(x))
  )
}

#' @noRd
.br_pivot_demographics <- function(dados_perfil) {
  gender_wide <- NULL
  if ("DS_GENERO" %in% names(dados_perfil)) {
    gender_wide <- dados_perfil |>
      dplyr::mutate(genero_col = paste0("GENERO_", gsub("\\s+", "_",
        toupper(trimws(.br_remove_accents(.data$DS_GENERO)))))) |>
      dplyr::count(.data$NR_ZONA, .data$NR_SECAO, .data$NR_LOCAL_VOTACAO,
        .data$genero_col, wt = .data$QT_ELEITORES_PERFIL) |>
      tidyr::pivot_wider(names_from = "genero_col", values_from = "n",
        values_fill = 0)
  }

  educ_wide <- NULL
  if ("DS_GRAU_ESCOLARIDADE" %in% names(dados_perfil)) {
    educ_wide <- dados_perfil |>
      dplyr::mutate(educ_col = paste0("EDUC_",
        .br_normalize_educ(.data$DS_GRAU_ESCOLARIDADE))) |>
      dplyr::count(.data$NR_ZONA, .data$NR_SECAO, .data$NR_LOCAL_VOTACAO,
        .data$educ_col, wt = .data$QT_ELEITORES_PERFIL) |>
      tidyr::pivot_wider(names_from = "educ_col", values_from = "n",
        values_fill = 0)
  }

  if (!is.null(gender_wide) && !is.null(educ_wide)) {
    result <- dplyr::left_join(gender_wide, educ_wide,
      by = c("NR_ZONA", "NR_SECAO", "NR_LOCAL_VOTACAO"))
  } else {
    result <- if (!is.null(gender_wide)) gender_wide else educ_wide
  }
  result
}

#' Build a named vector mapping party number -> abbreviation from TSE legends
#' @noRd
.br_build_party_map <- function(year, cache = TRUE, force = FALSE,
                                verbose = TRUE) {
  legends <- br_download_party_legends(year, cache = cache, force = force,
                                       verbose = verbose)
  nr <- as.character(legends$NR_PARTIDO)
  sg <- toupper(trimws(as.character(legends$SG_PARTIDO)))
  map <- stats::setNames(sg, nr)
  map[!duplicated(names(map))]
}

#' @noRd
.br_extract_candidate_dict <- function(tse_votos, prefix, cargo_label,
                                       dict_rows, party_map = NULL) {
  has_name <- "NM_VOTAVEL" %in% names(tse_votos)
  has_party <- "SG_PARTIDO" %in% names(tse_votos)

  # Get unique ballot numbers with their metadata
  nr_vals <- unique(as.character(tse_votos$NR_VOTAVEL))

  for (nr in nr_vals) {
    subset_i <- tse_votos[as.character(tse_votos$NR_VOTAVEL) == nr, ]

    # Candidate name
    cand_name <- if (nr == "95") {
      "Votos em Branco"
    } else if (nr == "96") {
      "Votos Nulos"
    } else if (has_name) {
      nm <- unique(as.character(subset_i$NM_VOTAVEL))
      nm <- nm[!is.na(nm) & nm != ""]
      if (length(nm) > 0) nm[1] else NA_character_
    } else {
      NA_character_
    }

    # Party abbreviation
    party <- if (nr %in% c("95", "96")) {
      NA_character_
    } else if (has_party) {
      sg <- unique(trimws(as.character(subset_i$SG_PARTIDO)))
      sg <- sg[!is.na(sg) & sg != ""]
      if (length(sg) > 0) toupper(sg[1]) else NA_character_
    } else if (!is.null(party_map)) {
      party_num <- substr(nr, 1, 2)
      if (party_num %in% names(party_map)) {
        unname(party_map[party_num])
      } else {
        NA_character_
      }
    } else {
      NA_character_
    }

    dict_rows[[length(dict_rows) + 1L]] <- data.frame(
      column         = paste0(prefix, "CAND_", nr),
      type           = "candidate",
      cargo          = cargo_label,
      ballot_number  = nr,
      candidate_name = cand_name,
      party          = party,
      stringsAsFactors = FALSE
    )
  }
  dict_rows
}

#' @noRd
.br_extract_party_dict <- function(tse_votos, prefix, cargo_label,
                                   dict_rows, party_map = NULL) {
  # Exclude blank (95) and null (96) votes
  party_data <- tse_votos[
    !(as.character(tse_votos$NR_VOTAVEL) %in% c("95", "96")), ]
  if (nrow(party_data) == 0) return(dict_rows)

  has_sg <- "SG_PARTIDO" %in% names(party_data) &&
    !all(is.na(party_data$SG_PARTIDO)) &&
    !all(trimws(party_data$SG_PARTIDO) == "")

  if (has_sg) {
    parties <- unique(gsub("\\s+", "_", toupper(trimws(party_data$SG_PARTIDO))))
    parties <- parties[!is.na(parties) & parties != ""]
  } else {
    parties <- unique(substr(as.character(party_data$NR_VOTAVEL), 1, 2))
  }

  for (p in parties) {
    # When using numeric party codes, try to resolve abbreviation from legends
    party_label <- if (!has_sg && !is.null(party_map) && p %in% names(party_map)) {
      unname(party_map[p])
    } else {
      p
    }
    dict_rows[[length(dict_rows) + 1L]] <- data.frame(
      column         = paste0(prefix, "PARTY_", p),
      type           = "party",
      cargo          = cargo_label,
      ballot_number  = NA_character_,
      candidate_name = NA_character_,
      party          = party_label,
      stringsAsFactors = FALSE
    )
  }
  dict_rows
}

#' @noRd
.br_pivot_candidates <- function(tse_votos, id_cols, prefix = "") {
  tse_votos$NUMERO_CANDIDATO <- as.character(tse_votos$NR_VOTAVEL)
  tse_votos$QTDE_VOTOS <- as.integer(tse_votos$QT_VOTOS)
  glue_str <- if (nzchar(prefix)) {
    paste0(prefix, "CAND_{NUMERO_CANDIDATO}")
  } else {
    "CAND_{NUMERO_CANDIDATO}"
  }
  tse_votos |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_cols),
      names_from = "NUMERO_CANDIDATO",
      names_glue = glue_str,
      values_from = "QTDE_VOTOS",
      values_fill = 0L
    )
}

#' @noRd
.br_pivot_parties <- function(tse_votos, id_cols, prefix = "") {
  # Exclude blank (95) and null (96) votes
  party_data <- tse_votos[
    !(as.character(tse_votos$NR_VOTAVEL) %in% c("95", "96")), ]
  if (nrow(party_data) == 0) return(NULL)

  has_sg <- "SG_PARTIDO" %in% names(party_data) &&
    !all(is.na(party_data$SG_PARTIDO)) &&
    !all(trimws(party_data$SG_PARTIDO) == "")

  if (has_sg) {
    party_data$party_key <- paste0(prefix, "PARTY_",
      gsub("\\s+", "_", toupper(trimws(party_data$SG_PARTIDO))))
  } else {
    party_data$party_key <- paste0(prefix, "PARTY_",
      substr(as.character(party_data$NR_VOTAVEL), 1, 2))
  }

  party_data |>
    dplyr::mutate(QT_VOTOS = as.integer(.data$QT_VOTOS)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols)), .data$party_key) |>
    dplyr::summarise(party_votes = sum(.data$QT_VOTOS, na.rm = TRUE),
      .groups = "drop") |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_cols),
      names_from = "party_key",
      values_from = "party_votes",
      values_fill = 0L
    )
}

#' @noRd
.br_filter_candidates <- function(tse_votos, candidates, verbose = TRUE) {
  numeric_ids <- candidates[suppressWarnings(!is.na(as.numeric(candidates)))]
  name_searches <- candidates[suppressWarnings(is.na(as.numeric(candidates)))]

  keep <- rep(FALSE, nrow(tse_votos))

  if (length(numeric_ids) > 0) {
    keep <- keep |
      (as.character(tse_votos$NR_VOTAVEL) %in% as.character(numeric_ids))
  }

  if (length(name_searches) > 0 && "NM_VOTAVEL" %in% names(tse_votos)) {
    normalized_names <- toupper(.br_remove_accents(
      as.character(tse_votos$NM_VOTAVEL)))
    for (search_term in name_searches) {
      pattern <- toupper(.br_remove_accents(search_term))
      keep <- keep | grepl(pattern, normalized_names, fixed = TRUE)
    }
  } else if (length(name_searches) > 0 && verbose) {
    warning("NM_VOTAVEL column not available; cannot filter by candidate name. ",
            "Use numeric candidate IDs instead.", call. = FALSE)
  }

  # Coerce NAs to FALSE (can happen if iconv returns NA on some platforms)
  keep[is.na(keep)] <- FALSE
  result <- tse_votos[keep, ]

  if (verbose) {
    if (nrow(result) > 0 && "NM_VOTAVEL" %in% names(result)) {
      matched <- unique(paste0(
        result$NM_VOTAVEL, " (", result$NR_VOTAVEL, ")"))
      message(sprintf("  Matched candidates: %s",
        paste(utils::head(matched, 10), collapse = ", ")))
      if (length(matched) > 10)
        message(sprintf("  ... and %d more", length(matched) - 10))
    } else if (nrow(result) == 0) {
      if ("NM_VOTAVEL" %in% names(tse_votos)) {
        available <- unique(paste0(
          tse_votos$NM_VOTAVEL, " (", tse_votos$NR_VOTAVEL, ")"))
        warning(sprintf(
          "No candidates matched filter: %s\nAvailable: %s",
          paste(candidates, collapse = ", "),
          paste(utils::head(available, 20), collapse = ", ")),
          call. = FALSE)
      }
    }
  }
  result
}

#' @noRd
.br_filter_parties <- function(tse_votos, parties, verbose = TRUE) {
  parties_upper <- toupper(trimws(parties))

  if ("SG_PARTIDO" %in% names(tse_votos)) {
    keep <- toupper(trimws(tse_votos$SG_PARTIDO)) %in% parties_upper
    result <- tse_votos[keep, ]

    if (verbose) {
      matched <- unique(toupper(trimws(result$SG_PARTIDO)))
      unmatched <- setdiff(parties_upper, matched)
      if (length(matched) > 0)
        message(sprintf("  Matched parties: %s",
          paste(matched, collapse = ", ")))
      if (length(unmatched) > 0) {
        available <- sort(unique(toupper(trimws(tse_votos$SG_PARTIDO))))
        warning(sprintf("Parties not found: %s\nAvailable: %s",
          paste(unmatched, collapse = ", "),
          paste(available, collapse = ", ")),
          call. = FALSE)
      }
    }
    return(result)
  }

  if (verbose) {
    message("  SG_PARTIDO column not available; cannot filter by party name")
  }
  tse_votos
}

#' @noRd
.br_extract_turnout <- function(tse_votos, grp_cols) {
  turnout_cols <- intersect(c("QT_APTOS", "QT_ABSTENCOES"), names(tse_votos))
  if (length(turnout_cols) == 0) return(NULL)

  tse_votos |>
    dplyr::mutate(dplyr::across(dplyr::all_of(turnout_cols), as.integer)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(grp_cols)),
      dplyr::across(dplyr::all_of(turnout_cols)))
}

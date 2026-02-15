#' One-step interpolation for Brazilian elections
#'
#' High-level wrapper that auto-downloads census data, electoral data,
#' tract geometries, and OSM road networks, then runs the full optimization
#' and interpolation pipeline. The user only needs to provide an IBGE
#' municipality code and an election year.
#'
#' Internally calls [interpolate_election()] after preparing all inputs.
#'
#' @param code_muni Numeric or character. 7-digit IBGE municipality code
#'   (e.g., `3550308` for Sao Paulo, `3170701` for Varginha). The TSE code
#'   and state abbreviation are resolved automatically.
#' @param year Integer. Election year. Brazil holds two types of elections:
#'
#'   - **Municipal** (even years divisible by 4): 2000, 2004, 2008, 2012,
#'     2016, 2020, 2024. Offices: prefeito, vereador.
#'   - **General/federal** (even years *not* divisible by 4): 2002, 2006,
#'     2010, 2014, 2018, 2022. Offices: presidente, governador, senador,
#'     deputado federal, deputado estadual.
#' @param comparecimento_path Character or NULL. Path to attendance/turnout
#'   parquet file. If NULL (the default), turnout is computed from the vote
#'   data itself.
#' @param votacao_path Character or NULL. Path to candidate votes parquet
#'   file. If NULL (the default), vote data is auto-downloaded from the
#'   TSE open data portal.
#' @param network_path Character or NULL. Path to a directory containing an
#'   OSM `.pbf` file for r5r routing. If NULL and `time_matrix` is also
#'   NULL, OSM data is auto-downloaded via [download_r5r_data()].
#' @param time_matrix Numeric matrix or NULL. Pre-computed travel time
#'   matrix \[n x m\]. If provided, skips all travel time computation
#'   (no r5r, no OSM download). Useful for re-running with different
#'   parameters on the same municipality.
#' @param cargo Integer, character, or NULL. Which electoral office(s) to
#'   include. Accepts one or more human-readable aliases (case-insensitive)
#'   or TSE numeric codes:
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
#'   When `NULL` (the default), **all offices available** for the election
#'   year are included. When multiple cargos are selected, output columns
#'   are prefixed with the office name (e.g., `PRESIDENTE_CAND_13`,
#'   `GOVERNADOR_CAND_22`).
#' @param turno Integer. Election round: `1` (first round, the default) or
#'   `2` (runoff). The first round always exists. The second round
#'   (turno 2) is only held for presidente, governador, and prefeito
#'   (in cities with >200k voters), and only when no candidate wins an
#'   outright majority. If turno 2 did not occur, the result will contain
#'   zero vote rows for those offices.
#' @param what Character vector. Controls **what information** is
#'   interpolated into census tracts. One or more of:
#'
#'   - `"candidates"` **(default)**: Vote counts per candidate. Creates
#'     one column per candidate, named `CAND_<number>` (e.g., `CAND_13`,
#'     `CAND_22`). Also includes `QT_COMPARECIMENTO` (total turnout).
#'   - `"parties"`: Vote counts aggregated by party. Creates one column
#'     per party, named `PARTY_<abbreviation>` (e.g., `PARTY_PT`,
#'     `PARTY_PL`). Also includes `QT_COMPARECIMENTO`.
#'   - `"turnout"`: Turnout and abstention. Creates `QT_COMPARECIMENTO`
#'     (voters who showed up), `QT_APTOS` (eligible voters), and
#'     `QT_ABSTENCOES` (abstentions), when available.
#'   - `"demographics"`: Voter profile demographics. Creates `GENERO_*`
#'     columns (e.g., `GENERO_FEMININO`, `GENERO_MASCULINO`) and
#'     `EDUC_*` columns (e.g., `EDUC_SUP_COMP`, `EDUC_FUND_INCOMP`).
#'
#'   Multiple values can be combined:
#'   `what = c("candidates", "parties", "turnout", "demographics")`.
#' @param candidates Character or numeric vector, or NULL. Filter specific
#'   candidates (only used when `"candidates" %in% what`):
#'
#'   - **By number** (numeric): Matches the candidate's ballot number
#'     exactly. Example: `candidates = c(13, 22)` keeps only candidates
#'     13 (Lula) and 22 (Bolsonaro) in 2022.
#'   - **By name** (character): Performs accent-normalized,
#'     case-insensitive substring matching against the candidate's
#'     registered name. Example: `candidates = "LULA"` matches
#'     "LUIZ INACIO LULA DA SILVA".
#'   - `NULL` **(default)**: All candidates are included (including
#'     special codes 95 = votos em branco, 96 = votos nulos).
#' @param parties Character vector or NULL. Filter specific parties
#'   (only used when `"parties" %in% what`). Uses official TSE party
#'   abbreviations, matched case-insensitively:
#'
#'   - Example: `parties = c("PT", "PL")` keeps only PT and PL.
#'   - `NULL` **(default)**: All parties are included.
#' @param interp_sources Character vector or NULL. Column names from the
#'   electoral data to interpolate. Default `NULL` auto-selects based on
#'   `what`. Override this only if you need fine-grained control over
#'   which columns are interpolated.
#' @param census_year Integer or NULL. Census year for population data
#'   (2000, 2010, or 2022). If `NULL` (the default), auto-selected based
#'   on the election year:
#'   - Elections 2000-2004 use Census 2000
#'   - Elections 2008-2016 use Census 2010
#'   - Elections 2020+ use Census 2022
#' @param clip_sf `sf` polygon or NULL. Optional geometry to clip tracts
#'   (e.g., an urban area boundary). Tracts outside this polygon are
#'   removed before interpolation.
#' @param remove_unpopulated Logical. Remove zero-population tracts.
#'   Default: TRUE.
#' @param osm_buffer_km Numeric. Buffer in km for OSM bounding box
#'   expansion. Default: 10.
#' @param osm_provider Character. OSM extract provider for `osmextract`.
#'   Default: `"openstreetmap_fr"` (has state-level extracts for Brazil).
#'   Alternatives: `"geofabrik"`, `"bbbike"`. Only used when OSM data is
#'   auto-downloaded (no `network_path` or `time_matrix` provided).
#' @param keep Character vector or NULL. Names of heavy intermediate
#'   objects to include in the result. Default NULL (lightweight).
#'   Options: `"weights"`, `"time_matrix"`, `"sources_sf"`.
#'   See [interpolate_election()] for details.
#' @param alpha Numeric vector or NULL. Pre-computed decay parameters
#'   (one per tract). If provided, the optimization step is skipped
#'   entirely. Useful for re-interpolating with a previously optimized
#'   alpha.
#' @param offset Numeric. Travel time offset. Default: 1.
#' @param use_gpu Logical or NULL. Passed to [optimize_alpha()].
#' @param cache Logical. If TRUE (default), downloaded files (TSE data,
#'   OSM networks, census tracts) are stored persistently across R sessions.
#'   See [get_interpElections_cache_dir()]. Subsequent calls reuse cached files,
#'   making re-runs much faster.
#' @param force Logical. Re-download even if cached file exists.
#'   Default: FALSE.
#' @param verbose Logical. Default: TRUE.
#' @param ... Additional arguments forwarded to [interpolate_election()],
#'   [optimize_alpha()], [compute_travel_times()], and/or
#'   [download_r5r_data()].
#'
#' @return A list of class `"interpElections_result"` with components:
#' \describe{
#'   \item{interpolated}{Numeric matrix \[n x p\]. Rows = census tracts,
#'     columns = interpolated variables.}
#'   \item{alpha}{Numeric vector of length n. Optimized decay parameters.}
#'   \item{tracts_sf}{`sf` object with interpolated columns joined in,
#'     ready for mapping with `plot()` or `ggplot2`.}
#'   \item{sources}{Data frame of prepared electoral data (one row
#'     per voting location), without geometry.}
#'   \item{optimization}{`interpElections_optim` or NULL (if alpha was
#'     pre-supplied).}
#'   \item{offset}{Numeric. Offset value used.}
#'   \item{call}{The matched call.}
#'   \item{zone_id}{Character. Name of zone ID column.}
#'   \item{point_id}{Character. Name of source point ID column.}
#'   \item{interp_cols}{Character vector. Names of interpolated columns.}
#'   \item{calib_cols}{List with `$zones` and `$sources` calibration columns.}
#'   \item{weights}{Numeric matrix or NULL. Present only when
#'     `keep` includes `"weights"`.}
#'   \item{time_matrix}{Numeric matrix or NULL. Present only when
#'     `keep` includes `"time_matrix"`.}
#'   \item{sources_sf}{`sf` point object or NULL. Present only when
#'     `keep` includes `"sources_sf"`.}
#'   \item{code_muni}{IBGE municipality code.}
#'   \item{year}{Election year.}
#'   \item{census_year}{Census year.}
#'   \item{what}{Character vector of data types interpolated.}
#'   \item{pop_data}{Data frame of census population by tract.}
#' }
#'
#' @section Election types:
#'
#' Brazil holds elections every two years, alternating between municipal
#' and general (federal/state) elections:
#'
#' - **Municipal elections** (2000, 2004, 2008, 2012, 2016, 2020, 2024):
#'   elect prefeito (mayor) and vereador (city councilor).
#' - **General elections** (2002, 2006, 2010, 2014, 2018, 2022): elect
#'   presidente, governador, senador, deputado federal, and deputado
#'   estadual.
#'
#' Presidential vote data is published by the TSE in a separate national
#' file (~250 MB). This function handles the download automatically
#' when `cargo` includes `"presidente"` or when `cargo = NULL` in a
#' general election year.
#'
#' @section Output columns:
#'
#' The `tracts_sf` output contains the original census tract geometry plus
#' interpolated columns. Column names follow these patterns:
#'
#' \describe{
#'   \item{`CAND_<number>`}{Interpolated vote count for candidate with
#'     ballot number `<number>`. Special numbers: 95 = blank votes
#'     (em branco), 96 = null votes (nulo).}
#'   \item{`PARTY_<abbrev>`}{Interpolated total votes for party
#'     `<abbrev>` (e.g., `PARTY_PT`, `PARTY_PL`).}
#'   \item{`GENERO_<category>`}{Interpolated voter count by gender
#'     (e.g., `GENERO_FEMININO`, `GENERO_MASCULINO`).}
#'   \item{`EDUC_<level>`}{Interpolated voter count by education level
#'     (e.g., `EDUC_SUP_COMP`, `EDUC_FUND_INCOMP`).}
#'   \item{`QT_COMPARECIMENTO`}{Total voters who showed up.}
#'   \item{`QT_APTOS`}{Total eligible voters (when available).}
#'   \item{`QT_ABSTENCOES`}{Total abstentions (when available).}
#' }
#'
#' When multiple `cargo` values are selected, candidate and party columns
#' are prefixed: `PRESIDENTE_CAND_13`, `GOVERNADOR_PARTY_PT`, etc.
#'
#' @examples
#' \dontrun{
#' # ── Minimal usage ───────────────────────────────────────────────
#' # Just an IBGE code + year. Everything else is auto-downloaded.
#' result <- interpolate_election_br(
#'   code_muni = 3550308,   # Sao Paulo
#'   year = 2020
#' )
#'
#' # The result includes an sf object ready for mapping
#' plot(result$tracts_sf["CAND_13"])
#'
#'
#' # ── Choosing a specific cargo ───────────────────────────────────
#' # Municipal election: only city councilors (vereador)
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2020,
#'   cargo = "vereador"
#' )
#'
#' # General election: only the presidential race
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "presidente"
#' )
#'
#' # Multiple offices at once (columns are prefixed)
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = c("presidente", "governador")
#' )
#' # -> columns: PRESIDENTE_CAND_13, GOVERNADOR_CAND_30, ...
#'
#'
#' # ── Turno (election round) ─────────────────────────────────────
#' # First round (default)
#' r1 <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "presidente", turno = 1
#' )
#'
#' # Runoff (second round) -- only 2 candidates
#' r2 <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "presidente", turno = 2
#' )
#'
#'
#' # ── Choosing what to interpolate ───────────────────────────────
#' # Party vote totals instead of individual candidates
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "governador",
#'   what = "parties"
#' )
#' # -> columns: PARTY_PT, PARTY_PL, PARTY_MDB, ...
#'
#' # Voter demographics (gender + education)
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   what = "demographics"
#' )
#' # -> columns: GENERO_FEMININO, EDUC_SUP_COMP, ...
#'
#' # Turnout and abstention
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2020,
#'   cargo = "prefeito",
#'   what = "turnout"
#' )
#' # -> columns: QT_COMPARECIMENTO, QT_APTOS, QT_ABSTENCOES
#'
#' # Everything at once
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "governador",
#'   what = c("candidates", "parties", "turnout", "demographics")
#' )
#'
#'
#' # ── Filtering candidates ───────────────────────────────────────
#' # By ballot number
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "presidente",
#'   candidates = c(13, 22)
#' )
#' # -> only CAND_13 (Lula) and CAND_22 (Bolsonaro)
#'
#' # By name (accent-insensitive substring search)
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "presidente",
#'   candidates = "LULA"
#' )
#' # -> matches "LUIZ INACIO LULA DA SILVA"
#'
#'
#' # ── Filtering parties ──────────────────────────────────────────
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "presidente",
#'   what = "parties",
#'   parties = c("PT", "PL")
#' )
#' # -> only PARTY_PT and PARTY_PL
#'
#'
#' # ── Re-using a previous result ─────────────────────────────────
#' # Keep the time_matrix for reuse (opt-in via keep)
#' result <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "governador",
#'   keep = "time_matrix"
#' )
#'
#' # Then reuse the alpha and travel time matrix
#' result2 <- interpolate_election_br(
#'   code_muni = 3170701, year = 2022,
#'   cargo = "governador",
#'   what = "parties",
#'   time_matrix = result$time_matrix,
#'   alpha = result$alpha
#' )
#'
#'
#' # ── Pre-computed travel times (skip r5r) ───────────────────────
#' result <- interpolate_election_br(
#'   code_muni = 3550308, year = 2020,
#'   time_matrix = my_tt_matrix
#' )
#' }
#'
#' @family wrappers
#'
#' @seealso [interpolate_election()] for the general-purpose wrapper,
#'   [br_prepare_population()], [br_prepare_electoral()],
#'   [br_prepare_tracts()].
#'
#' @export
interpolate_election_br <- function(
    code_muni,
    year,
    comparecimento_path = NULL,
    votacao_path        = NULL,
    network_path        = NULL,
    time_matrix         = NULL,
    cargo               = NULL,
    turno               = 1L,
    what                = "candidates",
    candidates          = NULL,
    parties             = NULL,
    interp_sources      = NULL,
    census_year         = NULL,
    clip_sf             = NULL,
    remove_unpopulated  = TRUE,
    osm_buffer_km       = 10,
    osm_provider        = "openstreetmap_fr",
    keep                = NULL,
    alpha               = NULL,
    offset              = 1,
    use_gpu             = NULL,
    cache               = TRUE,
    force               = FALSE,
    verbose             = TRUE,
    ...
) {
  cl <- match.call()
  code_muni <- as.character(code_muni)

  # --- Check all dependencies upfront ---
  missing_pkgs <- character(0)
  for (pkg in c("sf", "dplyr", "censobr", "geobr", "data.table", "stringr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, pkg)
    }
  }
  if (is.null(time_matrix) && is.null(network_path)) {
    # Will need to download OSM and compute travel times
    for (pkg in c("osmextract", "r5r")) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        missing_pkgs <- c(missing_pkgs, pkg)
      }
    }
  } else if (is.null(time_matrix)) {
    # Have network_path but need r5r to compute travel times
    if (!requireNamespace("r5r", quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, "r5r")
    }
  }
  if (length(missing_pkgs) > 0) {
    stop(
      "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
      "\nInstall with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))",
      call. = FALSE
    )
  }

  # Check for OSM clipping tools if travel times will be computed
  if (is.null(time_matrix) && is.null(network_path) && !.has_clip_tool()) {
    .offer_osmium_install(verbose = verbose)
    if (!.has_clip_tool()) {
      stop(
        "A clipping tool (osmium or osmconvert) is required ",
        "to clip state-level OSM files for r5r routing.\n\n",
        "Run interpElections::setup_osmium() to install, ",
        "or install manually:\n",
        "  Windows: conda install -c conda-forge osmium-tool\n",
        "  macOS:   brew install osmium-tool\n",
        "  Linux:   sudo apt install osmium-tool",
        call. = FALSE
      )
    }
  }

  # --- Compute step counts for unified numbering ---
  # Outer steps: 5 (resolve, population, tracts, electoral, calibration)
  # Inner steps (interpolate_election): depends on travel time availability
  #   time_matrix provided:   2 (optimize + interpolate)
  #   network_path provided:  3 (+ compute travel times)
  #   nothing provided:       4 (+ download OSM + compute travel times)
  # Cache hits are determined inside interpolate_election, so we estimate
  # the maximum and let inner function adjust.
  .outer_steps <- 5L
  .inner_estimate <- if (!is.null(time_matrix)) 2L
                     else if (!is.null(network_path)) 3L
                     else 4L
  .total_steps <- .outer_steps + .inner_estimate

  # --- Step 1: Resolve IBGE -> TSE code + UF ---
  if (verbose) message(sprintf("[1/%d] Resolving municipality identifiers...",
                               .total_steps))
  muni_info <- .br_resolve_muni(code_muni)
  if (verbose) {
    message(sprintf("  %s (%s) - IBGE: %s, TSE: %s",
                    muni_info$nome_municipio, muni_info$uf,
                    muni_info$code_muni_ibge,
                    muni_info$code_muni_tse))
  }

  # --- Step 2: Auto-select census year ---
  # Uses the most recent census that precedes or coincides with the election:
  #   Elections 2000-2004 -> Census 2000
  #   Elections 2008-2016 -> Census 2010
  #   Elections 2020+     -> Census 2022
  if (is.null(census_year)) {
    census_year <- if (year <= 2004) {
      2000L
    } else if (year <= 2016) {
      2010L
    } else {
      2022L
    }
    if (verbose) {
      message(sprintf("  Census year: %d (election %d)",
                      census_year, year))
    }
  }

  # --- Step 3: Census population ---
  if (verbose) message(sprintf("[2/%d] Preparing census population data...",
                               .total_steps))
  pop_data <- br_prepare_population(
    code_muni = code_muni,
    year = census_year
  )

  # --- Step 4: Census tract geometries ---
  if (verbose) message(sprintf("[3/%d] Preparing census tract geometries...",
                               .total_steps))
  tracts_sf <- br_prepare_tracts(
    code_muni = code_muni,
    pop_data = pop_data,
    remove_unpopulated = remove_unpopulated,
    clip_sf = clip_sf,
    year = census_year
  )

  # --- Step 5: Electoral data ---
  if (verbose) message(sprintf("[4/%d] Preparing electoral data...",
                               .total_steps))
  # Forward geocode_path from ... if provided
  dots <- list(...)
  geocode_path <- dots$geocode_path

  electoral_data <- br_prepare_electoral(
    code_muni_ibge = muni_info$code_muni_ibge,
    code_muni_tse = muni_info$code_muni_tse,
    uf = muni_info$uf,
    year = year,
    cargo = cargo,
    turno = turno,
    what = what,
    candidates = candidates,
    parties = parties,
    comparecimento_path = comparecimento_path,
    votacao_path = votacao_path,
    geocode_path = geocode_path,
    cache = cache,
    force = force,
    verbose = verbose
  )

  # Build sf point object from electoral data
  elec_valid <- electoral_data[
    !is.na(electoral_data$lat) & !is.na(electoral_data$long),
  ]
  electoral_sf <- sf::st_as_sf(
    elec_valid,
    coords = c("long", "lat"),
    crs = 4326
  )
  electoral_sf$id <- as.character(electoral_sf$id)

  # Ensure tracts have an id column
  tracts_sf$id <- as.character(tracts_sf$code_tract)

  # --- Step 6: Match calibration brackets ---
  if (verbose) message(sprintf("[5/%d] Matching calibration brackets...",
                               .total_steps))
  calib <- .br_match_calibration(
    census_year = census_year,
    tracts_sf = tracts_sf,
    electoral_sf = electoral_sf
  )
  tracts_sf <- calib$tracts_sf
  electoral_sf <- calib$electoral_sf

  # --- Step 7: Determine interpolation columns ---
  elec_df <- sf::st_drop_geometry(electoral_sf)
  if (is.null(interp_sources)) {
    auto_sources <- calib$calib_sources

    if ("candidates" %in% what) {
      auto_sources <- c(auto_sources,
        grep("CAND_", names(elec_df), value = TRUE))
    }
    if ("parties" %in% what) {
      auto_sources <- c(auto_sources,
        grep("PARTY_", names(elec_df), value = TRUE))
    }
    if ("turnout" %in% what) {
      auto_sources <- c(auto_sources, intersect(
        c("QT_COMPARECIMENTO", "QT_APTOS", "QT_ABSTENCOES"),
        names(elec_df)))
    } else if (any(c("candidates", "parties") %in% what)) {
      if ("QT_COMPARECIMENTO" %in% names(elec_df)) {
        auto_sources <- c(auto_sources, "QT_COMPARECIMENTO")
      }
    }
    if ("demographics" %in% what) {
      auto_sources <- c(auto_sources,
        grep("^GENERO_", names(elec_df), value = TRUE),
        grep("^EDUC_", names(elec_df), value = TRUE))
    }

    interp_sources <- unique(auto_sources)
    if (length(interp_sources) == 0) {
      interp_sources <- calib$calib_sources
    }
  }

  # --- Step 8: Call interpolate_election() ---
  # Pass step offset so inner steps continue the unified numbering
  ie_result <- interpolate_election(
    tracts_sf = tracts_sf,
    electoral_sf = electoral_sf,
    zone_id = "id",
    point_id = "id",
    calib_zones = calib$calib_zones,
    calib_sources = calib$calib_sources,
    interp_sources = interp_sources,
    time_matrix = time_matrix,
    network_path = network_path,
    osm_buffer_km = osm_buffer_km,
    min_pop = 0,  # already filtered via br_prepare_tracts
    alpha = alpha,
    offset = offset,
    keep = keep,
    use_gpu = use_gpu,
    verbose = verbose,
    osm_provider = osm_provider,
    ...,
    .step_offset = .outer_steps,
    .step_total = .total_steps
  )

  if (verbose) message("\nDone.")

  # --- Set Brazilian metadata on the unified result ---
  ie_result$code_muni   <- code_muni
  ie_result$year        <- as.integer(year)
  ie_result$census_year <- as.integer(census_year)
  ie_result$what        <- what
  ie_result$pop_data    <- pop_data
  ie_result$call        <- cl  # Override with br call

  ie_result
}


# --- Internal: resolve IBGE code to TSE code + UF ---

.br_resolve_muni <- function(code_muni_ibge) {
  code_muni_ibge <- as.character(code_muni_ibge)

  # Use bundled crosswalk data (interpElections::muni_crosswalk)
  xwalk <- muni_crosswalk
  row <- xwalk[xwalk$code_ibge == code_muni_ibge, ]

  if (nrow(row) == 0) {
    stop(sprintf("IBGE code '%s' not found in crosswalk table",
                 code_muni_ibge), call. = FALSE)
  }

  list(
    code_muni_ibge = row$code_ibge[1],
    code_muni_tse  = row$code_tse[1],
    uf             = row$uf[1],
    nome_municipio = row$nome[1]
  )
}


# --- Internal: match calibration brackets ---
# Aggregates TSE voter brackets to match census population brackets.

.br_match_calibration <- function(census_year, tracts_sf, electoral_sf) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required", call. = FALSE)
  }

  tracts_df <- sf::st_drop_geometry(tracts_sf)
  elec_df <- sf::st_drop_geometry(electoral_sf)

  # Get pop columns from census
  pop_cols <- grep("^pop_", names(tracts_df), value = TRUE)
  vot_cols <- grep("^votantes_", names(elec_df), value = TRUE)

  if (census_year %in% c(2000, 2010)) {
    # Census produces: pop_18_20, pop_21_24, pop_25_29, pop_30_39,
    #   pop_40_49, pop_50_59, pop_60_69
    # TSE produces: votantes_18_20, ..., votantes_65_69 (11 fine groups)
    # Need to aggregate TSE's fine groups to match census coarser groups

    calib_zones <- c("pop_18_20", "pop_21_24", "pop_25_29",
                     "pop_30_39", "pop_40_49", "pop_50_59",
                     "pop_60_69")

    # Aggregate TSE voter columns
    electoral_sf$votantes_30_39 <- .safe_sum(
      elec_df, c("votantes_30_34", "votantes_35_39"))
    electoral_sf$votantes_40_49 <- .safe_sum(
      elec_df, c("votantes_40_44", "votantes_45_49"))
    electoral_sf$votantes_50_59 <- .safe_sum(
      elec_df, c("votantes_50_54", "votantes_55_59"))
    electoral_sf$votantes_60_69 <- .safe_sum(
      elec_df, c("votantes_60_64", "votantes_65_69"))

    calib_sources <- c("votantes_18_20", "votantes_21_24",
                       "votantes_25_29", "votantes_30_39",
                       "votantes_40_49", "votantes_50_59",
                       "votantes_60_69")

  } else {
    # Census 2022 produces: pop_15_19, pop_20_24, pop_25_29, pop_30_39,
    #   pop_40_49, pop_50_59, pop_60_69
    # 15-19 and 20-24 don't match TSE's 18-20 and 21-24
    # Combine both sides: pop_15_24 <-> votantes_15_24

    tracts_sf$pop_15_24 <- tracts_df$pop_15_19 + tracts_df$pop_20_24

    electoral_sf$votantes_15_24 <- .safe_sum(
      elec_df, c("votantes_18_20", "votantes_21_24"))
    electoral_sf$votantes_30_39 <- .safe_sum(
      elec_df, c("votantes_30_34", "votantes_35_39"))
    electoral_sf$votantes_40_49 <- .safe_sum(
      elec_df, c("votantes_40_44", "votantes_45_49"))
    electoral_sf$votantes_50_59 <- .safe_sum(
      elec_df, c("votantes_50_54", "votantes_55_59"))
    electoral_sf$votantes_60_69 <- .safe_sum(
      elec_df, c("votantes_60_64", "votantes_65_69"))

    calib_zones <- c("pop_15_24", "pop_25_29", "pop_30_39",
                     "pop_40_49", "pop_50_59", "pop_60_69")
    calib_sources <- c("votantes_15_24", "votantes_25_29",
                       "votantes_30_39", "votantes_40_49",
                       "votantes_50_59", "votantes_60_69")
  }

  # Verify all columns exist
  tracts_df2 <- sf::st_drop_geometry(tracts_sf)
  elec_df2 <- sf::st_drop_geometry(electoral_sf)

  missing_z <- setdiff(calib_zones, names(tracts_df2))
  if (length(missing_z) > 0) {
    stop("Missing calibration pop columns: ",
         paste(missing_z, collapse = ", "), call. = FALSE)
  }
  missing_s <- setdiff(calib_sources, names(elec_df2))
  if (length(missing_s) > 0) {
    stop("Missing calibration voter columns: ",
         paste(missing_s, collapse = ", "), call. = FALSE)
  }

  list(
    calib_zones = calib_zones,
    calib_sources = calib_sources,
    tracts_sf = tracts_sf,
    electoral_sf = electoral_sf
  )
}

# Sum columns safely (returns 0 for missing columns, warns about missing)
.safe_sum <- function(df, cols) {
  present <- intersect(cols, names(df))
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    warning(sprintf(
      "Columns not found (treated as 0): %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  if (length(present) == 0) return(rep(0, nrow(df)))
  rowSums(df[, present, drop = FALSE], na.rm = TRUE)
}

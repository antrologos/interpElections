#' One-step interpolation for Brazilian elections
#'
#' High-level wrapper that auto-downloads census data, electoral data,
#' tract geometries, and OSM road networks, then runs the full optimization
#' and interpolation pipeline. The user only needs to provide an IBGE
#' municipality code (or name) and an election year.
#'
#' Internally calls [interpolate_election()] after preparing all inputs.
#'
#' @param code_muni Numeric or character. Either a 7-digit IBGE municipality
#'   code (e.g., `3550308` for Sao Paulo) or a municipality **name** (e.g.,
#'   `"Sao Paulo"`, `"SAO PAULO"`, `"São Paulo"`). Name matching is
#'   case-insensitive, accent-insensitive, and whitespace-trimmed. If the
#'   name exists in multiple states, use the `uf` parameter to disambiguate.
#'   The TSE code and state abbreviation are resolved automatically from the
#'   bundled [muni_crosswalk] table.
#' @param uf Character or NULL. Two-letter state abbreviation (e.g., `"SP"`,
#'   `"RJ"`) for disambiguation when `code_muni` is a municipality name that
#'   exists in multiple states. Case-insensitive. Ignored (with a message)
#'   when `code_muni` is a numeric IBGE code. Default: `NULL`.
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
#' @param point_method Character. Method for computing representative points
#'   for census tracts. One of `"point_on_surface"` (default, guaranteed
#'   inside polygon), `"centroid"`, or `"pop_weighted"` (uses WorldPop
#'   raster for large tracts). See [compute_representative_points()].
#' @param pop_raster A [terra::SpatRaster], file path, or `NULL`. Population
#'   density raster for `point_method = "pop_weighted"`. If `NULL`, WorldPop
#'   Brazil Constrained 2020 (~48 MB) is auto-downloaded.
#' @param pop_min_area Numeric. Minimum tract area (km²) for applying the
#'   population-weighted method. Smaller tracts use `point_on_surface`.
#'   Default: 1.
#' @param keep Character vector or NULL. Names of heavy intermediate
#'   objects to include in the result. Default NULL (lightweight).
#'   Options: `"weights"`, `"time_matrix"`, `"sources_sf"`,
#'   `"pop_raster"`, `"rep_points"`, `"neighborhoods"`.
#'   The first five are passed to [interpolate_election()]; see its docs.
#'   `"neighborhoods"` downloads IBGE 2010 neighborhood boundaries via
#'   [geobr::read_neighborhood()] and stores them in `$neighborhoods`.
#' @param alpha Numeric vector or NULL. Pre-computed decay parameters
#'   (one per tract). If provided, the optimization step is skipped
#'   entirely. Useful for re-interpolating with a previously optimized
#'   alpha.
#' @param offset Numeric. Travel time offset. Default: 1.
#' @param calib_type Character. Calibration column type:
#'   - `"full"` **(default)**: gender (male/female) × 7 age brackets.
#'     Provides stronger spatial signal for the optimizer.
#'   - `"age_only"`: 7 age-bracket-only columns (ignores gender).
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
#'   \item{tract_id}{Character. Name of census tract ID column.}
#'   \item{point_id}{Character. Name of source point ID column.}
#'   \item{interp_cols}{Character vector. Names of interpolated columns.}
#'   \item{calib_cols}{List with `$tracts` and `$sources` calibration columns.}
#'   \item{weights}{Numeric matrix or NULL. Present only when
#'     `keep` includes `"weights"`.}
#'   \item{time_matrix}{Numeric matrix or NULL. Present only when
#'     `keep` includes `"time_matrix"`.}
#'   \item{sources_sf}{`sf` point object or NULL. Present only when
#'     `keep` includes `"sources_sf"`.}
#'   \item{neighborhoods}{`sf` polygon object or NULL. IBGE 2010 neighborhood
#'     boundaries for the municipality. Present only when `keep` includes
#'     `"neighborhoods"`.}
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
    uf                  = NULL,
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
    point_method        = "point_on_surface",
    pop_raster          = NULL,
    pop_min_area        = 1,
    keep                = NULL,
    alpha               = NULL,
    offset              = 1,
    calib_type          = "full",
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
  muni_info <- .br_resolve_muni(code_muni, uf = uf)
  code_muni <- muni_info$code_muni_ibge
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
    electoral_sf = electoral_sf,
    calib_type = calib_type
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
  # Construct state-level OSM URL to avoid oe_match() selecting
  # overly broad extracts (country/continent level)
  osm_url <- if (osm_provider == "openstreetmap_fr") {
    .br_osm_url(muni_info$uf)
  } else {
    NULL
  }

  ie_result <- interpolate_election(
    tracts_sf = tracts_sf,
    electoral_sf = electoral_sf,
    tract_id = "id",
    point_id = "id",
    calib_tracts = calib$calib_tracts,
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
    osm_url = osm_url,
    point_method = point_method,
    pop_raster = pop_raster,
    pop_min_area = pop_min_area,
    ...,
    .step_offset = .outer_steps,
    .step_total = .total_steps
  )

  if (verbose) message("\nDone.")

  # --- Set Brazilian metadata on the unified result ---
  ie_result$code_muni      <- code_muni
  ie_result$nome_municipio <- muni_info$nome_municipio
  ie_result$code_muni_tse  <- muni_info$code_muni_tse
  ie_result$uf             <- muni_info$uf
  ie_result$year           <- as.integer(year)
  ie_result$census_year    <- as.integer(census_year)
  ie_result$what        <- what
  ie_result$pop_data    <- pop_data
  ie_result$call        <- cl  # Override with br call

  # --- Build column dictionary ---
  ie_result$dictionary <- .br_build_dictionary(
    electoral_data, ie_result$interp_cols, calib, what)

  # --- Download neighborhood boundaries (opt-in) ---
  if ("neighborhoods" %in% keep) {
    if (verbose) message("Downloading neighborhood boundaries...")
    ie_result$neighborhoods <- .br_download_neighborhoods(code_muni)
  }

  ie_result
}


# --- Internal: resolve municipality identifier to TSE code + UF ---
# Accepts either a 7-digit IBGE code (numeric or character) or a municipality
# name.  When using a name, the optional `uf` narrows matches to one state.

.br_resolve_muni <- function(code_muni, uf = NULL) {
  code_muni <- trimws(as.character(code_muni))
  xwalk <- muni_crosswalk

  # Detect whether input is a numeric IBGE code or a name
  is_code <- grepl("^[0-9]+$", code_muni)

  if (is_code) {
    # --- IBGE code path ---
    if (!is.null(uf)) {
      message("Note: `uf` is ignored when `code_muni` is a numeric IBGE code.")
    }
    row <- xwalk[xwalk$code_ibge == code_muni, ]
    if (nrow(row) == 0) {
      stop(sprintf("IBGE code '%s' not found in crosswalk table",
                   code_muni), call. = FALSE)
    }
  } else {
    # --- Name-based lookup ---
    input_norm <- toupper(trimws(.br_remove_accents(code_muni)))
    xwalk_norm <- toupper(trimws(.br_remove_accents(xwalk$nome)))

    matches <- which(xwalk_norm == input_norm)

    # Apply UF filter if provided
    if (!is.null(uf)) {
      uf_norm <- toupper(trimws(uf))
      matches <- matches[toupper(xwalk$uf[matches]) == uf_norm]
    }

    if (length(matches) == 0) {
      if (!is.null(uf)) {
        stop(sprintf(
          "Municipality '%s' not found in state '%s'.\n  Check spelling or try without the `uf` parameter to see all matches.",
          code_muni, toupper(trimws(uf))
        ), call. = FALSE)
      } else {
        stop(sprintf(
          "Municipality name '%s' not found in crosswalk table.\n  Check spelling. Names are matched case- and accent-insensitively.",
          code_muni
        ), call. = FALSE)
      }
    }

    if (length(matches) > 1) {
      dup_rows <- xwalk[matches, ]
      uf_list <- paste(
        sprintf("  %s (%s) - IBGE: %s", dup_rows$nome, dup_rows$uf, dup_rows$code_ibge),
        collapse = "\n"
      )
      stop(sprintf(
        "Municipality name '%s' matches %d municipalities in different states:\n%s\n\nUse the `uf` parameter to disambiguate, e.g.: uf = \"%s\"",
        code_muni, length(matches), uf_list, dup_rows$uf[1]
      ), call. = FALSE)
    }

    row <- xwalk[matches, ]
  }

  list(
    code_muni_ibge = row$code_ibge[1],
    code_muni_tse  = row$code_tse[1],
    uf             = row$uf[1],
    nome_municipio = row$nome[1]
  )
}


# --- Internal: build column dictionary for the result ---
# Combines the electoral dictionary (from br_prepare_electoral) with entries
# for calibration, turnout, and demographics columns.

.br_build_dictionary <- function(electoral_data, interp_cols, calib, what) {
  # Start with electoral metadata (candidates + parties)
  dict <- attr(electoral_data, "column_dictionary")

  # Fallback: if no dictionary attribute (e.g. old cached data), build
  # basic dictionary entries from column name patterns
  if (is.null(dict)) {
    dict <- .br_dict_from_colnames(interp_cols)
    message(
      "Note: Using basic dictionary from column names (party/candidate ",
      "metadata may be incomplete).\n",
      "Clear the electoral cache with clear_cache(type = 'electoral') ",
      "and re-run for full metadata."
    )
  }

  # Add calibration columns
  calib_rows <- lapply(calib$calib_sources, function(col) {
    data.frame(
      column = col, type = "calibration", cargo = NA_character_,
      ballot_number = NA_character_, candidate_name = NA_character_,
      party = NA_character_, stringsAsFactors = FALSE)
  })

  # Add turnout columns
  turnout_names <- intersect(
    c("QT_COMPARECIMENTO", "QT_APTOS", "QT_ABSTENCOES"), interp_cols)
  turnout_rows <- lapply(turnout_names, function(col) {
    data.frame(
      column = col, type = "turnout", cargo = NA_character_,
      ballot_number = NA_character_, candidate_name = NA_character_,
      party = NA_character_, stringsAsFactors = FALSE)
  })

  # Add demographics columns
  demo_names <- grep("^(GENERO_|EDUC_)", interp_cols, value = TRUE)
  demo_rows <- lapply(demo_names, function(col) {
    data.frame(
      column = col, type = "demographics", cargo = NA_character_,
      ballot_number = NA_character_, candidate_name = NA_character_,
      party = NA_character_, stringsAsFactors = FALSE)
  })

  all_rows <- c(
    if (!is.null(dict)) list(dict) else list(),
    calib_rows, turnout_rows, demo_rows
  )

  if (length(all_rows) == 0) return(NULL)

  full_dict <- do.call(rbind, all_rows)
  row.names(full_dict) <- NULL

  # Deduplicate: keep first occurrence of each column name
  full_dict <- full_dict[!duplicated(full_dict$column), , drop = FALSE]

  # Filter to only columns actually interpolated
  full_dict <- full_dict[full_dict$column %in% interp_cols, , drop = FALSE]

  if (nrow(full_dict) == 0) return(NULL)
  full_dict
}


# --- Internal: build basic dictionary from column names ---
# Fallback for when electoral data was cached without the dictionary attribute.
# Extracts type and ballot number from patterns like PRESIDENTE_CAND_13,
# PARTY_PT, DEPUTADO_FEDERAL_PARTY_MDB, etc.

.br_dict_from_colnames <- function(interp_cols) {
  # Known non-electoral patterns (handled elsewhere)
  skip_re <- "^(votantes_|pop_|QT_|GENERO_|EDUC_)"

  rows <- list()
  for (col in interp_cols) {
    if (grepl(skip_re, col)) next

    # Candidate columns: *_CAND_<number> or CAND_<number>
    if (grepl("CAND_[0-9]+$", col)) {
      ballot <- sub(".*CAND_", "", col)
      # Extract cargo prefix (everything before _CAND_)
      cargo <- sub("_CAND_[0-9]+$", "", col)
      if (cargo == col) cargo <- NA_character_  # no prefix (bare CAND_13)

      cand_name <- if (ballot == "95") {
        "Votos em Branco"
      } else if (ballot == "96") {
        "Votos Nulos"
      } else {
        NA_character_
      }

      rows[[length(rows) + 1L]] <- data.frame(
        column = col, type = "candidate", cargo = cargo,
        ballot_number = ballot, candidate_name = cand_name,
        party = NA_character_, stringsAsFactors = FALSE
      )
      next
    }

    # Party columns: *_PARTY_<abbr> or PARTY_<abbr>
    if (grepl("PARTY_", col)) {
      party_abbr <- sub(".*PARTY_", "", col)
      cargo <- sub("_PARTY_.*$", "", col)
      if (cargo == col) cargo <- NA_character_

      rows[[length(rows) + 1L]] <- data.frame(
        column = col, type = "party", cargo = cargo,
        ballot_number = NA_character_, candidate_name = NA_character_,
        party = party_abbr, stringsAsFactors = FALSE
      )
      next
    }
  }

  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}


# --- Internal: match calibration brackets ---
# Aggregates TSE voter brackets to match census population brackets.
# calib_type = "age_only": 7 age-only pairs (original behavior)
# calib_type = "full": gender x 7 age pairs

.br_match_calibration <- function(census_year, tracts_sf, electoral_sf,
                                   calib_type = "full") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required", call. = FALSE)
  }
  if (!calib_type %in% c("full", "age_only")) {
    stop("calib_type must be 'full' or 'age_only'", call. = FALSE)
  }

  tracts_df <- sf::st_drop_geometry(tracts_sf)
  elec_df <- sf::st_drop_geometry(electoral_sf)

  # --- Helper: aggregate fine TSE groups to census groups ---
  # Applies to both age-only (votantes_*) and cross-tab (vot_*) columns
  .aggregate_tse_groups <- function(electoral_sf, elec_df, prefix,
                                     is_2022 = FALSE) {
    if (is_2022) {
      electoral_sf[[paste0(prefix, "20_24")]] <- .safe_sum(
        elec_df, c(paste0(prefix, "20"), paste0(prefix, "21_24")))
    } else {
      electoral_sf[[paste0(prefix, "18_20")]] <- .safe_sum(
        elec_df, c(paste0(prefix, "18_19"), paste0(prefix, "20")))
    }
    electoral_sf[[paste0(prefix, "30_39")]] <- .safe_sum(
      elec_df, c(paste0(prefix, "30_34"), paste0(prefix, "35_39")))
    electoral_sf[[paste0(prefix, "40_49")]] <- .safe_sum(
      elec_df, c(paste0(prefix, "40_44"), paste0(prefix, "45_49")))
    electoral_sf[[paste0(prefix, "50_59")]] <- .safe_sum(
      elec_df, c(paste0(prefix, "50_54"), paste0(prefix, "55_59")))
    electoral_sf[[paste0(prefix, "60_69")]] <- .safe_sum(
      elec_df, c(paste0(prefix, "60_64"), paste0(prefix, "65_69")))
    electoral_sf
  }

  is_2022 <- !census_year %in% c(2000, 2010)

  if (is_2022) {
    age_groups <- c("18_19", "20_24", "25_29", "30_39",
                    "40_49", "50_59", "60_69")
  } else {
    age_groups <- c("18_20", "21_24", "25_29", "30_39",
                    "40_49", "50_59", "60_69")
  }

  # Always aggregate age-only TSE columns (needed even in full mode)
  electoral_sf <- .aggregate_tse_groups(
    electoral_sf, elec_df, "votantes_", is_2022 = is_2022)

  if (is_2022) {
    tracts_sf$pop_18_19 <- tracts_df$pop_15_19 * 2 / 5
  }

  if (calib_type == "age_only") {
    # Original behavior: 7 age-only pairs
    calib_tracts <- paste0("pop_", age_groups)
    calib_sources <- paste0("votantes_", age_groups)

  } else {
    # Full calibration: gender x age pairs (14 brackets = 2 genders x 7 ages)
    # IBGE/TSE split by literacy (alf/nalf); we aggregate them here.
    lit_categories <- c("hom_alf", "hom_nalf", "mul_alf", "mul_nalf")

    # Step 1: aggregate fine TSE age groups within each literacy category.
    # Some alf/nalf columns may be absent (e.g., nalf_20 for young cohorts);
    # .safe_sum handles missing columns as zero — suppress those warnings.
    suppressWarnings({
      for (cat in lit_categories) {
        electoral_sf <- .aggregate_tse_groups(
          electoral_sf, elec_df, paste0("vot_", cat, "_"), is_2022 = is_2022)
      }
    })

    if (is_2022) {
      # Apply 2/5 proxy for 18-19 from census 15-19 bracket, per lit category
      for (cat in lit_categories) {
        pop_15_19_col <- paste0("pop_", cat, "_15_19")
        pop_18_19_col <- paste0("pop_", cat, "_18_19")
        if (pop_15_19_col %in% names(tracts_df)) {
          tracts_sf[[pop_18_19_col]] <- tracts_df[[pop_15_19_col]] * 2 / 5
        }
      }
    }

    # Step 2: sum alf + nalf to get gender-only brackets (14 = 2 x 7)
    # Some alf/nalf columns may be absent for certain age groups; that is
    # expected and handled silently by .safe_sum (missing = 0).
    gender_categories <- c("hom", "mul")
    elec_df2_tmp <- sf::st_drop_geometry(electoral_sf)
    tracts_df2_tmp <- sf::st_drop_geometry(tracts_sf)

    suppressWarnings({
      for (gender in gender_categories) {
        for (ag in age_groups) {
          alf_vot  <- paste0("vot_", gender, "_alf_", ag)
          nalf_vot <- paste0("vot_", gender, "_nalf_", ag)
          agg_vot  <- paste0("vot_", gender, "_", ag)
          electoral_sf[[agg_vot]] <- .safe_sum(elec_df2_tmp, c(alf_vot, nalf_vot))

          alf_pop  <- paste0("pop_", gender, "_alf_", ag)
          nalf_pop <- paste0("pop_", gender, "_nalf_", ag)
          agg_pop  <- paste0("pop_", gender, "_", ag)
          tracts_sf[[agg_pop]] <- .safe_sum(tracts_df2_tmp, c(alf_pop, nalf_pop))
        }
      }
    })

    calib_tracts <- paste0("pop_", rep(gender_categories, each = 7), "_",
                           age_groups)
    calib_sources <- paste0("vot_", rep(gender_categories, each = 7), "_",
                            age_groups)
  }

  # Verify all columns exist
  tracts_df2 <- sf::st_drop_geometry(tracts_sf)
  elec_df2 <- sf::st_drop_geometry(electoral_sf)

  missing_z <- setdiff(calib_tracts, names(tracts_df2))
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
    calib_tracts = calib_tracts,
    calib_sources = calib_sources,
    tracts_sf = tracts_sf,
    electoral_sf = electoral_sf
  )
}

# Download IBGE 2010 neighborhood boundaries for a municipality
# Returns an sf object filtered to the given code_muni, or NULL on failure.
.br_download_neighborhoods <- function(code_muni) {
  if (!requireNamespace("geobr", quietly = TRUE)) {
    message("geobr not installed; cannot download neighborhoods.")
    return(NULL)
  }
  tryCatch({
    nbhoods <- suppressMessages(geobr::read_neighborhood(year = 2010))
    nbhoods <- nbhoods[substr(nbhoods$code_neighborhood, 1, 7) == code_muni, ]
    if (nrow(nbhoods) == 0) {
      message("No neighborhoods found for municipality ", code_muni)
      return(NULL)
    }
    nbhoods
  }, error = function(e) {
    message("Could not download neighborhoods: ", conditionMessage(e))
    NULL
  })
}

# --- Internal: construct state-level OSM URL for openstreetmap_fr ---
# Maps a two-letter UF code to the openstreetmap.fr state-level extract URL.
# This avoids relying on osmextract::oe_match() bounding-box matching, which
# can select country/continent-level extracts when polling station geocodes
# have outliers that inflate the bounding box.

.br_osm_url <- function(uf) {
  uf <- toupper(trimws(uf))

  # UF -> c(region, state_slug) for openstreetmap.fr
  uf_map <- list(
    # Norte
    AC = c("north", "acre"),
    AM = c("north", "amazonas"),
    AP = c("north", "amapa"),
    PA = c("north", "para"),
    RO = c("north", "rondonia"),
    RR = c("north", "roraima"),
    TO = c("north", "tocantins"),
    # Nordeste
    AL = c("northeast", "alagoas"),
    BA = c("northeast", "bahia"),
    CE = c("northeast", "ceara"),
    MA = c("northeast", "maranhao"),
    PB = c("northeast", "paraiba"),
    PE = c("northeast", "pernambuco"),
    PI = c("northeast", "piaui"),
    RN = c("northeast", "rio-grande-do-norte"),
    SE = c("northeast", "sergipe"),
    # Centro-Oeste
    DF = c("central-west", "distrito-federal"),
    GO = c("central-west", "goias"),
    MS = c("central-west", "mato-grosso-do-sul"),
    MT = c("central-west", "mato-grosso"),
    # Sudeste
    ES = c("southeast", "espirito-santo"),
    MG = c("southeast", "minas-gerais"),
    RJ = c("southeast", "rio-de-janeiro"),
    SP = c("southeast", "sao-paulo"),
    # Sul
    PR = c("south", "parana"),
    RS = c("south", "rio-grande-do-sul"),
    SC = c("south", "santa-catarina")
  )

  info <- uf_map[[uf]]
  if (is.null(info)) {
    warning(sprintf("Unknown UF '%s'; falling back to oe_match()", uf),
            call. = FALSE)
    return(NULL)
  }

  sprintf(
    "http://download.openstreetmap.fr/extracts/south-america/brazil/%s/%s-latest.osm.pbf",
    info[1], info[2]
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

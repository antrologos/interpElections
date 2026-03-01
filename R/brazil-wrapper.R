#' Interpolate Brazilian election data into census tracts
#'
#' Downloads census data, electoral results, tract geometries, and road
#' networks automatically. Only requires a municipality identifier and
#' an election year. Internally calls [interpolate_election()] after
#' preparing all inputs.
#'
#' @param municipality Numeric or character. Either a 7-digit IBGE
#'   municipality code (e.g., `3550308` for Sao Paulo) or a municipality
#'   **name** (e.g., `"Sao Paulo"`, `"São Paulo"`). Name matching is
#'   case/accent-insensitive. Use `uf` to disambiguate if the name exists
#'   in multiple states.
#' @param year Integer. Election year (even integer).
#' @param cargo Integer, character, or NULL. Electoral office(s). Accepts
#'   aliases like `"presidente"`, `"vereador"` or TSE codes. NULL =
#'   all offices for that year.
#' @param turno Integer. Election round: 1 (default) or 2 (runoff).
#' @param what Character vector. What to interpolate: `"candidates"`
#'   (default), `"parties"`, `"turnout"`, `"demographics"`.
#'   Multiple values can be combined.
#' @param candidates Character or numeric vector, or NULL. Filter
#'   candidates by ballot number or name substring. Default: all.
#' @param parties Character vector or NULL. Filter parties by TSE
#'   abbreviation (e.g., `c("PT", "PL")`). Default: all.
#' @param uf Two-letter state abbreviation for disambiguation when
#'   `municipality` is a name. Default: NULL.
#' @param census_year Integer or NULL. Census year (2000, 2010, 2022).
#'   NULL = auto-selected from election year.
#' @param clip_tracts_sf `sf` polygon or NULL. Clip tracts to this
#'   geometry before interpolation.
#' @param min_tract_pop Minimum total population in a tract to include
#'   it. Tracts below this are dropped. Default: 1.
#' @param time_matrix Pre-computed travel time matrix \[n x m\], or NULL.
#'   If provided, skips routing (no r5r, no OSM download).
#' @param optim An [optim_control()] object with optimization parameters.
#'   Default: `optim_control()`.
#' @param routing A [routing_control()] object with routing parameters.
#'   Default: `routing_control()`. When `gtfs_zip_path` is set but
#'   `departure_datetime` is NULL, the departure is auto-set to election
#'   day at 10 AM.
#' @param offset Numeric. Travel time offset. Default: 1.
#' @param force Re-download all cached data. Default: FALSE.
#' @param weights Numeric matrix or NULL. Pre-computed weight matrix.
#'   When provided, optimization is skipped.
#' @param keep Character vector of extra objects to include in the
#'   result. `weights`, `time_matrix`, `electoral_sf`, and `rep_points`
#'   are always kept. Options: `"pop_raster"`.
#' @param verbose Print progress messages. Default: TRUE.
#' @param ... Advanced arguments. `network_path` (character),
#'   `cache` (logical, default TRUE), `osm_provider` (character),
#'   `comparecimento_path`, `votacao_path`, `geocode_path`,
#'   `interp_sources`.
#'
#' @return An `interpElections_result` list. Key fields:
#'   `$tracts_sf` (sf with interpolated columns),
#'   `$interpolated` (matrix \[n x p\]),
#'   `$alpha` (decay parameters),
#'   `$optimization` (optimizer details).
#'
#' @section Quick start:
#' \preformatted{
#' result <- interpolate_election_br(3550308, 2022)
#' plot(result)
#' }
#'
#' @section Re-interpolation:
#' Use [reinterpolate()] to re-run with different candidates/parties
#' without recomputing travel times:
#' \preformatted{
#' result <- interpolate_election_br(3550308, 2022)
#' result2 <- reinterpolate(result, what = "parties")
#' }
#'
#' @section Advanced tuning:
#' Use [optim_control()] to adjust optimization parameters (epochs,
#' learning rate, GPU, etc.) and [routing_control()] for travel-time
#' computation (mode, GTFS, representative points, etc.).
#'
#' @family wrappers
#'
#' @seealso [optim_control()], [routing_control()],
#'   [interpolate_election()] for the general-purpose wrapper,
#'   [reinterpolate()] for quick re-runs.
#'
#' @export
interpolate_election_br <- function(
    municipality,
    year,
    # -- What to interpolate --
    cargo              = NULL,
    turno              = 1L,
    what               = "candidates",
    candidates         = NULL,
    parties            = NULL,
    # -- Geographic / census --
    uf                 = NULL,
    census_year        = NULL,
    clip_tracts_sf     = NULL,
    min_tract_pop      = 1,
    # -- Pre-computed input --
    time_matrix        = NULL,
    weights            = NULL,
    # -- Control --
    optim              = optim_control(),
    routing            = routing_control(),
    offset             = 1,
    force              = FALSE,
    keep               = NULL,
    verbose            = TRUE,
    ...
) {
  cl <- match.call()
  code_muni <- as.character(municipality)
  dots <- list(...)

  # Extract from control objects
  gtfs_zip_path <- routing$gtfs_zip_path

  # Extract advanced args from dots
  network_path        <- dots$network_path
  comparecimento_path <- dots$comparecimento_path
  votacao_path        <- dots$votacao_path
  interp_sources      <- dots$interp_sources
  osm_provider        <- dots$osm_provider %||% "openstreetmap_fr"
  cache               <- dots$cache %||% TRUE
  geocode_path        <- dots$geocode_path

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
    clip_sf = clip_tracts_sf,
    year = census_year
  )

  # --- Step 5: Electoral data ---
  if (verbose) message(sprintf("[4/%d] Preparing electoral data...",
                               .total_steps))
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
  # Construct state-level OSM URL to avoid oe_match() selecting
  # overly broad extracts (country/continent level)
  osm_url <- if (osm_provider == "openstreetmap_fr") {
    .br_osm_url(muni_info$uf)
  } else {
    NULL
  }

  # Auto-derive departure_datetime for GTFS transit routing
  if (!is.null(gtfs_zip_path) && is.null(routing$departure_datetime)) {
    election_date <- br_election_dates$date[
      br_election_dates$year == year & br_election_dates$turno == turno
    ]
    if (length(election_date) == 0) {
      # Fallback: first Sunday of October
      oct1 <- as.Date(sprintf("%d-10-01", year))
      wday <- as.integer(format(oct1, "%u"))  # 1=Mon .. 7=Sun
      election_date <- oct1 + ((7L - wday) %% 7L)
    }
    routing$departure_datetime <- as.POSIXct(
      paste(as.character(election_date[1]), "10:00:00"),
      tz = "America/Sao_Paulo"
    )
    if (verbose) {
      message(sprintf("  GTFS departure: %s",
                      format(routing$departure_datetime)))
    }
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
    weights = weights,
    optim = optim,
    routing = routing,
    min_tract_pop = 1,
    offset = offset,
    keep = keep,
    verbose = verbose,
    network_path = network_path,
    osm_provider = osm_provider,
    osm_url = osm_url,
    .progress = list(offset = .outer_steps, total = .total_steps)
  )

  if (verbose) message("\nDone.")

  # --- Set Brazilian metadata on the unified result ---
  ie_result$code_muni      <- code_muni
  ie_result$nome_municipio <- muni_info$nome_municipio
  ie_result$code_muni_tse  <- muni_info$code_muni_tse
  ie_result$uf             <- muni_info$uf
  ie_result$turno          <- as.integer(turno)
  ie_result$cargo          <- if (!is.null(cargo)) .br_resolve_cargo(cargo) else NULL
  ie_result$year           <- as.integer(year)
  ie_result$census_year    <- as.integer(census_year)
  ie_result$what        <- what
  ie_result$pop_data    <- pop_data
  ie_result$call        <- cl  # Override with br call

  # --- Build column dictionary ---
  ie_result$dictionary <- .br_build_dictionary(
    electoral_data, ie_result$interp_cols, calib, what)

  # --- Download geobr spatial data (municipality boundary + neighborhoods) ---
  # Always fetched (with timeout + caching) so plot() never needs network access
  if (verbose) message("Downloading municipality boundary and neighborhoods...")
  ie_result$muni_boundary   <- .br_download_muni_boundary(code_muni)
  ie_result$neighborhoods   <- .br_download_neighborhoods(code_muni)

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
# Always uses full calibration: gender x 7 age pairs (14 brackets).

.br_match_calibration <- function(census_year, tracts_sf, electoral_sf) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required", call. = FALSE)
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

# Download municipality boundary polygon via geobr (with timeout).
# Returns an sf object or NULL on failure/timeout.
.br_download_muni_boundary <- function(code_muni) {
  if (!requireNamespace("geobr", quietly = TRUE)) return(NULL)
  tryCatch({
    setTimeLimit(elapsed = 30, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    suppressMessages(
      geobr::read_municipality(code_muni = as.numeric(code_muni), year = 2022)
    )
  }, error = function(e) {
    message("Could not download municipality boundary: ", conditionMessage(e))
    NULL
  })
}

# Download IBGE 2010 neighborhood boundaries for a municipality (with timeout).
# Returns an sf object filtered to the given code_muni, or NULL on failure.
.br_download_neighborhoods <- function(code_muni) {
  if (!requireNamespace("geobr", quietly = TRUE)) {
    message("geobr not installed; cannot download neighborhoods.")
    return(NULL)
  }
  tryCatch({
    setTimeLimit(elapsed = 30, transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
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

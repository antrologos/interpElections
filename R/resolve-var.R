# Variable resolution and quantity computation helpers
# Used by plot() and plot_interactive() to resolve user-friendly variable
# identifiers (candidate name, ballot number, party abbreviation) to column
# names, and to compute derived quantities (percentages, density).

# --- Variable resolution ---

#' Resolve a user-friendly variable identifier to a column name
#'
#' @param variable Character or numeric. One of:
#'   - A column name (e.g., "CAND_13", "PARTY_PT")
#'   - A numeric ballot number (e.g., 13)
#'   - A party abbreviation (e.g., "PT")
#'   - A candidate name substring (e.g., "Lula")
#' @param result An `interpElections_result` object.
#' @param allow_multiple Logical. If TRUE, return all matches.
#'   Default FALSE (error on ambiguity).
#'
#' @return Character vector of matched column name(s).
#' @noRd
.resolve_var <- function(variable, result, allow_multiple = FALSE) {
  interp_cols <- result$interp_cols
  dict <- result$dictionary

  # 1. Exact column name match
  if (is.character(variable) && variable %in% interp_cols) {
    return(variable)
  }

  # 2. Ballot number (numeric or digit string)
  is_number <- is.numeric(variable) ||
    (is.character(variable) && grepl("^[0-9]+$", variable))
  if (is_number) {
    bn <- as.character(as.integer(variable))
    # Dictionary lookup
    if (!is.null(dict)) {
      cands <- dict[dict$type == "candidate" &
                      !is.na(dict$ballot_number) &
                      dict$ballot_number == bn, , drop = FALSE]
      if (nrow(cands) == 1L) return(cands$column[1L])
      if (nrow(cands) > 1L) {
        if (allow_multiple) return(cands$column)
        stop(
          sprintf(
            "Ballot number '%s' matches multiple columns (multi-cargo):\n  %s\n%s",
            bn, paste(cands$column, collapse = ", "),
            "Use the exact column name or allow_multiple = TRUE."
          ),
          call. = FALSE
        )
      }
    }
    # Fallback: pattern match against interp_cols
    pattern <- paste0("(^|_)CAND_", bn, "$")
    matches <- grep(pattern, interp_cols, value = TRUE)
    if (length(matches) == 1L) return(matches)
    if (length(matches) > 1L) {
      if (allow_multiple) return(matches)
      stop(
        sprintf(
          "Ballot number '%s' matches multiple columns:\n  %s\n%s",
          bn, paste(matches, collapse = ", "),
          "Use the exact column name."
        ),
        call. = FALSE
      )
    }
    stop(
      sprintf("No column found for ballot number '%s'.", bn),
      call. = FALSE
    )
  }

  # Steps 3-4 require character input
  if (!is.character(variable)) {
    stop(
      sprintf("Cannot resolve variable of type '%s'. %s",
              typeof(variable),
              "Provide a column name, ballot number, party abbreviation, or candidate name."),
      call. = FALSE
    )
  }

  # 3. Party abbreviation (dictionary lookup)
  if (!is.null(dict)) {
    parties <- dict[dict$type == "party" &
                      !is.na(dict$party) &
                      toupper(dict$party) == toupper(variable), , drop = FALSE]
    if (nrow(parties) == 1L) return(parties$column[1L])
    if (nrow(parties) > 1L) {
      if (allow_multiple) return(parties$column)
      stop(
        sprintf(
          "Party '%s' matches multiple columns:\n  %s",
          variable, paste(parties$column, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  # Party fallback without dictionary
  party_col <- paste0("PARTY_", toupper(variable))
  if (party_col %in% interp_cols) return(party_col)

  # 4. Candidate name substring (accent-normalized, case-insensitive)
  if (!is.null(dict)) {
    cands <- dict[dict$type == "candidate" & !is.na(dict$candidate_name), ,
                  drop = FALSE]
    if (nrow(cands) > 0) {
      search_norm <- toupper(.br_remove_accents(variable))
      name_norm <- toupper(.br_remove_accents(cands$candidate_name))
      matches_idx <- grep(search_norm, name_norm, fixed = TRUE)
      if (length(matches_idx) == 1L) return(cands$column[matches_idx])
      if (length(matches_idx) > 1L) {
        matched <- cands[matches_idx, , drop = FALSE]
        if (allow_multiple) return(matched$column)
        labels <- vapply(seq_len(nrow(matched)), function(i) {
          paste0(matched$column[i], " (", matched$candidate_name[i], ")")
        }, character(1))
        stop(
          sprintf(
            "'%s' matches multiple candidates:\n  %s\n%s",
            variable, paste(labels, collapse = "\n  "),
            "Use a more specific name or the exact column name."
          ),
          call. = FALSE
        )
      }
    }
  }

  # Nothing matched
  avail <- interp_cols
  if (length(avail) > 8) {
    avail_str <- paste(c(avail[1:8], sprintf("... and %d more", length(avail) - 8)),
                       collapse = ", ")
  } else {
    avail_str <- paste(avail, collapse = ", ")
  }
  stop(
    sprintf(
      "Could not resolve variable '%s'.\nAvailable columns: %s",
      variable, avail_str
    ),
    call. = FALSE
  )
}


#' Batch-resolve multiple variable identifiers
#' @noRd
.resolve_vars <- function(vars, result) {
  unname(vapply(vars, function(v) .resolve_var(v, result, allow_multiple = FALSE),
                character(1)))
}


# --- Quantity computation ---

#' Compute derived quantities from interpolated values
#'
#' @param result An `interpElections_result` object.
#' @param col Character. Column name (already resolved).
#' @param quantity Character. Quantity type.
#'
#' @return Numeric vector of length n (one per tract).
#' @noRd
.compute_quantity <- function(result, col, quantity = "absolute") {
  quantity <- match.arg(quantity, c("absolute", "pct_tract", "pct_muni",
                             "pct_valid", "pct_eligible", "density"))

  vals <- .get_col_values(result, col)

  switch(quantity,
    absolute = vals,
    pct_tract = {
      denom <- .get_col_values(result, "QT_COMPARECIMENTO",
                               error_msg = paste(
                                 "Cannot compute pct_tract: QT_COMPARECIMENTO not found.",
                                 "Include turnout data (what = c('votes', 'turnout'))."
                               ))
      denom[denom == 0] <- NA_real_
      vals / denom * 100
    },
    pct_muni = {
      total <- sum(vals, na.rm = TRUE)
      if (total == 0) {
        rep(NA_real_, length(vals))
      } else {
        vals / total * 100
      }
    },
    pct_valid = {
      turnout <- .get_col_values(result, "QT_COMPARECIMENTO",
                                  error_msg = paste(
                                    "Cannot compute pct_valid: QT_COMPARECIMENTO not found.",
                                    "Include turnout data."
                                  ))
      # Subtract blank (95) and null (96) votes
      blank <- .try_col_values(result, "CAND_95")
      null_v <- .try_col_values(result, "CAND_96")
      denom <- turnout - blank - null_v
      denom[denom <= 0] <- NA_real_
      vals / denom * 100
    },
    pct_eligible = {
      denom <- .get_col_values(result, "QT_APTOS",
                                error_msg = paste(
                                  "Cannot compute pct_eligible: QT_APTOS not found.",
                                  "Include turnout data (what = c('votes', 'turnout'))."
                                ))
      denom[denom == 0] <- NA_real_
      vals / denom * 100
    },
    density = {
      if (!requireNamespace("sf", quietly = TRUE)) {
        stop("The 'sf' package is required for quantity = 'density'.", call. = FALSE)
      }
      if (is.null(result$tracts_sf)) {
        stop("Cannot compute density: tracts_sf not available.", call. = FALSE)
      }
      area_m2 <- as.numeric(sf::st_area(result$tracts_sf))
      area_km2 <- area_m2 / 1e6
      area_km2[area_km2 == 0] <- NA_real_
      vals / area_km2
    }
  )
}


#' Get column values from tracts_sf or interpolated matrix
#' @noRd
.get_col_values <- function(result, col, error_msg = NULL) {
  # Try tracts_sf first (has all joined columns)
  if (!is.null(result$tracts_sf) && col %in% names(result$tracts_sf)) {
    return(as.numeric(result$tracts_sf[[col]]))
  }
  # Try interpolated matrix
  if (col %in% colnames(result$interpolated)) {
    return(as.numeric(result$interpolated[, col]))
  }
  # Not found
  if (is.null(error_msg)) {
    error_msg <- sprintf("Column '%s' not found in result.", col)
  }
  stop(error_msg, call. = FALSE)
}


#' Try to get column values, returning 0 vector if not found
#' @noRd
.try_col_values <- function(result, col) {
  if (!is.null(result$tracts_sf) && col %in% names(result$tracts_sf)) {
    return(as.numeric(result$tracts_sf[[col]]))
  }
  if (col %in% colnames(result$interpolated)) {
    return(as.numeric(result$interpolated[, col]))
  }
  # Also check columns matching *_CAND_95 etc. pattern
  pattern <- paste0("(^|_)", col, "$")
  sf_match <- grep(pattern, names(result$tracts_sf), value = TRUE)
  if (length(sf_match) == 1L) {
    return(as.numeric(result$tracts_sf[[sf_match]]))
  }
  mat_match <- grep(pattern, colnames(result$interpolated), value = TRUE)
  if (length(mat_match) == 1L) {
    return(as.numeric(result$interpolated[, mat_match]))
  }
  rep(0, nrow(result$interpolated))
}


# --- Title case helper for Portuguese names ---

#' Convert all-caps name to title case with Portuguese particles
#' @noRd
.title_case_pt <- function(x) {
  particles <- c("de", "da", "do", "das", "dos", "e")
  words <- strsplit(tolower(x), "\\s+")[[1]]
  words <- vapply(words, function(w) {
    if (w %in% particles) w
    else paste0(toupper(substring(w, 1, 1)), substring(w, 2))
  }, character(1), USE.NAMES = FALSE)
  paste(words, collapse = " ")
}


# --- Auto-select variable ---

#' Pick a sensible default variable for plotting
#' @noRd
.auto_select_var <- function(result) {
  dict <- result$dictionary
  if (!is.null(dict) && nrow(dict) > 0) {
    # Prefer first real candidate (not blank 95 / null 96)
    cands <- dict[dict$type == "candidate", , drop = FALSE]
    real <- cands[!cands$ballot_number %in% c("95", "96"), , drop = FALSE]
    if (nrow(real) > 0) return(real$column[1L])
    # Fall back to first party
    parties <- dict[dict$type == "party", , drop = FALSE]
    if (nrow(parties) > 0) return(parties$column[1L])
  }
  # No dictionary or no candidates/parties: first interp column
  result$interp_cols[1L]
}

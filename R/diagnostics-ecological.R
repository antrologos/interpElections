# Ecological validation diagnostics: income, turnout, cross-election

#' Plot income-vote ecological correlation
#'
#' Downloads tract-level household head income via `censobr` and
#' produces a scatter plot or side-by-side map of income vs. vote share.
#'
#' @param result An `interpElections_result` object (Brazilian).
#' @param variable Variable to correlate with income. Accepts column name,
#'   ballot number, candidate name, or party abbreviation.
#' @param census_year Census year for income data. NULL auto-detects from
#'   `result$census_year`. Must be 2000, 2010, or 2022.
#' @param type Character. `"scatter"` (default) or `"map"`.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly). Prints the plot.
#'
#' @details
#' Income data is downloaded from IBGE via the `censobr` package.
#' No deflation is applied — values are used as-is for within-year
#' correlation analysis.
#'
#' Variable sources by census year:
#' - **2000**: `censobr::read_tracts(2000, "Basico")`, variable `VAR06`
#' - **2010**: `censobr::read_tracts(2010, "ResponsavelRenda")`,
#'   computed as `V022 / V020`
#' - **2022**: `censobr::read_tracts(2022, "ResponsavelRenda")`,
#'   variable `V06004`
#'
#' @export
plot_income <- function(result, variable = NULL, census_year = NULL,
                         type = c("scatter", "map"), ...) {
  type <- match.arg(type)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("The 'ggplot2' package is required.", call. = FALSE)
  if (!requireNamespace("sf", quietly = TRUE))
    stop("The 'sf' package is required.", call. = FALSE)
  if (!requireNamespace("censobr", quietly = TRUE))
    stop("The 'censobr' package is required.\n",
         "Install with: install.packages(\"censobr\")", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("The 'dplyr' package is required.", call. = FALSE)

  if (is.null(result$code_muni)) {
    message("plot_income() requires a Brazilian result with code_muni.")
    return(invisible(NULL))
  }

  # Resolve census year
  if (is.null(census_year)) census_year <- result$census_year
  if (is.null(census_year) || !census_year %in% c(2000, 2010, 2022)) {
    message("Census year must be 2000, 2010, or 2022.")
    return(invisible(NULL))
  }

  # Resolve variable
  if (is.null(variable)) {
    variable <- colnames(result$interpolated)[1L]
    message(sprintf("No variable specified. Using '%s'.", variable))
  }
  col <- tryCatch(.resolve_var(variable, result), error = function(e) {
    message(conditionMessage(e))
    NULL
  })
  if (is.null(col)) return(invisible(NULL))

  # Download income data
  income_df <- .download_income(result$code_muni, census_year)
  if (is.null(income_df)) return(invisible(NULL))

  # Join income with tracts
  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  tract_id_col <- result$tract_id

  # Compute vote share (% of tract turnout, consistent with plot_interactive)
  vote_share <- tryCatch(
    .compute_quantity(result, col, "pct_tract"),
    error = function(e) {
      # Fallback if QT_COMPARECIMENTO not available
      vote_vals <- result$interpolated[, col]
      total_votes <- rowSums(result$interpolated)
      total_votes[total_votes == 0] <- NA_real_
      vote_vals / total_votes * 100
    }
  )

  tracts_df$.vote_share <- vote_share
  tracts_df$.tract_code <- as.character(tracts_df[[tract_id_col]])

  merged <- merge(tracts_df, income_df, by.x = ".tract_code",
                  by.y = "code_tract", all.x = TRUE)
  merged <- merged[!is.na(merged$renda) & merged$renda > 0 &
                     !is.na(merged$.vote_share), ]
  merged$ln_renda <- log(merged$renda)

  if (nrow(merged) < 3L) {
    message("Too few tracts with both income and vote data.")
    return(invisible(NULL))
  }

  title <- tryCatch(.auto_title(col, result), error = function(e) col)

  if (type == "scatter") {
    p <- .income_scatter(merged, title, result)
  } else {
    p <- .income_map(merged, col, title, result)
  }

  if (!is.null(p)) print(p)
  invisible(p)
}


#' @noRd
.income_scatter <- function(merged, title, result) {
  r_pearson <- stats::cor(merged$ln_renda, merged$.vote_share,
                           use = "complete.obs")
  r_spearman <- stats::cor(merged$ln_renda, merged$.vote_share,
                            use = "complete.obs", method = "spearman")

  subtitle_parts <- character(0)
  if (!is.null(result$nome_municipio))
    subtitle_parts <- c(subtitle_parts,
                         sprintf("%s (%s)", result$nome_municipio, result$uf))
  if (!is.null(result$year))
    subtitle_parts <- c(subtitle_parts, as.character(result$year))

  p <- ggplot2::ggplot(merged,
                        ggplot2::aes(x = .data$ln_renda,
                                     y = .data$.vote_share)) +
    ggplot2::geom_point(alpha = 0.4, size = 1.5, color = "steelblue") +
    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "red",
                          linewidth = 0.8) +
    ggplot2::annotate("text", x = Inf, y = Inf,
                       label = sprintf("r = %.3f (Pearson)\nrho = %.3f (Spearman)",
                                       r_pearson, r_spearman),
                       hjust = 1.1, vjust = 1.3, size = 3.5,
                       fontface = "bold") +
    ggplot2::labs(
      title = sprintf("Income vs. %s", title),
      subtitle = paste(subtitle_parts, collapse = " \u2014 "),
      x = "log(Mean household head income)",
      y = "Vote share"
    ) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    ggplot2::theme_minimal()
  p
}


#' @noRd
.income_map <- function(merged, col, title, result) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    message("The 'patchwork' package is required for side-by-side maps.\n",
            "Install with: install.packages(\"patchwork\")")
    return(NULL)
  }

  # Join income back to tracts_sf for mapping
  tracts_sf <- result$tracts_sf
  tracts_df <- sf::st_drop_geometry(tracts_sf)
  tracts_df$.tract_code <- as.character(tracts_df[[result$tract_id]])

  income_joined <- merge(
    data.frame(.tract_code = tracts_df$.tract_code,
               .row_idx = seq_len(nrow(tracts_df))),
    merged[, c(".tract_code", "ln_renda", ".vote_share")],
    by = ".tract_code", all.x = TRUE
  )
  income_joined <- income_joined[order(income_joined$.row_idx), ]

  tracts_sf$.ln_renda <- income_joined$ln_renda
  tracts_sf$.vote_share <- income_joined$.vote_share

  p1 <- ggplot2::ggplot(tracts_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$.ln_renda),
                      color = "white", linewidth = 0.05) +
    ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    ggplot2::labs(title = "log(Income)", fill = "ln(R$)") +
    .map_theme()

  p2 <- ggplot2::ggplot(tracts_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$.vote_share),
                      color = "white", linewidth = 0.05) +
    ggplot2::scale_fill_viridis_c(option = "mako", na.value = "grey90",
                                   labels = function(x) paste0(round(x), "%")) +
    ggplot2::labs(title = title, fill = "Vote %") +
    .map_theme()

  p1 + p2
}


#' Download income data from censobr
#' @noRd
.download_income <- function(code_muni, census_year) {
  code_muni_chr <- as.character(code_muni)
  tryCatch({
    if (census_year == 2000) {
      raw <- censobr::read_tracts(2000, "Basico", showProgress = FALSE)
      df <- raw |>
        dplyr::filter(as.character(.data$code_muni) == code_muni_chr) |>
        dplyr::select("code_tract", renda = "VAR06") |>
        dplyr::collect()
    } else if (census_year == 2010) {
      raw <- censobr::read_tracts(2010, "ResponsavelRenda",
                                   showProgress = FALSE)
      df <- raw |>
        dplyr::filter(as.character(.data$code_muni) == code_muni_chr) |>
        dplyr::select("code_tract", pop = "V020", tot = "V022") |>
        dplyr::collect() |>
        dplyr::mutate(renda = .data$tot / .data$pop)
    } else {
      raw <- censobr::read_tracts(2022, "ResponsavelRenda",
                                   showProgress = FALSE)
      df <- raw |>
        dplyr::filter(as.character(.data$code_muni) == code_muni_chr) |>
        dplyr::select("code_tract", renda = "V06004") |>
        dplyr::collect()
    }

    df <- df |>
      dplyr::mutate(
        code_tract = as.character(.data$code_tract),
        renda = as.numeric(gsub(",", ".", as.character(.data$renda)))
      ) |>
      dplyr::filter(.data$renda > 0 & is.finite(.data$renda)) |>
      dplyr::select("code_tract", "renda")

    as.data.frame(df)
  }, error = function(e) {
    message("Failed to download income data: ", conditionMessage(e))
    NULL
  })
}


#' Plot implied turnout rates by demographic bracket
#'
#' Computes implied turnout rate per tract and bracket as the ratio of
#' interpolated voters to census population. Rates exceeding 100%
#' indicate boundary effects or registration mismatches.
#'
#' @param result An `interpElections_result` object.
#' @param type Character. `"bracket"` (boxplot), `"map"`, or
#'   `"histogram"`. Default: `"bracket"`.
#' @param summary_fn Summary for map mode. Default: `"mean"`.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly).
#'
#' @export
plot_turnout_rates <- function(result, type = c("bracket", "map", "histogram"),
                                summary_fn = "mean", ...) {
  type <- match.arg(type)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("The 'ggplot2' package is required.", call. = FALSE)
  if (!requireNamespace("sf", quietly = TRUE))
    stop("The 'sf' package is required.", call. = FALSE)

  # Need calibration columns and weights
  if (is.null(result$calib_cols)) {
    message("Calibration columns required for turnout rates.")
    return(invisible(NULL))
  }

  W <- .get_weights(result)
  if (is.null(W)) {
    message("Weight matrix required for turnout rates.")
    return(invisible(NULL))
  }

  # Compute fitted voters per bracket
  src_mat <- as.matrix(
    result$sources[, result$calib_cols$sources, drop = FALSE]
  )
  storage.mode(src_mat) <- "double"
  fitted <- W %*% src_mat

  # Census population per bracket
  tracts_df <- sf::st_drop_geometry(result$tracts_sf)
  pop_mat <- as.matrix(tracts_df[, result$calib_cols$tracts, drop = FALSE])
  storage.mode(pop_mat) <- "double"

  # Turnout rate = fitted / census_pop
  rate_mat <- fitted / pmax(pop_mat, 1)  # avoid division by zero
  # Set to NA where census pop is zero
  rate_mat[pop_mat == 0] <- NA_real_

  bracket_names <- .bracket_labels(result)
  if (is.null(bracket_names))
    bracket_names <- result$calib_cols$tracts
  k <- ncol(rate_mat)

  if (type == "bracket") {
    p <- .turnout_bracket(rate_mat, bracket_names)
  } else if (type == "map") {
    p <- .turnout_map(rate_mat, summary_fn, result)
  } else {
    p <- .turnout_histogram(rate_mat, bracket_names)
  }

  if (!is.null(p)) print(p)
  invisible(p)
}


#' @noRd
.turnout_bracket <- function(rate_mat, bracket_names) {
  k <- ncol(rate_mat)
  n <- nrow(rate_mat)

  df <- data.frame(
    bracket = rep(factor(bracket_names, levels = bracket_names), each = n),
    rate = as.numeric(rate_mat)
  )
  df <- df[!is.na(df$rate), ]

  n_over_100 <- sum(df$rate > 1, na.rm = TRUE)
  pct_over_100 <- round(n_over_100 / nrow(df) * 100, 1)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$bracket,
                                          y = .data$rate)) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.6,
                           outlier.size = 0.8) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                         color = "red", linewidth = 0.5) +
    ggplot2::labs(
      title = "Implied turnout rates by bracket",
      subtitle = sprintf("Red line = 100%%. %.1f%% of tract-brackets > 100%%",
                         pct_over_100),
      x = NULL,
      y = "Turnout rate"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x * 100), "%")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  p
}


#' @noRd
.turnout_map <- function(rate_mat, summary_fn, result) {
  # Collapse across brackets
  if (ncol(rate_mat) == 1L) {
    rate_vec <- rate_mat[, 1]
  } else {
    rate_vec <- switch(summary_fn,
      mean = rowMeans(rate_mat, na.rm = TRUE),
      median = apply(rate_mat, 1, stats::median, na.rm = TRUE),
      max = apply(rate_mat, 1, max, na.rm = TRUE),
      rowMeans(rate_mat, na.rm = TRUE)
    )
  }

  plot_sf <- result$tracts_sf
  plot_sf$.turnout_rate <- rate_vec

  n_over <- sum(rate_vec > 1, na.rm = TRUE)

  p <- ggplot2::ggplot(plot_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$.turnout_rate),
      color = "white", linewidth = 0.05
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "RdYlBu",
      na.value = "grey90",
      labels = function(x) paste0(round(x * 100), "%")
    ) +
    ggplot2::labs(
      title = "Implied mean turnout rate",
      subtitle = sprintf("%d tracts with rate > 100%%", n_over),
      fill = "Rate"
    ) +
    .map_theme()

  border_layer <- .muni_border_layer(result$code_muni, result$tracts_sf,
                                      result$muni_boundary)
  if (!is.null(border_layer)) p <- p + border_layer
  p
}


#' @noRd
.turnout_histogram <- function(rate_mat, bracket_names) {
  k <- ncol(rate_mat)
  n <- nrow(rate_mat)

  df <- data.frame(
    bracket = rep(factor(bracket_names, levels = bracket_names), each = n),
    rate = as.numeric(rate_mat)
  )
  df <- df[!is.na(df$rate), ]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$rate)) +
    ggplot2::geom_histogram(bins = 40, fill = "steelblue", color = "white",
                             linewidth = 0.1) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::facet_wrap(~ .data$bracket, nrow = 2, scales = "free_y") +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(round(x * 100), "%")
    ) +
    ggplot2::labs(
      title = "Implied turnout rate distribution by bracket",
      x = "Turnout rate",
      y = "Count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7))
  p
}


#' Cross-election consistency check
#'
#' Compares tract-level results between two election years to check
#' whether spatial patterns are consistent.
#'
#' @param result An `interpElections_result` object.
#' @param result2 A second `interpElections_result` object (different year).
#' @param variable Variable name to compare. NULL auto-matches by party.
#' @param type Character. `"scatter"` or `"map"`. Default: `"scatter"`.
#' @param ... Ignored.
#'
#' @return A `ggplot` object (invisibly).
#'
#' @export
plot_ecological <- function(result, result2, variable = NULL,
                             type = c("scatter", "map"), ...) {
  type <- match.arg(type)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("The 'ggplot2' package is required.", call. = FALSE)
  if (!requireNamespace("sf", quietly = TRUE))
    stop("The 'sf' package is required.", call. = FALSE)

  if (!inherits(result2, "interpElections_result"))
    stop("result2 must be an interpElections_result object.", call. = FALSE)

  # Resolve variable in both results
  if (is.null(variable)) {
    # Try to find a matching party column
    common <- intersect(colnames(result$interpolated),
                        colnames(result2$interpolated))
    if (length(common) > 0) {
      variable <- common[1L]
      message(sprintf("Auto-selected variable: '%s'", variable))
    } else {
      message("No matching variables between the two results.")
      return(invisible(NULL))
    }
  }

  col1 <- tryCatch(.resolve_var(variable, result), error = function(e) NULL)
  col2 <- tryCatch(.resolve_var(variable, result2), error = function(e) NULL)
  if (is.null(col1) || is.null(col2)) {
    message(sprintf("Variable '%s' not found in both results.", variable))
    return(invisible(NULL))
  }

  # Get tract IDs and match
  id1 <- sf::st_drop_geometry(result$tracts_sf)[[result$tract_id]]
  id2 <- sf::st_drop_geometry(result2$tracts_sf)[[result2$tract_id]]
  common_ids <- intersect(as.character(id1), as.character(id2))

  if (length(common_ids) < 3L) {
    message("Too few matching tracts between results.")
    return(invisible(NULL))
  }

  # Compute vote shares
  idx1 <- match(common_ids, as.character(id1))
  idx2 <- match(common_ids, as.character(id2))

  # Compute vote shares (% of tract turnout, consistent with plot_interactive)
  share1_full <- tryCatch(
    .compute_quantity(result, col1, "pct_tract"),
    error = function(e) {
      v <- result$interpolated[, col1]
      tot <- rowSums(result$interpolated)
      tot[tot == 0] <- NA_real_
      v / tot * 100
    }
  )
  share2_full <- tryCatch(
    .compute_quantity(result2, col2, "pct_tract"),
    error = function(e) {
      v <- result2$interpolated[, col2]
      tot <- rowSums(result2$interpolated)
      tot[tot == 0] <- NA_real_
      v / tot * 100
    }
  )
  share1 <- share1_full[idx1]
  share2 <- share2_full[idx2]

  year1 <- result$year %||% "Year 1"
  year2 <- result2$year %||% "Year 2"
  title <- tryCatch(.auto_title(col1, result), error = function(e) col1)

  if (type == "scatter") {
    df <- data.frame(share1 = share1, share2 = share2)
    df <- df[complete.cases(df), ]

    r <- stats::cor(df$share1, df$share2, use = "complete.obs")

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$share1,
                                            y = .data$share2)) +
      ggplot2::geom_point(alpha = 0.4, size = 1.5, color = "steelblue") +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                            color = "grey60") +
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red",
                            linewidth = 0.8) +
      ggplot2::annotate("text", x = Inf, y = Inf,
                         label = sprintf("r = %.3f", r),
                         hjust = 1.1, vjust = 1.3, size = 4,
                         fontface = "bold") +
      ggplot2::labs(
        title = sprintf("Cross-election consistency: %s", title),
        x = sprintf("%s vote share (%s)", title, year1),
        y = sprintf("%s vote share (%s)", title, year2)
      ) +
      ggplot2::scale_x_continuous(labels = function(x) paste0(round(x), "%")) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
      ggplot2::theme_minimal()
    print(p)
    invisible(p)
  } else {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      message("The 'patchwork' package is required for side-by-side maps.")
      return(invisible(NULL))
    }

    sf1 <- result$tracts_sf[idx1, ]
    sf1$.share <- share1
    sf2 <- result2$tracts_sf[idx2, ]
    sf2$.share <- share2

    p1 <- ggplot2::ggplot(sf1) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$.share),
                        color = "white", linewidth = 0.05) +
      ggplot2::scale_fill_viridis_c(
        na.value = "grey90",
        labels = function(x) paste0(round(x), "%")
      ) +
      ggplot2::labs(title = sprintf("%s (%s)", title, year1), fill = "%") +
      .map_theme()

    p2 <- ggplot2::ggplot(sf2) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$.share),
                        color = "white", linewidth = 0.05) +
      ggplot2::scale_fill_viridis_c(
        na.value = "grey90",
        labels = function(x) paste0(round(x), "%")
      ) +
      ggplot2::labs(title = sprintf("%s (%s)", title, year2), fill = "%") +
      .map_theme()

    p <- p1 + p2
    print(p)
    invisible(p)
  }
}

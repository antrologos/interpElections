# Convergence diagnostic plots for interpElections optimization

#' Plot optimization convergence diagnostics
#'
#' Two-column diagnostic plot. The left column shows operational metrics
#' (total loss, gradient norm, learning rate); the right column decomposes
#' the total loss into its components (Poisson deviance, barrier penalty,
#' entropy penalty), each with its own y-axis so every term is clearly
#' visible regardless of scale.
#'
#' When component histories are unavailable (legacy result objects) or
#' the \pkg{patchwork} package is not installed, the function falls back
#' to a single-column layout.
#'
#' @param result An `interpElections_result` object.
#' @param which Character vector. Which panels to show: any combination of
#'   `"loss"`, `"gradient"`, `"lr"`. Default: all three. Selecting `"loss"`
#'   adds both the total loss panel (left column) and the individual
#'   component panels (right column).
#' @param log_y Logical. Use log scale for y-axis on loss and gradient
#'   panels? Default: TRUE. Learning rate is always on linear scale.
#' @param ... Ignored.
#'
#' @return A ggplot or patchwork object (invisibly). Prints the plot.
#'
#' @export
plot_convergence <- function(result, which = c("loss", "gradient", "lr"),
                              log_y = TRUE, ...) {
  if (!inherits(result, "interpElections_result")) {
    stop("'result' must be an interpElections_result object.", call. = FALSE)
  }

  if (is.null(result$optimization)) {
    message("No optimization data in result (alpha may have been user-supplied).")
    return(invisible(NULL))
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "The 'ggplot2' package is required for plot_convergence().\n",
      "Install with: install.packages(\"ggplot2\")",
      call. = FALSE
    )
  }

  which <- match.arg(which, c("loss", "gradient", "lr"), several.ok = TRUE)
  opt <- result$optimization

  # Best epoch: stored by optimizer, or fall back to which.min
  best_epoch <- opt$best_epoch
  if (is.null(best_epoch) && !is.null(opt$history) && length(opt$history) > 0) {
    best_epoch <- which.min(opt$history)
  }

  # --- Build left column: operational panels ---
  left_plots <- list()
  loss_panels <- NULL
  has_decomposition <- FALSE

  if ("loss" %in% which && !is.null(opt$history)) {
    loss_panels <- .build_loss_panels(opt)
    has_decomposition <- length(loss_panels) > 1L

    left_plots$total <- .make_convergence_panel(
      loss_panels$total, "Total loss",
      color = "#2c7fb8", best_epoch = best_epoch, log_y = log_y
    )
  }

  if ("gradient" %in% which && !is.null(opt$grad_history)) {
    df <- data.frame(epoch = seq_along(opt$grad_history),
                     value = opt$grad_history, stringsAsFactors = FALSE)
    left_plots$gradient <- .make_convergence_panel(
      df, "Gradient norm",
      color = "#2c7fb8", best_epoch = best_epoch, log_y = log_y
    )
  }

  if ("lr" %in% which && !is.null(opt$lr_history)) {
    df <- data.frame(epoch = seq_along(opt$lr_history),
                     value = opt$lr_history, stringsAsFactors = FALSE)
    left_plots$lr <- .make_convergence_panel(
      df, "Learning rate",
      color = "#2c7fb8", best_epoch = NULL, log_y = FALSE
    )
  }

  # --- Build right column: decomposition panels ---
  right_plots <- list()
  if (has_decomposition &&
      requireNamespace("patchwork", quietly = TRUE)) {
    component_spec <- list(
      deviance = list(title = "Deviance",        color = "#31a354"),
      barrier  = list(title = "Barrier penalty",  color = "#d95f0e"),
      entropy  = list(title = "Entropy penalty",  color = "#756bb1")
    )
    for (comp in names(component_spec)) {
      if (!is.null(loss_panels[[comp]])) {
        spec <- component_spec[[comp]]
        right_plots[[comp]] <- .make_convergence_panel(
          loss_panels[[comp]], spec$title,
          color = spec$color, best_epoch = best_epoch, log_y = log_y
        )
      }
    }
  }

  if (length(left_plots) == 0L && length(right_plots) == 0L) {
    message("No history data available for the requested panels.")
    return(invisible(NULL))
  }

  # --- Compose layout ---
  p <- .compose_convergence(left_plots, right_plots, opt)

  print(p)
  invisible(p)
}


# --- Internal helpers ---------------------------------------------------

#' Build a single convergence panel
#' @noRd
.make_convergence_panel <- function(df, panel_title, color, best_epoch,
                                     log_y = FALSE) {
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$epoch, y = .data$value)) +
    ggplot2::geom_line(color = color, linewidth = 0.6) +
    ggplot2::labs(x = "Epoch", y = NULL, title = panel_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 9),
      plot.margin = ggplot2::margin(2, 5, 2, 5)
    )

  if (!is.null(best_epoch)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = best_epoch, linetype = "dashed",
        color = "firebrick", linewidth = 0.4
      ) +
      ggplot2::annotate(
        "text", x = best_epoch, y = Inf,
        label = sprintf(" best (%d)", best_epoch),
        hjust = 0, vjust = 1.5, size = 2.5, color = "firebrick"
      )
  }

  if (log_y && all(df$value > 0)) {
    p <- p + ggplot2::scale_y_log10()
  }

  p
}


#' Hide x-axis elements (for non-bottom panels)
#' @noRd
.hide_x_axis <- function() {
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x  = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank()
  )
}


#' Build per-component loss data frames
#'
#' Returns a named list: `$total` always present; `$deviance`, `$barrier`,
#' `$entropy` present only when the corresponding history exists and is
#' non-trivial.
#' @noRd
.build_loss_panels <- function(opt) {
  epochs <- seq_along(opt$history)
  panels <- list()

  panels$total <- data.frame(
    epoch = epochs, value = opt$history, stringsAsFactors = FALSE
  )

  if (!is.null(opt$deviance_history) && any(opt$deviance_history != 0)) {
    panels$deviance <- data.frame(
      epoch = epochs, value = opt$deviance_history, stringsAsFactors = FALSE
    )
  }
  if (!is.null(opt$barrier_history) && any(opt$barrier_history != 0)) {
    panels$barrier <- data.frame(
      epoch = epochs, value = opt$barrier_history, stringsAsFactors = FALSE
    )
  }
  if (!is.null(opt$entropy_history) && any(opt$entropy_history != 0)) {
    panels$entropy <- data.frame(
      epoch = epochs, value = opt$entropy_history, stringsAsFactors = FALSE
    )
  }

  panels
}


#' Compose two-column layout with patchwork, or single-column fallback
#' @noRd
.compose_convergence <- function(left_plots, right_plots, opt) {
  has_right <- length(right_plots) > 0L
  use_patchwork <- has_right && requireNamespace("patchwork", quietly = TRUE)

  # --- Single-column fallback ---
  if (!use_patchwork) {
    all_plots <- unname(left_plots)
    if (length(all_plots) == 1L) {
      p <- all_plots[[1]] +
        ggplot2::labs(
          title = "Optimization convergence",
          subtitle = .convergence_subtitle(opt)
        )
      return(p)
    }

    # Multiple left-column panels: stack with patchwork if available,
    # otherwise use facet_wrap fallback
    if (requireNamespace("patchwork", quietly = TRUE)) {
      # Hide x-axis on non-bottom panels
      n <- length(all_plots)
      for (i in seq_len(n - 1L)) {
        all_plots[[i]] <- all_plots[[i]] + .hide_x_axis()
      }
      p <- patchwork::wrap_plots(all_plots, ncol = 1) +
        patchwork::plot_annotation(
          title = "Optimization convergence",
          subtitle = .convergence_subtitle(opt)
        )
      return(p)
    }

    # No patchwork at all: minimal facet_wrap fallback
    return(.fallback_facet(left_plots, opt))
  }

  # --- Two-column patchwork layout ---
  n_left  <- length(left_plots)
  n_right <- length(right_plots)
  n_rows  <- max(n_left, n_right)

  left_list  <- unname(left_plots)
  right_list <- unname(right_plots)

  # Hide x-axis on non-bottom panels in each column
  if (n_left > 1L) {
    for (i in seq_len(n_left - 1L)) {
      left_list[[i]] <- left_list[[i]] + .hide_x_axis()
    }
  }
  if (n_right > 1L) {
    for (i in seq_len(n_right - 1L)) {
      right_list[[i]] <- right_list[[i]] + .hide_x_axis()
    }
  }

  # Pad shorter column with spacers
  while (length(left_list) < n_rows) {
    left_list[[length(left_list) + 1L]] <- patchwork::plot_spacer()
  }
  while (length(right_list) < n_rows) {
    right_list[[length(right_list) + 1L]] <- patchwork::plot_spacer()
  }

  # Interleave: row1_left, row1_right, row2_left, row2_right, ...
  all_plots <- vector("list", 2L * n_rows)
  for (i in seq_len(n_rows)) {
    all_plots[[2L * (i - 1L) + 1L]] <- left_list[[i]]
    all_plots[[2L * (i - 1L) + 2L]] <- right_list[[i]]
  }

  patchwork::wrap_plots(all_plots, ncol = 2) +
    patchwork::plot_annotation(
      title = "Optimization convergence",
      subtitle = .convergence_subtitle(opt)
    )
}


#' Facet-wrap fallback when patchwork is unavailable
#' @noRd
.fallback_facet <- function(left_plots_named, opt) {
  # Reconstruct a single data.frame for facet_wrap
  panel_names <- c(total = "Total loss", gradient = "Gradient norm",
                   lr = "Learning rate")
  frames <- list()
  for (nm in names(left_plots_named)) {
    df <- left_plots_named[[nm]]$data
    df$panel <- panel_names[[nm]]
    frames[[nm]] <- df
  }
  plot_df <- do.call(rbind, frames)
  rownames(plot_df) <- NULL
  plot_df$panel <- factor(plot_df$panel,
                          levels = intersect(panel_names, plot_df$panel))

  ggplot2::ggplot(plot_df,
    ggplot2::aes(x = .data$epoch, y = .data$value)) +
    ggplot2::geom_line(color = "#2c7fb8", linewidth = 0.6) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 1) +
    ggplot2::labs(
      x = "Epoch", y = NULL,
      title = "Optimization convergence",
      subtitle = .convergence_subtitle(opt)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 9)
    )
}


#' Build subtitle for convergence plot
#' @noRd
.convergence_subtitle <- function(opt) {
  parts <- character(0)

  if (!is.null(opt$method)) {
    parts <- c(parts, opt$method)
  }

  conv_text <- if (identical(opt$convergence, 0L) || identical(opt$convergence, 0)) {
    "converged"
  } else {
    "did NOT converge"
  }
  parts <- c(parts, conv_text)

  if (!is.null(opt$epochs)) {
    parts <- c(parts, sprintf("%d epochs", opt$epochs))
  }

  if (!is.null(opt$value)) {
    parts <- c(parts, sprintf("final loss = %.4f", opt$value))
  }

  paste(parts, collapse = " | ")
}

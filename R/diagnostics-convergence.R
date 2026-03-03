# Convergence diagnostic plots for interpElections optimization

#' Plot optimization convergence diagnostics
#'
#' Multi-panel plot showing loss history, gradient norm, and learning rate
#' per epoch. The loss panel decomposes the total objective into its
#' components (Poisson deviance, barrier penalty, entropy penalty), so
#' you can see how each term evolves during training.
#'
#' @param result An `interpElections_result` object.
#' @param which Character vector. Which panels to show: any combination of
#'   `"loss"`, `"gradient"`, `"lr"`. Default: all three. The `"loss"` panel
#'   shows all non-zero loss components as separate colored lines.
#' @param log_y Logical. Use log scale for y-axis? Default: TRUE.
#' @param ... Ignored.
#'
#' @return A ggplot object (invisibly). Prints the plot.
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

  # Build data for each requested panel
  pieces <- list()

  if ("loss" %in% which && !is.null(opt$history)) {
    pieces$loss <- .build_loss_panel(opt)
  }

  if ("gradient" %in% which && !is.null(opt$grad_history)) {
    pieces$gradient <- data.frame(
      epoch = seq_along(opt$grad_history),
      value = opt$grad_history,
      component = "Gradient norm",
      panel = "Gradient norm",
      stringsAsFactors = FALSE
    )
  }

  if ("lr" %in% which && !is.null(opt$lr_history)) {
    pieces$lr <- data.frame(
      epoch = seq_along(opt$lr_history),
      value = opt$lr_history,
      component = "Learning rate",
      panel = "Learning rate",
      stringsAsFactors = FALSE
    )
  }

  if (length(pieces) == 0L) {
    message("No history data available for the requested panels.")
    return(invisible(NULL))
  }

  plot_df <- do.call(rbind, pieces)
  rownames(plot_df) <- NULL

  # Preserve panel order
  panel_order <- character(0)
  if ("loss" %in% names(pieces)) panel_order <- c(panel_order, "Loss")
  if ("gradient" %in% names(pieces)) panel_order <- c(panel_order, "Gradient norm")
  if ("lr" %in% names(pieces)) panel_order <- c(panel_order, "Learning rate")
  plot_df$panel <- factor(plot_df$panel, levels = panel_order)

  # Best epoch: use stored best_epoch if available, fall back to which.min
  best_epoch <- opt$best_epoch
  if (is.null(best_epoch) && !is.null(opt$history) && length(opt$history) > 0) {
    best_epoch <- which.min(opt$history)
  }

  # Determine if loss panel has multiple components
  has_multi_loss <- "loss" %in% names(pieces) &&
    length(unique(pieces$loss$component)) > 1L

  # Build plot
  if (has_multi_loss) {
    # Multi-line loss panel: use color aesthetic for loss components,
    # single-color for other panels
    p <- ggplot2::ggplot(plot_df,
      ggplot2::aes(x = .data$epoch, y = .data$value)) +
      ggplot2::geom_line(
        ggplot2::aes(color = .data$component, linetype = .data$component),
        linewidth = 0.6
      ) +
      ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 1) +
      ggplot2::scale_color_manual(values = .loss_component_colors()) +
      ggplot2::scale_linetype_manual(values = .loss_component_linetypes()) +
      ggplot2::guides(
        color = ggplot2::guide_legend(title = NULL),
        linetype = ggplot2::guide_legend(title = NULL)
      )
  } else {
    # Single-line panels (backward compat or gradient/lr only)
    p <- ggplot2::ggplot(plot_df,
      ggplot2::aes(x = .data$epoch, y = .data$value)) +
      ggplot2::geom_line(color = "#2c7fb8", linewidth = 0.6) +
      ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 1)
  }

  p <- p +
    ggplot2::labs(
      x = "Epoch",
      y = NULL,
      title = "Optimization convergence",
      subtitle = .convergence_subtitle(opt)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 9),
      legend.position = "bottom"
    )

  # Add vertical dashed line at best epoch
  if (!is.null(best_epoch)) {
    p <- p + ggplot2::geom_vline(
      xintercept = best_epoch,
      linetype = "dashed",
      color = "firebrick",
      linewidth = 0.4
    ) +
    ggplot2::annotate(
      "text",
      x = best_epoch,
      y = Inf,
      label = sprintf(" best (epoch %d)", best_epoch),
      hjust = 0, vjust = 1.5,
      size = 3, color = "firebrick"
    )
  }

  # Log scale for loss and gradient panels (not lr)
  if (log_y) {
    log_panels <- intersect(c("Loss", "Gradient norm"), panel_order)
    if (length(log_panels) > 0) {
      has_lr <- "Learning rate" %in% panel_order
      if (!has_lr) {
        positive <- plot_df$value > 0
        if (all(positive)) {
          p <- p + ggplot2::scale_y_log10()
        }
      } else {
        p <- p + ggplot2::labs(
          caption = "Tip: use which = c(\"loss\", \"gradient\") for log-scale"
        )
      }
    }
  }

  print(p)
  invisible(p)
}


#' Build loss panel data with component decomposition
#' @noRd
.build_loss_panel <- function(opt) {
  epochs <- seq_along(opt$history)

  # Always include total loss
  frames <- list(
    data.frame(
      epoch = epochs,
      value = opt$history,
      component = "Total",
      panel = "Loss",
      stringsAsFactors = FALSE
    )
  )

  # Add component traces if available and non-trivial
  if (!is.null(opt$deviance_history) && any(opt$deviance_history != 0)) {
    frames[[length(frames) + 1L]] <- data.frame(
      epoch = epochs,
      value = opt$deviance_history,
      component = "Deviance",
      panel = "Loss",
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(opt$barrier_history) && any(opt$barrier_history != 0)) {
    frames[[length(frames) + 1L]] <- data.frame(
      epoch = epochs,
      value = opt$barrier_history,
      component = "Barrier",
      panel = "Loss",
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(opt$entropy_history) && any(opt$entropy_history != 0)) {
    frames[[length(frames) + 1L]] <- data.frame(
      epoch = epochs,
      value = opt$entropy_history,
      component = "Entropy",
      panel = "Loss",
      stringsAsFactors = FALSE
    )
  }

  # If no component histories available (old result object), show total only
  # with the legacy label for single-line rendering
  if (length(frames) == 1L) {
    frames[[1]]$component <- "Loss"
  }

  do.call(rbind, frames)
}


#' Color palette for loss components
#' @noRd
.loss_component_colors <- function() {
  c(
    "Total"         = "#2c7fb8",
    "Deviance"      = "#31a354",
    "Barrier"       = "#d95f0e",
    "Entropy"       = "#756bb1",
    # Single-trace components (gradient, lr) keep the default blue
    "Loss"          = "#2c7fb8",
    "Gradient norm" = "#2c7fb8",
    "Learning rate" = "#2c7fb8"
  )
}


#' Linetype mapping for loss components
#' @noRd
.loss_component_linetypes <- function() {
  c(
    "Total"         = "solid",
    "Deviance"      = "solid",
    "Barrier"       = "dashed",
    "Entropy"       = "dotted",
    "Loss"          = "solid",
    "Gradient norm" = "solid",
    "Learning rate" = "solid"
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

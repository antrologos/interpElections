# Convergence diagnostic plots for interpElections optimization

#' Plot optimization convergence diagnostics
#'
#' Multi-panel plot showing loss history, gradient norm, and learning rate
#' per epoch.
#'
#' @param result An `interpElections_result` object.
#' @param which Character vector. Which panels to show: any combination of
#'   `"loss"`, `"gradient"`, `"lr"`. Default: all three.
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
    pieces$loss <- data.frame(
      epoch = seq_along(opt$history),
      value = opt$history,
      panel = "Loss (Poisson deviance)",
      stringsAsFactors = FALSE
    )
  }

  if ("gradient" %in% which && !is.null(opt$grad_history)) {
    pieces$gradient <- data.frame(
      epoch = seq_along(opt$grad_history),
      value = opt$grad_history,
      panel = "Gradient norm",
      stringsAsFactors = FALSE
    )
  }

  if ("lr" %in% which && !is.null(opt$lr_history)) {
    pieces$lr <- data.frame(
      epoch = seq_along(opt$lr_history),
      value = opt$lr_history,
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
  if ("loss" %in% names(pieces)) panel_order <- c(panel_order, "Loss (Poisson deviance)")
  if ("gradient" %in% names(pieces)) panel_order <- c(panel_order, "Gradient norm")
  if ("lr" %in% names(pieces)) panel_order <- c(panel_order, "Learning rate")
  plot_df$panel <- factor(plot_df$panel, levels = panel_order)

  # Best epoch (minimum loss)
  best_epoch <- NULL
  if (!is.null(opt$history) && length(opt$history) > 0) {
    best_epoch <- which.min(opt$history)
  }

  # Build plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$epoch, y = .data$value)) +
    ggplot2::geom_line(color = "#2c7fb8", linewidth = 0.6) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 1) +
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
      plot.subtitle = ggplot2::element_text(color = "grey40", size = 9)
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
    log_panels <- intersect(
      c("Loss (Poisson deviance)", "Gradient norm"),
      panel_order
    )
    if (length(log_panels) > 0) {
      # Apply log scale to all panels that can use it; lr stays linear
      # With free_y scales, we need to handle this carefully
      # ggplot2 facet_wrap with free scales applies the same transform
      # to all panels, so we filter out non-positive values instead
      has_lr <- "Learning rate" %in% panel_order
      if (!has_lr) {
        # Only loss and/or gradient panels: safe to use log scale
        positive <- plot_df$value > 0
        if (all(positive)) {
          p <- p + ggplot2::scale_y_log10()
        }
      } else {
        # Mix of log and linear panels: skip global log scale,
        # annotate subtitle instead
        p <- p + ggplot2::labs(
          caption = "Tip: use which = c(\"loss\", \"gradient\") for log-scale"
        )
      }
    }
  }

  print(p)
  invisible(p)
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

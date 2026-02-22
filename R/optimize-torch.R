# ============================================================
# Internal: Per-Tract-Per-Bracket SGD via torch (GPU or CPU)
# ============================================================
#
# Uses mini-batch log-domain 3D Sinkhorn inside torch autograd with
# per-tract-per-bracket decay kernels: K^b[i,j] = t[i,j]^(-alpha[i,b]).
# Softplus reparameterization: alpha = alpha_min + softplus(theta),
# theta unconstrained — no clamping, no gradient death at boundaries.
# Epoch structure: each epoch is one full shuffled pass through
# all tracts; the loss reported at each epoch is the true
# objective evaluated on the full dataset.

.optimize_torch <- function(time_matrix, pop_matrix, source_matrix,
                             alpha_init, batch_size, sk_iter, sk_tol,
                             max_epochs, lr_init, device, dtype,
                             convergence_tol, patience,
                             method, barrier_mu, loss_fn, alpha_min,
                             verbose) {

  # --- RStudio subprocess delegation ---
  if (.is_rstudio() && !identical(Sys.getenv("INTERPELECTIONS_SUBPROCESS"), "1")) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop(
        "Torch optimization inside RStudio requires the 'callr' package.\n",
        "Install with: install.packages('callr')",
        call. = FALSE
      )
    }
    if (verbose) {
      message("  Running torch optimization in subprocess...")
    }
    result <- callr::r(
      function(time_matrix, pop_matrix, source_matrix, alpha_init,
               batch_size, sk_iter, sk_tol, max_epochs, lr_init,
               device, dtype,
               convergence_tol, patience,
               method, barrier_mu, loss_fn, alpha_min,
               verbose, pkg_path) {
        Sys.setenv(INTERPELECTIONS_SUBPROCESS = "1")
        if (nzchar(pkg_path) && file.exists(file.path(pkg_path, "DESCRIPTION"))) {
          pkgload::load_all(pkg_path, quiet = TRUE)
        } else {
          library(interpElections)
        }
        interpElections:::.optimize_torch(
          time_matrix = time_matrix,
          pop_matrix = pop_matrix,
          source_matrix = source_matrix,
          alpha_init = alpha_init,
          batch_size = batch_size, sk_iter = sk_iter, sk_tol = sk_tol,
          max_epochs = max_epochs, lr_init = lr_init,
          device = device, dtype = dtype,
          convergence_tol = convergence_tol, patience = patience,
          method = method, barrier_mu = barrier_mu,
          loss_fn = loss_fn, alpha_min = alpha_min,
          verbose = verbose
        )
      },
      args = list(
        time_matrix = time_matrix,
        pop_matrix = pop_matrix,
        source_matrix = source_matrix,
        alpha_init = alpha_init,
        batch_size = batch_size, sk_iter = sk_iter, sk_tol = sk_tol,
        max_epochs = max_epochs, lr_init = lr_init,
        device = device, dtype = dtype,
        convergence_tol = convergence_tol, patience = patience,
        method = method, barrier_mu = barrier_mu,
        loss_fn = loss_fn, alpha_min = alpha_min,
        verbose = verbose,
        pkg_path = .find_package_root()
      ),
      show = verbose
    )
    return(result)
  }

  torch_dtype <- .resolve_dtype(dtype)
  n <- nrow(time_matrix)
  m <- ncol(time_matrix)
  k <- ncol(pop_matrix)
  b <- min(as.integer(batch_size), n)

  # --- Per-bracket preprocessing (R-side) ---
  P_cpu <- pop_matrix
  V_cpu <- source_matrix
  c_mat <- matrix(0, k, m)
  skip  <- logical(k)

  for (bi in seq_len(k)) {
    rb <- P_cpu[, bi]
    cb <- V_cpu[, bi]
    if (sum(cb) < 0.5 || sum(rb) < 0.5) {
      skip[bi] <- TRUE
      next
    }
    c_mat[bi, ] <- cb
  }

  active <- which(!skip)
  ka     <- length(active)

  if (ka == 0L) {
    stop("All demographic brackets are empty (sum < 0.5). Cannot optimize.",
         call. = FALSE)
  }

  c_mat <- c_mat[active, , drop = FALSE]

  # --- Epoch structure ---
  # Each epoch = one full shuffled pass through all n tracts.
  # max_epochs is the parameter directly (no conversion from steps).
  n_batches_per_epoch <- ceiling(n / b)

  # LR schedule: ReduceLROnPlateau — halve LR when epoch loss plateaus.
  min_lr <- lr_init * 1e-3

  # Verbose: print ~20 lines regardless of epoch count
  report_every <- max(1L, max_epochs %/% 20L)

  # --- Memory safety check (GPU only) ---
  bytes_per_elem <- if (dtype == "float32") 4 else 8
  # Dominant cost: log_K_3d (ka×b×m) + one log_KV copy per IPF step
  estimated_mb   <- (2.0 * as.double(ka) * b * m) * bytes_per_elem / 1e6

  if (grepl("cuda", device, fixed = TRUE)) {
    vram_mb <- tryCatch({
      hw <- .detect_gpu_nvidia()
      if (hw$found && !is.na(hw$vram_mb)) hw$vram_mb else NA_real_
    }, error = function(e) NA_real_)

    if (!is.na(vram_mb) && estimated_mb > vram_mb * 0.8) {
      stop(sprintf(paste0(
        "Estimated GPU memory (batch=%d, sk_iter=%d): %.0f MB ",
        "(%.0f MB VRAM available).\n",
        "Reduce batch_size, sk_iter, or use use_gpu = FALSE."
      ), b, sk_iter, estimated_mb, vram_mb), call. = FALSE)
    }

    if (verbose && !is.na(vram_mb)) {
      message(sprintf("  Estimated memory: %.0f MB / %.0f MB VRAM",
                      estimated_mb, vram_mb))
    }
  }

  if (verbose) {
    if (method == "colnorm") {
      message(sprintf(paste0(
        "  PB-SGD colnorm (%s, %s): batch=%d, barrier_mu=%.1f, ",
        "loss=%s, alpha_min=%.1f, max %d epochs (%d batches/epoch)"),
        device, dtype, b, barrier_mu, loss_fn, alpha_min,
        max_epochs, n_batches_per_epoch))
    } else {
      message(sprintf(paste0(
        "  PB-SGD sinkhorn (%s, %s): batch=%d, sk_iter=%d (tol=%.1e), ",
        "loss=%s, alpha_min=%.1f, max %d epochs (%d batches/epoch)"),
        device, dtype, b, sk_iter, sk_tol, loss_fn, alpha_min,
        max_epochs, n_batches_per_epoch))
    }
  }

  # Track tensors for cleanup
  gpu_tensors <- new.env(parent = emptyenv())
  on.exit({
    rm(list = ls(gpu_tensors), envir = gpu_tensors)
    gc(verbose = FALSE)
    if (requireNamespace("torch", quietly = TRUE)) {
      tryCatch(torch::cuda_empty_cache(), error = function(e) NULL)
    }
  }, add = TRUE)

  result <- tryCatch({
    # Transfer to tensors
    log_t_torch <- torch::torch_log(torch::torch_tensor(
      time_matrix, device = device, dtype = torch_dtype))

    # --- 3D IPF pre-computations ---
    # Uses original (unrescaled) population for Property 2 row targets.
    p_mat_active <- P_cpu[, active, drop = FALSE]   # n × ka
    r_pop_total  <- rowSums(p_mat_active)            # n-vector
    P_total_val  <- sum(r_pop_total)
    r_total_norm <- r_pop_total / pmax(P_total_val, 1e-30)

    V_total_val  <- sum(c_mat)

    # Row targets: r_norm[i] * V_total
    # Col targets: c_mat[b,j] = V^b_j
    # Both sum to V_total_val, so the 3D IPF is feasible.
    log_row_target_full <- torch::torch_tensor(
      log(pmax(r_total_norm, 1e-30)) +
        log(pmax(V_total_val, 1e-30)),
      device = device, dtype = torch_dtype)          # (n,)

    log_V_b_torch <- torch::torch_tensor(
      log(pmax(c_mat, 1e-30)),
      device = device, dtype = torch_dtype)          # (ka, m)

    # Active-bracket population tensor for loss computation
    p_active_torch <- torch::torch_tensor(
      p_mat_active, device = device, dtype = torch_dtype)  # (n, ka)

    # --- Softplus reparameterization — per-tract-per-bracket ---
    # alpha[i,b] = alpha_min + softplus(theta[i,b]), theta unconstrained.
    # One alpha per census tract per active bracket.
    alpha_init_mat <- matrix(
      pmax(alpha_init, alpha_min + 1e-6), nrow = n, ncol = ka)
    # Invert: theta = softplus_inverse(alpha - alpha_min)
    # softplus_inverse(x) = log(exp(x) - 1); for x > 20, ~ x
    sp_arg <- alpha_init_mat - alpha_min
    theta_init <- ifelse(
      sp_arg > 20,
      sp_arg,
      log(pmax(exp(sp_arg) - 1, 1e-30))
    )

    # Break initialization symmetry with small deterministic perturbation
    # so different tracts/brackets get different initial gradients.
    old_seed <- if (exists(".Random.seed", envir = globalenv()))
      get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = globalenv()))
          rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      }
    }, add = TRUE)
    set.seed(n * 7919L + ka * 104729L)
    theta_init <- theta_init + matrix(
      stats::rnorm(n * ka, mean = 0, sd = 0.1),
      nrow = n, ncol = ka
    )

    theta_torch <- torch::torch_tensor(
      theta_init,
      device        = device,
      requires_grad = TRUE,
      dtype         = torch_dtype
    )

    # alpha_fn: theta -> alpha (always >= alpha_min)
    alpha_min_t <- torch::torch_tensor(
      alpha_min, device = device, dtype = torch_dtype)
    alpha_fn <- function(th) alpha_min_t + torch::nnf_softplus(th)

    # Loss computation helper: Poisson deviance or SSE
    compute_data_loss <- function(V_hat, P, scale_factor) {
      if (loss_fn == "poisson") {
        V_clamped <- torch::torch_clamp(V_hat, min = 1e-30)
        P_safe <- torch::torch_where(
          P > 0, P, torch::torch_ones_like(P))
        log_ratio <- torch::torch_where(
          P > 0,
          torch::torch_log(P_safe) - torch::torch_log(V_clamped),
          torch::torch_zeros_like(P))
        dev <- 2 * (V_clamped - P + P * log_ratio)
        dev$sum() * scale_factor
      } else {
        (V_hat - P)$pow(2)$sum() * scale_factor
      }
    }

    gpu_tensors$log_t   <- log_t_torch
    gpu_tensors$p_act   <- p_active_torch
    gpu_tensors$theta   <- theta_torch
    gpu_tensors$log_V_b <- log_V_b_torch
    gpu_tensors$log_r   <- log_row_target_full

    optimizer <- torch::optim_adam(
      list(theta_torch), lr = lr_init, betas = c(0.9, 0.99))

    scheduler <- torch::lr_reduce_on_plateau(
      optimizer,
      mode       = "min",
      factor     = 0.5,
      patience   = as.integer(patience * 2L),
      threshold  = convergence_tol,
      threshold_mode = "rel",
      min_lr     = min_lr,
      verbose    = FALSE
    )

    best_epoch_loss  <- Inf
    best_alpha <- as.matrix(
      alpha_fn(theta_torch)$detach()$cpu())            # (n, ka)
    best_W     <- NULL                                   # n × m R matrix
    epoch_losses     <- numeric(max_epochs)
    lr_history       <- numeric(max_epochs)
    last_grad_norm   <- NA_real_
    step_counter     <- 0L
    epoch            <- 0L

    # Convergence tracking
    patience_counter <- 0L
    converged        <- FALSE

    grad_history <- numeric(max_epochs)
    sk_nonconv_count <- 0L

    for (epoch in seq_len(max_epochs)) {

      # Shuffle all n tracts into sequential mini-batches
      perm       <- sample.int(n)
      batch_list <- split(perm, ceiling(seq_along(perm) / b))

      # --- Mini-batch gradient steps ---
      for (idx in batch_list) {
        b_actual <- length(idx)

        optimizer$zero_grad()

        if (method == "colnorm") {
          # Column-only normalization: W[b,i,j] = K[b,i,j] / colSum(K)[j]
          # All n tracts participate in column sums (not just batch).
          alpha_all <- alpha_fn(theta_torch)              # (n, ka)
          log_K_3d_full <- -alpha_all$t()$unsqueeze(3L) *
            log_t_torch$unsqueeze(1L)                     # (ka, n, m)

          # Column normalization in log-space
          log_col_sum_full <- torch::torch_logsumexp(
            log_K_3d_full, dim = 2L)                      # (ka, m)
          log_W_3d <- log_K_3d_full -
            log_col_sum_full$unsqueeze(2L)                # (ka, n, m)

          # V_hat[i,b] = sum_j W[b,i,j] * c[b,j]
          log_c_3d <- log_V_b_torch$unsqueeze(2L)        # (ka, 1, m)
          V_hat_all <- torch::torch_exp(
            torch::torch_logsumexp(
              log_W_3d + log_c_3d, dim = 3L))$t()        # (n, ka)

          # Data loss on batch only, scaled to full-dataset units
          V_hat_batch <- V_hat_all[idx, ]                 # (b_actual, ka)
          P_batch     <- p_active_torch[idx, ]            # (b_actual, ka)
          data_loss <- compute_data_loss(
            V_hat_batch, P_batch, as.double(n) / b_actual)

          # Log-barrier penalty on ALL tracts
          if (barrier_mu > 0) {
            V_hat_total <- V_hat_all$sum(dim = 2L)        # (n,)
            barrier <- -barrier_mu *
              torch::torch_log(
                torch::torch_clamp(V_hat_total, min = 1e-30))$sum()
            loss <- data_loss + barrier
          } else {
            loss <- data_loss
          }

        } else {
          # --- Sinkhorn IPF path ---
          # Mini-batch 3D IPF targets
          r_total_batch <- r_total_norm[idx]
          batch_r_sum   <- sum(r_total_batch)
          log_r_batch <- torch::torch_tensor(
            log(pmax(r_total_batch, 1e-30)) +
              log(pmax(V_total_val, 1e-30)),
            device = device, dtype = torch_dtype)        # (b_actual,)
          log_col_batch <- log_V_b_torch +
            log(pmax(batch_r_sum, 1e-30))                # (ka, m)

          log_t_batch  <- log_t_torch[idx, ]             # (b_actual, m)
          alpha_batch  <- alpha_fn(theta_torch[idx, ])   # (b_actual, ka)
          # (ka, b_actual, 1) * (1, b_actual, m) -> (ka, b_actual, m)
          log_K_3d_batch <- -alpha_batch$t()$unsqueeze(3L) *
            log_t_batch$unsqueeze(1L)

          # 3D log-domain IPF (Sinkhorn)
          log_u <- torch::torch_zeros(
            b_actual, device = device, dtype = torch_dtype)
          log_v <- torch::torch_zeros(
            c(ka, m), device = device, dtype = torch_dtype)

          for (sk_i in seq_len(sk_iter)) {
            log_u_prev <- log_u

            log_KV <- log_K_3d_batch + log_v$unsqueeze(2L)
            log_row_sum <- torch::torch_logsumexp(
              torch::torch_logsumexp(log_KV, dim = 3L),
              dim = 1L)                                  # (b_actual,)
            log_u <- log_r_batch - log_row_sum

            log_KU <- log_K_3d_batch +
              log_u$unsqueeze(1L)$unsqueeze(3L)
            log_col_sum <- torch::torch_logsumexp(
              log_KU, dim = 2L)                          # (ka, m)
            log_v <- log_col_batch - log_col_sum

            if (sk_i >= 2L) {
              sk_delta <- (log_u - log_u_prev)$abs()$max()$item()
              if (is.finite(sk_delta) && sk_delta < sk_tol) break
            }
          }

          # Transport plan: T[b,i,j] = K^b[i,j] * u[i] * v[b,j]
          log_T_final <- log_K_3d_batch +
            log_u$unsqueeze(1L)$unsqueeze(3L) +
            log_v$unsqueeze(2L)
          V_hat_batch <- torch::torch_exp(
            torch::torch_logsumexp(
              log_T_final, dim = 3L))$t()                # (b_actual, ka)

          P_batch <- p_active_torch[idx, ]               # (b_actual, ka)
          loss    <- compute_data_loss(
            V_hat_batch, P_batch, as.double(n) / b_actual)
        }

        loss$backward()

        # Capture pre-clip gradient norm, then clip
        last_grad_norm <- tryCatch(
          as.numeric(theta_torch$grad$norm()$cpu()),
          error = function(e) NA_real_
        )
        torch::nn_utils_clip_grad_norm_(
          list(theta_torch), max_norm = 1.0)

        optimizer$step()
        step_counter <- step_counter + 1L
      }  # end mini-batch loop

      # --- True epoch loss: full-dataset forward pass ---
      torch::with_no_grad({
        alpha_ep <- alpha_fn(theta_torch)              # (n, ka)
        log_K_3d_ep <- -alpha_ep$t()$unsqueeze(3L) *
          log_t_torch$unsqueeze(1L)                    # (ka, n, m)

        if (method == "colnorm") {
          # Column normalization
          log_col_sum_ep <- torch::torch_logsumexp(
            log_K_3d_ep, dim = 2L)                     # (ka, m)
          log_W_3d_ep <- log_K_3d_ep -
            log_col_sum_ep$unsqueeze(2L)               # (ka, n, m)

          log_c_3d_ep <- log_V_b_torch$unsqueeze(2L)  # (ka, 1, m)
          V_hat_ep <- torch::torch_exp(
            torch::torch_logsumexp(
              log_W_3d_ep + log_c_3d_ep, dim = 3L))$t()  # (n, ka)

          epoch_loss_val <- as.numeric(
            compute_data_loss(V_hat_ep, p_active_torch, 1.0)$item())

          # Derive W: aggregate per-bracket column-normalized weights
          log_T_ep <- log_W_3d_ep + log_c_3d_ep       # (ka, n, m)
          log_c_agg_ep <- torch::torch_logsumexp(
            log_V_b_torch, dim = 1L)                   # (m,)
          log_T_agg_ep <- torch::torch_logsumexp(
            log_T_ep, dim = 1L)                        # (n, m)
          W_candidate <- as.matrix(torch::torch_exp(
            log_T_agg_ep - log_c_agg_ep$unsqueeze(1L))$cpu())

        } else {
          # Full-dataset 3D Sinkhorn IPF
          log_u_ep <- torch::torch_zeros(
            n, device = device, dtype = torch_dtype)
          log_v_ep <- torch::torch_zeros(
            c(ka, m), device = device, dtype = torch_dtype)

          sk_converged_ep <- FALSE
          sk_delta_ep     <- Inf
          for (sk_i_ep in seq_len(sk_iter)) {
            log_u_ep_prev <- log_u_ep

            log_KV_ep <- log_K_3d_ep + log_v_ep$unsqueeze(2L)
            log_row_sum_ep <- torch::torch_logsumexp(
              torch::torch_logsumexp(log_KV_ep, dim = 3L),
              dim = 1L)
            log_u_ep <- log_row_target_full - log_row_sum_ep

            log_KU_ep <- log_K_3d_ep +
              log_u_ep$unsqueeze(1L)$unsqueeze(3L)
            log_col_sum_ep <- torch::torch_logsumexp(
              log_KU_ep, dim = 2L)
            log_v_ep <- log_V_b_torch - log_col_sum_ep

            if (sk_i_ep >= 2L) {
              sk_delta_ep <- (log_u_ep - log_u_ep_prev
                )$abs()$max()$item()
              if (is.finite(sk_delta_ep) &&
                  sk_delta_ep < sk_tol) {
                sk_converged_ep <- TRUE
                break
              }
            }
          }
          if (!sk_converged_ep) {
            sk_nonconv_count <- sk_nonconv_count + 1L
          }

          log_T_ep <- log_K_3d_ep +
            log_u_ep$unsqueeze(1L)$unsqueeze(3L) +
            log_v_ep$unsqueeze(2L)
          V_hat_ep <- torch::torch_exp(
            torch::torch_logsumexp(
              log_T_ep, dim = 3L))$t()                 # (n, ka)

          epoch_loss_val <- as.numeric(
            compute_data_loss(V_hat_ep, p_active_torch, 1.0)$item())

          log_c_agg_ep <- torch::torch_logsumexp(
            log_V_b_torch, dim = 1L)                   # (m,)
          log_T_agg_ep <- torch::torch_logsumexp(
            log_T_ep, dim = 1L)                        # (n, m)
          W_candidate <- as.matrix(torch::torch_exp(
            log_T_agg_ep - log_c_agg_ep$unsqueeze(1L))$cpu())
        }

        alpha_candidate <- as.matrix(alpha_ep$cpu())
      })

      epoch_losses[epoch] <- epoch_loss_val

      # Step the LR scheduler with the true epoch loss
      scheduler$step(epoch_loss_val)

      grad_history[epoch] <- last_grad_norm
      lr_history[epoch]   <- optimizer$param_groups[[1]]$lr

      # --- Convergence check ---
      # Converge when LR has been reduced to min_lr AND no improvement
      # for `patience` consecutive epochs at that LR.
      # NOTE: must check BEFORE updating best_epoch_loss, otherwise the
      # comparison (epoch_loss_val < best_epoch_loss - tol*|best|) is
      # always false after a best-loss update.
      current_lr <- optimizer$param_groups[[1]]$lr
      lr_at_min  <- (current_lr <= min_lr * 1.01)

      if (is.finite(epoch_loss_val) && epoch >= 5L) {
        if (epoch_loss_val < best_epoch_loss -
              convergence_tol * abs(best_epoch_loss)) {
          patience_counter <- 0L
        } else {
          patience_counter <- patience_counter + 1L
        }

        if (lr_at_min && patience_counter >= patience) {
          converged <- TRUE
          if (verbose) {
            message(sprintf(paste0(
              "  Converged at epoch %d (loss=%s, lr=%.1e, ",
              "no improvement for %d epochs)"),
              epoch,
              format(round(epoch_loss_val), big.mark = ","),
              current_lr, patience))
          }
          break
        }
      }

      # Track best alpha by true epoch loss (after convergence check)
      if (is.finite(epoch_loss_val) &&
          epoch_loss_val < best_epoch_loss) {
        best_epoch_loss <- epoch_loss_val
        best_alpha <- alpha_candidate
        best_W <- W_candidate
      }

      if (verbose &&
          (epoch == 1L || epoch %% report_every == 0L)) {
        message(sprintf(paste0(
          "  Epoch %3d/%d: loss=%s, grad=%.2e, lr=%.1e, ",
          "alpha=[%.2f, %.2f, %.2f]"),
          epoch, max_epochs,
          format(round(epoch_loss_val), big.mark = ","),
          last_grad_norm,
          optimizer$param_groups[[1]]$lr,
          min(alpha_candidate), stats::median(alpha_candidate),
          max(alpha_candidate)
        ))
      }
    }  # end epoch loop

    # Warn if Sinkhorn did not converge in any epoch
    if (method == "sinkhorn" && sk_nonconv_count > 0L) {
      warning(sprintf(paste0(
        "Sinkhorn did not converge within %d iterations ",
        "in %d of %d epoch evaluations (tol=%.1e). ",
        "Consider increasing sk_iter."),
        sk_iter, sk_nonconv_count, epoch, sk_tol
      ), call. = FALSE)
    }

    list(
      alpha           = best_alpha,   # n × ka matrix
      active_brackets = active,       # which brackets are active
      W               = best_W,       # n × m weight matrix
      value           = best_epoch_loss,
      method          = paste0("pb_sgd_", method, "_", device),
      convergence     = if (converged) 0L else 1L,
      epochs          = epoch,
      steps           = step_counter,
      message         = sprintf(paste0(
        "PB-SGD on %s (%s), %d epochs (%d steps), ",
        "batch=%d, %d brackets, ",
        "sk_iter=%d (tol=%.1e)"),
        device, dtype, epoch, step_counter,
        b, ka, sk_iter, sk_tol),
      history             = epoch_losses[seq_len(epoch)],
      grad_norm_final     = last_grad_norm,
      grad_history        = grad_history[seq_len(epoch)],
      lr_history          = lr_history[seq_len(epoch)],
      n_batches_per_epoch = n_batches_per_epoch
    )
  }, error = function(e) {
    stop(sprintf(
      "Torch optimization failed on device '%s': %s",
      device, e$message
    ), call. = FALSE)
  })

  result
}

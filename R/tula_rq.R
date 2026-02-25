#' @rdname tula
#' @export
tula.rq <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                    width = NULL, exp = FALSE, level = 95,
                    robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  tau   <- model$tau
  n_obs <- length(model$residuals)

  # summary.rq returns coefficient table with CIs by default.
  # Columns: "coefficients", "lower bd", "upper bd"  (when se != "rank")
  # or: "Value", "Std. Error", "t value", "Pr(>|t|)" (when se = "nid"/"iid"/"ker")
  # We use se = "nid" to get standard 4-column output with t-stats.
  s <- summary(model, se = "nid")

  # Extract 4-column coefficient table: Value, Std. Error, t value, Pr(>|t|)
  ct <- s$coefficients
  colnames(ct) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  # Apply robust SE if requested
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, "t", df = df_resid)
    ci <- if (wide) .robust_ci(ct, level, "t", df = df_resid) else NULL
  } else {
    # Wald-type CIs using model SEs
    ci <- if (wide) {
      z_crit <- stats::qnorm(0.5 + level / 200)
      cbind(
        "2.5 %"  = ct[, "Estimate"] - z_crit * ct[, "Std. Error"],
        "97.5 %" = ct[, "Estimate"] + z_crit * ct[, "Std. Error"]
      )
    } else NULL
  }

  # Header metrics
  # Raw sum of deviations (sum of weighted absolute residuals for fitted model)
  wt_resid <- ifelse(model$residuals >= 0,
                     tau * abs(model$residuals),
                     (1 - tau) * abs(model$residuals))
  raw_sum <- sum(wt_resid)

  # Min sum of deviations (from the fitted model)
  min_sum <- sum(ifelse(model$residuals >= 0,
                        tau * model$residuals,
                        (1 - tau) * model$residuals))
  # Actually min_sum is the objective value from the fitted model
  # raw_sum is the objective value from the null (intercept-only) model
  # Let's compute both properly:

  # Null model: intercept-only quantile regression
  y <- model$y
  if (is.null(y)) y <- stats::model.response(stats::model.frame(model))
  null_resid <- y - stats::quantile(y, tau)
  raw_sum_dev <- sum(ifelse(null_resid >= 0,
                            tau * null_resid,
                            (1 - tau) * abs(null_resid)))

  # Fitted model objective
  fit_resid <- model$residuals
  min_sum_dev <- sum(ifelse(fit_resid >= 0,
                            tau * fit_resid,
                            (1 - tau) * abs(fit_resid)))

  # Koenker-Bassett Pseudo R2 = 1 - (min sum / raw sum)
  pseudo_r2 <- 1 - min_sum_dev / raw_sum_dev

  header_left <- c(
    "Raw sum of dev" = raw_sum_dev,
    "Min sum of dev" = min_sum_dev,
    "K-B Pseudo R2"  = pseudo_r2
  )
  header_right <- c(
    "Number of obs" = n_obs,
    if (!is.null(robust_info$cluster_n)) c("Num. clusters" = robust_info$cluster_n)
  )

  # Family label: "Median regression" for tau=0.5, else "90th Quantile regression"
  family_label <- .rq_family_label(tau)

  # Extract design matrix info explicitly.
  # model.matrix(rq_object) fails because it can't find the data; bypass by
  # calling model.matrix on the terms with the model frame as data.
  mf     <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  mm_raw <- tryCatch(
    model.matrix(terms(model), data = mf),
    error = function(e) NULL
  )
  assign_vec <- if (!is.null(mm_raw)) {
    av <- attr(mm_raw, "assign")
    if (!is.null(av)) av else seq_along(rownames(ct))
  } else {
    seq_along(rownames(ct))
  }
  term_labels  <- attr(terms(model), "term.labels")
  data_classes <- tryCatch(
    attr(terms(model), "dataClasses"),
    error = function(e) character(0)
  )
  xlevels <- if (!is.null(model$xlevels)) model$xlevels else list()

  # Build coef_df
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                            ref          = opts$ref,
                            label        = opts$label,
                            assign_vec   = assign_vec,
                            term_labels  = term_labels,
                            data_classes = data_classes,
                            xlevels      = xlevels,
                            model_frame  = mf)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  new_tula_output(
    model_type   = "rq",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = "t",
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = c("Raw sum of dev" = "f3", "Min sum of dev" = "f3"),
    exp          = exp,
    dep_var      = dep_var,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
}


#' @rdname tula
#' @export
tula.rqs <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                     width = NULL, exp = FALSE, level = 95,
                     parallel = FALSE,
                     robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide <- .resolve_wide(wide, width)

  taus  <- model$tau
  n_obs <- length(model$residuals[, 1L])

  # Response vector
  y <- model$y
  if (is.null(y)) y <- stats::model.response(stats::model.frame(model))

  # Design matrix info (shared across all quantiles).
  # model.matrix(rqs_object) fails; use terms + model.frame instead.
  mf      <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  mm_raw  <- tryCatch(
    model.matrix(terms(model), data = mf),
    error = function(e) NULL
  )
  assign_vec <- if (!is.null(mm_raw)) {
    av <- attr(mm_raw, "assign")
    if (!is.null(av)) av else NULL
  } else NULL
  term_labels  <- tryCatch(attr(terms(model), "term.labels"),
                           error = function(e) character(0))
  data_classes <- tryCatch(attr(terms(model), "dataClasses"),
                           error = function(e) character(0))
  xlevels <- if (!is.null(model$xlevels)) model$xlevels else list()

  opts <- .parse_tula_opts(ref, label)

  # Pre-check if sandwich is available when robust SEs are requested,
  # to fail early with a clear error before the per-block loop.
  needs_robust <- isTRUE(robust) || !is.null(vcov) || !is.null(cluster)
  if (needs_robust && !requireNamespace("sandwich", quietly = TRUE)) {
    stop(
      "Package 'sandwich' is required for robust standard errors.\n",
      "Install it with: install.packages(\"sandwich\")",
      call. = FALSE
    )
  }

  # Build one block per quantile
  blocks <- lapply(seq_along(taus), function(k) {
    tau_k <- taus[k]

    # Extract single-quantile coefficients
    coef_k  <- model$coefficients[, k]
    resid_k <- model$residuals[, k]

    # Build a lightweight single-tau rq object for summary.
    # Construction and summary are separated so single_rq is available both
    # for summary() and for .resolve_robust_vcov() below.
    single_rq <- tryCatch({
      r <- model
      r$tau          <- tau_k
      r$coefficients <- coef_k
      r$residuals    <- resid_k
      class(r)       <- "rq"
      r
    }, error = function(e) NULL)

    s_k <- if (!is.null(single_rq)) {
      tryCatch(summary(single_rq, se = "nid"), error = function(e) NULL)
    } else NULL

    if (is.null(s_k)) {
      # Fallback: basic coefficient matrix without SEs
      ct_k <- cbind(
        "Estimate"   = coef_k,
        "Std. Error" = NA_real_,
        "t value"    = NA_real_,
        "Pr(>|t|)"   = NA_real_
      )
    } else {
      ct_k <- s_k$coefficients
      colnames(ct_k) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    }

    # Apply robust SE per quantile if requested
    robust_info_k <- NULL
    if (needs_robust && !is.null(single_rq)) {
      robust_info_k <- tryCatch(
        .resolve_robust_vcov(single_rq, robust, vcov, cluster),
        error = function(e) NULL
      )
      if (!is.null(robust_info_k)) {
        df_k  <- tryCatch(stats::df.residual(single_rq), error = function(e) Inf)
        ct_k  <- .recompute_ct_robust(ct_k, robust_info_k$vcov_mat, "t", df = df_k)
      }
    }

    ci_k <- if (wide) {
      if (!is.null(robust_info_k)) {
        df_k <- tryCatch(stats::df.residual(single_rq), error = function(e) Inf)
        .robust_ci(ct_k, level, "t", df = df_k)
      } else {
        z_crit <- stats::qnorm(0.5 + level / 200)
        cbind(
          "2.5 %"  = ct_k[, "Estimate"] - z_crit * ct_k[, "Std. Error"],
          "97.5 %" = ct_k[, "Estimate"] + z_crit * ct_k[, "Std. Error"]
        )
      }
    } else NULL

    coef_df <- build_coef_df(
      model        = model,
      ct           = ct_k,
      ci           = ci_k,
      wide         = wide,
      ref          = opts$ref,
      label        = opts$label,
      assign_vec   = assign_vec,
      term_labels  = term_labels,
      data_classes = data_classes,
      xlevels      = xlevels,
      model_frame  = mf
    )

    list(outcome = .rq_short_label(tau_k), coef_df = coef_df, tau = tau_k)
  })

  # Derive se_label and cluster_n from the first block's robust_info (if any).
  # We re-check needs_robust here; if sandwich failed in all blocks, se_label
  # stays NULL (output falls back to "Std. Err." automatically).
  rqs_se_label  <- NULL
  rqs_cluster_n <- NULL
  if (needs_robust) {
    # Build a representative single-tau rq for the first quantile to get
    # se_label and cluster_n without repeating the full per-block logic.
    rep_rq <- tryCatch({
      r <- model
      r$tau          <- taus[1L]
      r$coefficients <- model$coefficients[, 1L]
      r$residuals    <- model$residuals[, 1L]
      class(r)       <- "rq"
      r
    }, error = function(e) NULL)
    if (!is.null(rep_rq)) {
      rep_ri <- tryCatch(
        .resolve_robust_vcov(rep_rq, robust, vcov, cluster),
        error = function(e) NULL
      )
      if (!is.null(rep_ri)) {
        rqs_se_label  <- rep_ri$se_label
        rqs_cluster_n <- rep_ri$cluster_n
      }
    }
  }

  # Shared header: aggregate across all quantiles is not standard.
  # Show the range of quantiles and N.
  # For the header, show stats for the first quantile (user can see per-block).
  # Actually, follow Stata: shared header has only N.
  header_left  <- numeric(0)
  header_right <- c(
    "Number of obs" = n_obs,
    if (!is.null(rqs_cluster_n)) c("Num. clusters" = rqs_cluster_n)
  )

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  family_label <- paste0("Quantile regression (",
                         length(taus), " quantiles)")

  new_tula_rqs_output(
    header_left  = header_left,
    header_right = header_right,
    blocks       = blocks,
    stat_label   = "t",
    wide         = wide,
    width        = width,
    value_fmts   = character(0L),
    exp          = exp,
    parallel     = isTRUE(parallel),
    dep_var      = dep_var,
    level        = level,
    family_label = family_label,
    se_label     = rqs_se_label
  )
}


# ---------------------------------------------------------------------------
# Internal: format the family label for a quantile regression.
#
# tau=0.5 → "Median regression"
# tau=0.9 → "90th Quantile regression"
# tau=0.25 → "25th Quantile regression"
# ---------------------------------------------------------------------------
.rq_family_label <- function(tau) {
  if (tau == 0.5) return("Median regression")
  pct <- tau * 100
  # Format as integer if no fractional part
  pct_str <- if (pct == as.integer(pct)) as.character(as.integer(pct)) else as.character(pct)
  # Ordinal suffix
  suffix <- .ordinal_suffix(as.numeric(pct_str))
  paste0(pct_str, suffix, " Quantile regression")
}

# Internal: short label for stacked/parallel block headers.
#
# tau=0.5  → "Median"
# tau=0.9  → "90th Q"
# tau=0.25 → "25th Q"
# ---------------------------------------------------------------------------
.rq_short_label <- function(tau) {
  if (tau == 0.5) return("Median")
  pct <- tau * 100
  pct_str <- if (pct == as.integer(pct)) as.character(as.integer(pct)) else as.character(pct)
  suffix <- .ordinal_suffix(as.numeric(pct_str))
  paste0(pct_str, suffix, " Q")
}

# Internal: ordinal suffix for a number (1st, 2nd, 3rd, 4th, ...)
.ordinal_suffix <- function(n) {
  n <- abs(n)
  if (n %% 100 %in% 11:13) return("th")
  switch(as.character(n %% 10),
         "1" = "st",
         "2" = "nd",
         "3" = "rd",
         "th")
}


# ---------------------------------------------------------------------------
# Internal constructor for tula_rqs_output S3 objects (multi-quantile).
# Follows the same pattern as tula_multinom_output.
# ---------------------------------------------------------------------------
new_tula_rqs_output <- function(header_left,
                                 header_right,
                                 blocks,
                                 stat_label   = "t",
                                 wide         = FALSE,
                                 width        = NULL,
                                 value_fmts   = character(0L),
                                 exp          = FALSE,
                                 parallel     = FALSE,
                                 dep_var      = NULL,
                                 level        = 95,
                                 family_label = NULL,
                                 se_label     = NULL) {
  structure(
    list(
      header_left  = header_left,
      header_right = header_right,
      blocks       = blocks,
      stat_label   = stat_label,
      wide         = wide,
      width        = width,
      value_fmts   = value_fmts,
      exp          = exp,
      parallel     = parallel,
      dep_var      = dep_var,
      level        = level,
      family_label = family_label,
      se_label     = se_label
    ),
    class = "tula_rqs_output"
  )
}


#' Print method for tula_rqs_output objects
#'
#' Prints Stata-style quantile regression output for multiple quantiles:
#' a shared header, then one coefficient table per quantile separated by
#' dashed lines. Each quantile label appears on the column-header line.
#'
#' @param x A `tula_rqs_output` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.tula_rqs_output <- function(x, ...) {
  max_w <- .resolve_width(x$width)
  vf    <- if (is.null(x$value_fmts)) character(0L) else x$value_fmts

  all_labels <- unlist(lapply(x$blocks, function(b) b$coef_df$label))

  natural_width <- compute_total_width(
    header_left  = x$header_left,
    header_right = x$header_right,
    coef_labels  = all_labels,
    wide         = x$wide,
    value_fmts   = vf
  )

  if (isTRUE(x$parallel)) {
    # Parallel layout
    n_out_p            <- length(x$blocks)
    cw_col_p           <- .parallel_nz_w(x$blocks, isTRUE(x$exp)) + 3L
    stacked_num_cols_w <- 2L + 10L + 1L + 10L + 1L + 10L + 1L + 9L
    parallel_num_w     <- 2L + n_out_p * cw_col_p + (n_out_p - 1L) * 2L
    lbl_w_natural      <- min(natural_width, max_w) - stacked_num_cols_w
    total_width        <- min(lbl_w_natural + parallel_num_w, max_w)
  } else {
    total_width <- min(natural_width, max_w)
  }

  sep_line <- char_rep("-", total_width)

  num_cols_w <- 2L + 10L + 1L + 10L + 1L + 10L + 1L + 9L
  if (x$wide) num_cols_w <- num_cols_w + 1L + 10L + 1L + 10L
  lbl_w <- total_width - num_cols_w

  # Family label
  if (!is.null(x$family_label)) {
    cat(x$family_label, "\n", sep = "")
  }

  # Header (shared)
  if (length(x$header_left) > 0L || length(x$header_right) > 0L) {
    header_lines <- format_header(x$header_left, x$header_right,
                                  total_width = total_width,
                                  value_fmts  = vf)
    cat(paste(header_lines, collapse = "\n"), "\n", sep = "")
  }

  if (isTRUE(x$parallel)) {
    # Parallel: use the same renderer as multinom
    table_lines <- .format_parallel_multinom_table(
      blocks      = x$blocks,
      dep_var     = x$dep_var,
      total_width = total_width,
      exp         = isTRUE(x$exp)
    )
    cat(paste(table_lines, collapse = "\n"), "\n", sep = "")
    cat("* p<0.05  ** p<0.01  *** p<0.001\n")
  } else {
    # Stacked layout: one block per quantile
    for (i in seq_along(x$blocks)) {
      blk <- x$blocks[[i]]
      table_lines <- format_coef_table(blk$coef_df, x$stat_label, x$wide,
                                       total_width = total_width,
                                       exp       = isTRUE(x$exp),
                                       exp_label = NULL,
                                       level     = x$level %||% 95L,
                                       se_label  = x$se_label)
      inner_lines <- table_lines[-c(1L, length(table_lines))]

      # Embed quantile label in the label-column area
      outcome_lbl <- .truncate_label(blk$outcome, lbl_w)
      hdr_line    <- inner_lines[1L]
      inner_lines[1L] <- paste0(pad_right(outcome_lbl, lbl_w),
                                substring(hdr_line, lbl_w + 1L))

      cat(sep_line, "\n", sep = "")
      cat(paste(inner_lines, collapse = "\n"), "\n", sep = "")
    }
    cat(sep_line, "\n", sep = "")
  }

  invisible(x)
}

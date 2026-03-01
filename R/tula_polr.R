#' @rdname tula
#' @export
tula.polr <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                      width = NULL, exp = FALSE, level = 95,
                      robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide <- .resolve_wide(wide, width)

  s <- summary(model)

  # summary(polr)$coefficients has 3 columns: "Value", "Std. Error", "t value"
  # Rows cover BOTH predictor coefficients and cutpoints (zeta).
  ct_all <- s$coefficients

  # Separate predictor coefficients from cutpoints (model$zeta)
  pred_names <- names(stats::coef(model))   # predictors only
  zeta_names <- names(model$zeta)           # cutpoints, e.g. "1|2", "2|3"

  ct_pred_raw <- ct_all[pred_names, , drop = FALSE]
  ct_zeta_raw <- ct_all[zeta_names, , drop = FALSE]

  # polr labels the statistic "t value" but it is asymptotically z-distributed.
  # Compute two-tailed p-values: p = 2 * P(Z > |t|)
  t_vals <- ct_pred_raw[, "t value"]
  p_vals <- 2 * stats::pnorm(-abs(t_vals))

  # Build standard 4-column ct matrix (Estimate, SE, z, p) for predictors
  ct <- cbind(
    "Estimate"   = ct_pred_raw[, "Value"],
    "Std. Error" = ct_pred_raw[, "Std. Error"],
    "z value"    = t_vals,
    "Pr(>|z|)"   = p_vals
  )
  rownames(ct) <- pred_names

  # Cutpoint coefficient matrix (2 cols used: estimate, SE)
  ct_zeta <- cbind(
    "Estimate"   = ct_zeta_raw[, "Value"],
    "Std. Error" = ct_zeta_raw[, "Std. Error"]
  )
  rownames(ct_zeta) <- zeta_names

  # Apply robust SE if requested (uses full var-cov covering all params)
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    v_use <- robust_info$vcov_mat
    ct    <- .recompute_ct_robust(ct, v_use, "z")
    # Update cutpoint SEs from robust vcov
    ct_zeta[, "Std. Error"] <- sqrt(diag(v_use)[zeta_names])
  } else {
    v_use <- stats::vcov(model)   # full var-cov for all params (model SEs)
  }

  # Wald confidence intervals via vcov (covers both predictors and cutpoints)
  if (wide) {
    z_crit <- stats::qnorm(0.5 + level / 200)
    ses    <- sqrt(diag(v_use))

    ci_pred <- cbind(
      "2.5 %"  = ct[, "Estimate"]      - z_crit * ses[pred_names],
      "97.5 %" = ct[, "Estimate"]      + z_crit * ses[pred_names]
    )
    rownames(ci_pred) <- pred_names

    ci_zeta <- cbind(
      "2.5 %"  = model$zeta - z_crit * ses[zeta_names],
      "97.5 %" = model$zeta + z_crit * ses[zeta_names]
    )
    rownames(ci_zeta) <- zeta_names
  } else {
    ci_pred <- NULL
    ci_zeta <- NULL
  }

  # Log-likelihood
  ll_fitted <- as.numeric(stats::logLik(model))

  # McFadden R²: LL_null computed from marginal proportions of the outcome.
  # This avoids fitting a separate null model while giving the correct value.
  y       <- model$model[[1L]]
  y_tbl   <- table(y)
  n       <- sum(y_tbl)
  ll_null <- sum(y_tbl * log(y_tbl / n))
  pseudo_r2 <- 1 - ll_fitted / ll_null

  # Header blocks
  header_left <- c(
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model),
    "Log likelihood" = ll_fitted
  )
  header_right <- c(
    "Number of obs" = n,
    if (!is.null(robust_info$cluster_n)) c("Num. clusters" = robust_info$cluster_n),
    "McFadden R-sq" = pseudo_r2
  )

  # Extract design matrix info explicitly.
  # model.matrix() on a polr object includes an intercept column (assign = 0)
  # because the default method uses the formula's terms object (which has an
  # implicit intercept).  polr itself strips that column internally, so
  # coef(model) has no intercept — the two are misaligned by 1.  Fix: take
  # only the non-zero elements of assign_vec so positions match pred_names.
  mm_raw     <- tryCatch(model.matrix(model), error = function(e) NULL)
  assign_vec <- if (!is.null(mm_raw)) {
    av <- attr(mm_raw, "assign")
    if (!is.null(av)) av[av != 0L] else seq_along(pred_names)
  } else {
    seq_along(pred_names)  # fallback: 1, 2, 3, ...
  }
  term_labels  <- attr(terms(model), "term.labels")
  data_classes <- tryCatch(
    attr(terms(model), "dataClasses"),
    error = function(e) character(0)
  )
  xlevels   <- if (!is.null(model$xlevels)) model$xlevels else list()
  mf        <- tryCatch(model.frame(model), error = function(e) NULL)

  # Build coef_df for predictor coefficients
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci_pred, wide,
                            ref          = opts$ref,
                            label        = opts$label,
                            assign_vec   = assign_vec,
                            term_labels  = term_labels,
                            data_classes = data_classes,
                            xlevels      = xlevels,
                            model_frame  = mf)

  # Append cutpoint rows (with separator in format_coef_table before them)
  cutpoint_rows <- .build_cutpoint_rows(ct_zeta, ci_zeta)
  coef_df <- rbind(coef_df, cutpoint_rows)

  # Link function label printed above the header block
  family_label <- paste0("Ordered regression / Link: ", model$method)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  out <- new_tula_output(
    model_type     = "polr",
    header_left    = header_left,
    header_right   = header_right,
    coef_df        = coef_df,
    stat_label     = "z",
    wide           = wide,
    family_label   = family_label,
    width          = width,
    value_fmts     = c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3"),
    exp            = exp,
    dep_var        = dep_var,
    level          = level,
    outcome_levels = model$lev,
    se_label       = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
  .attach_select(out, ...)
}

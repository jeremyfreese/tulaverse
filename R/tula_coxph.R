#' @rdname tula
#' @export
tula.coxph <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                       width = NULL, exp = TRUE, level = 95,
                       robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  s     <- summary(model)
  n_obs <- s$n

  # --- ct: coefficient matrix (Estimate, SE, z, p) -------------------------
  # summary(coxph)$coefficients columns:
  #   coef, exp(coef), se(coef), z, Pr(>|z|)
  ct_raw <- s$coefficients
  ct <- ct_raw[, c("coef", "se(coef)", "z", "Pr(>|z|)"), drop = FALSE]
  colnames(ct) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

  # Apply robust SE if requested
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, "z", df = df_resid)
    ci <- if (wide) .robust_ci(ct, level, "z", df = df_resid) else NULL
  } else {
    # --- ci: Wald confidence intervals (on log-hazard scale) ------------------
    # format_coef_table() exponentiates these when exp = TRUE.
    ci <- if (wide) stats::confint.default(model, level = level / 100) else NULL
  }

  # --- Header metrics -------------------------------------------------------
  n_events <- s$nevent
  ll       <- model$loglik[2L]   # model log-likelihood (loglik[1] = null)

  # Time at risk: sum of survival times from the response
  time_at_risk <- tryCatch({
    y <- model$y
    if (is.null(y)) {
      mf <- stats::model.frame(model)
      y  <- mf[[1L]]
    }
    sum(y[, 1L])
  }, error = function(e) NA_real_)

  # Concordance (C-statistic)
  concordance <- tryCatch(s$concordance[["C"]], error = function(e) NA_real_)

  header_left <- c(
    "No. of subjects"  = n_obs,
    "No. of failures"  = n_events,
    "Time at risk"     = time_at_risk,
    "Log likelihood"   = ll
  )
  header_right <- c(
    "Number of obs" = n_obs,
    if (!is.null(robust_info$cluster_n)) c("Num. clusters" = robust_info$cluster_n),
    AIC             = stats::AIC(model),
    Concordance     = concordance
  )

  # --- Family label ----------------------------------------------------------
  family_label <- paste0("Cox regression / Ties: ", model$method)

  # --- Coef data frame -------------------------------------------------------
  # model.matrix(coxph) has no intercept column — assign_vec entries are all
  # >= 1, so no intercept row is emitted.
  mm         <- stats::model.matrix(model)
  assign_vec <- attr(mm, "assign")

  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                           ref = opts$ref, label = opts$label,
                           assign_vec = assign_vec)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  # --- Output ----------------------------------------------------------------
  new_tula_output(
    model_type   = "coxph",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = "z",
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = c("Log likelihood" = "f3", AIC = "f3"),
    exp          = exp,
    dep_var      = dep_var,
    exp_label    = if (isTRUE(exp)) "Haz. Ratio" else NULL,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
}

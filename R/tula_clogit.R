# ---------------------------------------------------------------------------
# tula.clogit() — Stata-inspired output for conditional logistic regression
#
# survival::clogit() returns an object that inherits from both "clogit" and
# "coxph" (the conditional likelihood is maximised via the Cox partial
# likelihood with method = "exact"). Dispatching on "clogit" lets us:
#   - replace the Cox header (No. of subjects / No. of failures / Time at risk
#     / Concordance) with a logistic-regression header (LR chi2 / Prob > chi2
#     / Pseudo R2 / Number of strata)
#   - default to log-odds coefficients (exp = FALSE) rather than the Cox
#     default of exponentiated hazard ratios — matching Stata's `clogit`
#   - relabel the exp = TRUE column as "Odds Ratio" rather than "Haz. Ratio"
# ---------------------------------------------------------------------------

#' @rdname tula
#' @export
tula.clogit <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                        width = NULL, exp = FALSE, level = 95,
                        robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  s     <- summary(model)
  n_obs <- s$n

  # --- ct: coefficient matrix (Estimate, SE, z, p) -------------------------
  ct_raw <- s$coefficients
  ct <- ct_raw[, c("coef", "se(coef)", "z", "Pr(>|z|)"), drop = FALSE]
  colnames(ct) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

  # Robust / cluster SEs ----------------------------------------------------
  # clogit inherits from coxph, so sandwich::vcovHC / vcovCL will dispatch.
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, "z", df = df_resid)
    ci <- if (wide) .robust_ci(ct, level, "z", df = df_resid) else NULL
  } else {
    # CIs on the log-odds scale; format_coef_table() exponentiates when exp = TRUE.
    ci <- if (wide) stats::confint.default(model, level = level / 100) else NULL
  }

  # --- Header metrics -------------------------------------------------------
  ll       <- model$loglik[2L]   # fitted model log-likelihood
  ll_null  <- model$loglik[1L]   # null model log-likelihood (no predictors)

  # McFadden's pseudo R2
  pseudo_r2 <- if (!is.null(ll_null) && is.finite(ll_null) && ll_null != 0)
    1 - ll / ll_null else NA_real_

  # Likelihood ratio test from summary.coxph
  lr_chi2 <- tryCatch(unname(s$logtest[["test"]]),   error = function(e) NA_real_)
  lr_df   <- tryCatch(unname(s$logtest[["df"]]),     error = function(e) NA_real_)
  lr_p    <- tryCatch(unname(s$logtest[["pvalue"]]), error = function(e) NA_real_)

  # Number of groups (strata): look up the strata() column in the model frame.
  # model$strata is NULL for clogit, so we have to recover this from model.frame().
  n_groups <- tryCatch({
    mf <- stats::model.frame(model)
    sp <- attr(model$terms, "specials")$strata
    if (!is.null(sp) && length(sp) >= 1L) {
      # `specials` is a 1-based index into the terms' variable list; subtract 1
      # to align with names(mf) (which drops the LHS terms entry).
      strata_col_name <- names(mf)[sp[1L]]
      length(unique(mf[[strata_col_name]]))
    } else NA_integer_
  }, error = function(e) NA_integer_)

  # LR chi2 label includes the df, Stata-style: "LR chi2(2)"
  lr_label <- if (!is.na(lr_df))
    sprintf("LR chi2(%d)", as.integer(lr_df)) else "LR chi2"

  header_left <- stats::setNames(
    c(lr_chi2, lr_p, ll, pseudo_r2),
    c(lr_label, "Prob > chi2", "Log likelihood", "Pseudo R2")
  )
  header_right <- c(
    "Number of obs"    = n_obs,
    "Number of groups" = n_groups,
    if (!is.null(robust_info$cluster_n))
      c("Num. clusters" = robust_info$cluster_n)
  )

  # --- Family label ---------------------------------------------------------
  family_label <- "Conditional (fixed-effects) logistic regression"

  # --- Coef data frame ------------------------------------------------------
  # model.matrix(clogit) has no intercept column — assign_vec is all >= 1.
  mm         <- stats::model.matrix(model)
  assign_vec <- attr(mm, "assign")

  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                           ref = opts$ref, label = opts$label,
                           assign_vec = assign_vec)

  # --- Dependent variable label ---------------------------------------------
  # clogit rewrites the response as Surv(rep(1, n), case) before passing it
  # to coxph, so model$call$formula and formula(model)[[2L]] both give back
  # the Surv() expression rather than the original outcome name. clogit
  # preserves the user's original call as model$userCall — match.call() it
  # against clogit() to normalise positional / named forms, then peel off
  # the formula's LHS.
  dep_var <- tryCatch({
    uc <- model$userCall
    if (is.null(uc)) {
      deparse(formula(model)[[2L]])
    } else {
      mc <- match.call(survival::clogit, uc)
      uf <- eval(mc$formula, envir = environment(formula(model)))
      deparse(uf[[2L]])
    }
  }, error = function(e) {
    tryCatch(deparse(formula(model)[[2L]]), error = function(e2) NULL)
  })

  # --- Pseudo R2 may print as e.g. ".2928" via the default formatting -----
  # f3 keeps Log likelihood from drifting into scientific notation; pseudo R2
  # is small enough that the default g4 is fine.
  out <- new_tula_output(
    model_type   = "clogit",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = "z",
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = c("Log likelihood" = "f3"),
    exp          = exp,
    dep_var      = dep_var,
    exp_label    = if (isTRUE(exp)) "Odds Ratio" else NULL,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
  .attach_select(out, ...)
}

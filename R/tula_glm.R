#' @rdname tula
#' @export
tula.glm <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                     width = NULL, exp = FALSE, level = 95,
                     robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)
  s     <- summary(model)
  n_obs <- stats::nobs(model)
  ll    <- as.numeric(stats::logLik(model))

  # Pseudo-R² measures
  dev      <- model$deviance
  null_dev <- model$null.deviance
  n        <- n_obs

  # 1 - deviance/null.deviance. For Bernoulli binomial (binomial family with
  # all prior weights == 1) the saturated log-likelihood is zero, so this
  # equals McFadden's R² = 1 - logLik(fit)/logLik(null). For grouped
  # binomial, Poisson, Gamma, etc. the saturated log-likelihood is non-zero
  # and the expression is the "deviance R²" — still a sensible likelihood-
  # ratio index, but not strictly McFadden's. The header label is set
  # accordingly below.
  pseudo_r2 <- 1 - dev / null_dev
  is_bernoulli_binomial <- identical(model$family$family, "binomial") &&
    !is.null(model$prior.weights) && all(model$prior.weights == 1)
  pseudo_r2_label <- if (is_bernoulli_binomial) "McFadden R-sq" else "Deviance R-sq"

  # Nagelkerke's R²
  # = (1 - exp((dev - null_dev) / n)) / (1 - exp(-null_dev / n))
  # The formula assumes the saturated log-likelihood is zero, so it is
  # only strictly Nagelkerke's R² for Bernoulli binomial glms; we suppress
  # it outside that case rather than print a deviance-based analog that
  # would mislead. Cap at 1 to handle edge cases.
  nagelkerke <- if (is_bernoulli_binomial) {
    min(
      (1 - exp((dev - null_dev) / n)) / (1 - exp(-null_dev / n)),
      1
    )
  } else NA_real_

  fam <- model$family$family
  lnk <- model$family$link
  family_label <- paste0("Family: ", fam, " / Link: ", lnk)

  # Coefficient matrix; stat_label determined before robust adjustment
  ct          <- stats::coef(s)
  stat_col_nm <- colnames(ct)[3L]
  stat_label  <- if (grepl("^z", stat_col_nm, ignore.case = TRUE)) "z" else "t"

  # Apply robust SE if requested
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, stat_label, df = df_resid)
    ci <- if (wide) .robust_ci(ct, level, stat_label, df = df_resid) else NULL
  } else {
    # Use Wald CIs (confint.default) for speed; avoids slow profile likelihood
    # and matches Stata's default for logistic/Poisson regression
    ci <- if (wide) stats::confint.default(model, level = level / 100) else NULL
  }

  # Left header block
  header_left <- c(
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model),
    "Log likelihood" = ll
  )

  # Right header block. Nagelkerke is shown only when it's strictly defined
  # (Bernoulli binomial); otherwise the row is omitted entirely rather than
  # printed under a misleading label.
  header_right <- c(
    "Number of obs"   = n_obs,
    if (!is.null(robust_info$cluster_n)) c("Num. clusters" = robust_info$cluster_n),
    stats::setNames(pseudo_r2, pseudo_r2_label),
    if (is_bernoulli_binomial) c("Nagelkerke R-sq" = nagelkerke)
  )

  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide, ref = opts$ref, label = opts$label)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  out <- new_tula_output(
    model_type   = "glm",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = stat_label,
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3"),
    exp          = exp,
    dep_var      = dep_var,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
  .attach_select(out, ...)
}

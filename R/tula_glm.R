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

  # McFadden's R² = 1 - L_fitted / L_null = 1 - deviance / null.deviance
  mcfadden <- 1 - dev / null_dev

  # Nagelkerke's R²
  # = (1 - exp((dev - null_dev) / n)) / (1 - exp(-null_dev / n))
  # Cap at 1 to handle edge cases (e.g. Gaussian glm where formula can exceed 1)
  nagelkerke <- min(
    (1 - exp((dev - null_dev) / n)) / (1 - exp(-null_dev / n)),
    1
  )

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

  # Right header block
  header_right <- c(
    "Number of obs"   = n_obs,
    if (!is.null(robust_info$cluster_n)) c("Num. clusters" = robust_info$cluster_n),
    "McFadden R-sq"   = mcfadden,
    "Nagelkerke R-sq" = nagelkerke
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

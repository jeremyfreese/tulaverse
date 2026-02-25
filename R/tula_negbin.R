#' @rdname tula
#' @export
tula.negbin <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                        width = NULL, exp = FALSE, level = 95,
                        robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  s     <- summary(model)
  n_obs <- stats::nobs(model)

  # --- ct: standard 4-column coefficient matrix (z-based) -------------------
  ct <- stats::coef(s)   # Estimate, Std. Error, z value, Pr(>|z|)

  # Apply robust SE if requested
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, "z", df = df_resid)
    ci <- if (wide) .robust_ci(ct, level, "z", df = df_resid) else NULL
  } else {
    # --- ci: Wald confidence intervals (fast, matches Stata default) ----------
    ci <- if (wide) stats::confint.default(model, level = level / 100) else NULL
  }

  # --- Log-likelihood and McFadden R-sq -------------------------------------
  ll <- as.numeric(stats::logLik(model))

  # McFadden R-sq requires the null model's log-likelihood.  We must fit an
  # intercept-only NB model because theta differs between null and fitted.
  null_model <- tryCatch(
    stats::update(model, . ~ 1, trace = FALSE),
    error = function(e) NULL
  )
  ll_null  <- if (!is.null(null_model)) as.numeric(stats::logLik(null_model)) else NA_real_
  mcfadden <- if (!is.na(ll_null) && ll_null != 0) 1 - ll / ll_null else NA_real_

  # --- Header blocks --------------------------------------------------------
  header_left <- c(
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model),
    "Log likelihood" = ll
  )
  header_right <- c(
    "Number of obs" = n_obs,
    "McFadden R-sq" = mcfadden
  )
  if (!is.null(robust_info$cluster_n)) {
    header_right <- c(header_right, "Num. clusters" = robust_info$cluster_n)
  }

  # --- Family label ---------------------------------------------------------
  family_label <- paste0("Family: Negative Binomial / Link: ", model$family$link)

  # --- Ancillary parameters (Stata-style /lnalpha and alpha) ----------------
  # R's theta = 1/alpha (Stata convention).
  theta    <- model$theta
  se_theta <- model$SE.theta

  # lnalpha = -log(theta) = log(alpha)
  lnalpha    <- -log(theta)
  se_lnalpha <- se_theta / theta   # delta method: |d/dtheta(-log(theta))| = 1/theta

  # alpha = 1/theta = exp(lnalpha)
  alpha    <- 1 / theta
  se_alpha <- alpha * se_lnalpha   # delta method: d/d(lnalpha)(exp(lnalpha)) = alpha

  # CIs: Wald on the log scale, then exponentiate for alpha
  if (wide) {
    z_crit     <- stats::qnorm(0.5 + level / 200)
    lnalpha_lo <- lnalpha - z_crit * se_lnalpha
    lnalpha_hi <- lnalpha + z_crit * se_lnalpha
    alpha_lo   <- exp(lnalpha_lo)
    alpha_hi   <- exp(lnalpha_hi)
  } else {
    lnalpha_lo <- NA_real_
    lnalpha_hi <- NA_real_
    alpha_lo   <- NA_real_
    alpha_hi   <- NA_real_
  }

  ancillary_df <- data.frame(
    label    = c("/lnalpha", "alpha"),
    estimate = c(lnalpha, alpha),
    std_err  = c(se_lnalpha, se_alpha),
    ci_lower = c(lnalpha_lo, alpha_lo),
    ci_upper = c(lnalpha_hi, alpha_hi),
    stringsAsFactors = FALSE
  )

  # --- Coef data frame ------------------------------------------------------
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide, ref = opts$ref, label = opts$label)

  # --- Output ---------------------------------------------------------------
  new_tula_output(
    model_type   = "negbin",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = "z",
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3"),
    exp          = exp,
    exp_label    = if (isTRUE(exp)) "IRR" else NULL,
    ancillary_df = ancillary_df,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
}

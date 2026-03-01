#' @rdname tula
#' @export
tula.survreg <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                         width = NULL, exp = FALSE, level = 95,
                         robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  s     <- summary(model)
  n_obs <- stats::nobs(model)

  # --- Extract coefficient table (includes Log(scale) as last row) ----------
  # AER::tobit summary stores a "coeftest" object in s$coefficients.

  # survival::survreg summary stores the table in s$table.
  ct_full <- if ("coefficients" %in% names(s) &&
                 inherits(s[["coefficients"]], "coeftest")) {
    s[["coefficients"]]          # AER::tobit path
  } else {
    s[["table"]]                 # survival::survreg path
  }

  # Separate predictor rows from Log(scale) row (if present).

  # Exponential distribution has a fixed scale = 1 — no Log(scale) row.
  n_pred    <- length(stats::coef(model))
  has_scale <- nrow(ct_full) > n_pred
  ct        <- ct_full[seq_len(n_pred), , drop = FALSE]
  log_scale_row <- if (has_scale) ct_full[n_pred + 1L, ] else NULL

  # --- stat label (Stata tobit uses t) --------------------------------------
  stat_label <- "t"

  # --- Robust SE (standard pipeline; vcovHC fails for survreg but
  #     .resolve_robust_vcov() falls back to sandwich::sandwich()) -----------
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, stat_label, df = Inf)
    ci <- if (wide) .robust_ci(ct, level, stat_label, df = Inf) else NULL
  } else {
    # Wald CIs (fast, matches Stata default).  confint.default() on survreg
    # includes Log(scale) row — subset to predictor rows only.
    if (wide) {
      ci_full <- stats::confint.default(model, level = level / 100)
      ci <- ci_full[seq_len(n_pred), , drop = FALSE]
    } else {
      ci <- NULL
    }
  }

  # --- Log-likelihood and Pseudo R-sq ---------------------------------------
  ll      <- model$loglik[2L]      # fitted model
  ll_null <- model$loglik[1L]      # null (intercept-only) model
  pseudo_r2 <- if (!is.na(ll_null) && ll_null != 0) {
    1 - ll / ll_null
  } else {
    NA_real_
  }

  # --- Censoring counts -----------------------------------------------------
  cens_counts <- .survreg_cens_counts(model)

  # --- Header blocks --------------------------------------------------------
  header_left <- c(
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model),
    "Log likelihood" = ll
  )
  header_right <- c(
    "Number of obs" = n_obs,
    if (!is.null(robust_info$cluster_n))
      c("Num. clusters" = robust_info$cluster_n),
    cens_counts,
    "Pseudo R-sq"   = pseudo_r2
  )

  # --- Family label ---------------------------------------------------------
  family_label <- if (inherits(model, "tobit")) {
    "Tobit regression"
  } else {
    dist <- model$dist
    dist_label <- switch(dist,
      "gaussian"    = "Normal",
      "weibull"     = "Weibull",
      "exponential" = "Exponential",
      "logistic"    = "Logistic",
      "lognormal"   = "Log-normal",
      "loglogistic" = "Log-logistic",
      dist  # fallback: use dist name as-is
    )
    paste0("Survival regression / Distribution: ", dist_label)
  }

  # --- Ancillary parameter: /sigma ------------------------------------------
  # Exponential distribution has fixed scale = 1 (no estimated Log(scale)),
  # so ancillary_df is NULL in that case.
  if (has_scale) {
    sigma        <- model$scale
    se_log_scale <- log_scale_row[2L]
    log_scale_est <- log_scale_row[1L]

    # Delta-method SE for sigma: sigma × SE(log_scale)
    se_sigma <- sigma * se_log_scale

    # CI: Wald on log scale, then exponentiate (asymmetric, correct)
    if (wide) {
      z_crit      <- stats::qnorm(0.5 + level / 200)
      ci_sigma_lo <- exp(log_scale_est - z_crit * se_log_scale)
      ci_sigma_hi <- exp(log_scale_est + z_crit * se_log_scale)
    } else {
      ci_sigma_lo <- NA_real_
      ci_sigma_hi <- NA_real_
    }

    ancillary_df <- data.frame(
      label    = "/sigma",
      estimate = sigma,
      std_err  = se_sigma,
      ci_lower = ci_sigma_lo,
      ci_upper = ci_sigma_hi,
      stringsAsFactors = FALSE
    )
  } else {
    ancillary_df <- NULL
  }

  # --- Coef data frame ------------------------------------------------------
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                           ref = opts$ref, label = opts$label)

  # --- Dependent variable ---------------------------------------------------
  dep_var <- .survreg_dep_var(model)

  # --- Output ---------------------------------------------------------------
  out <- new_tula_output(
    model_type   = "survreg",
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
    ancillary_df = ancillary_df,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
  .attach_select(out, ...)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Extract censoring observation counts from a survreg/tobit model
#'
#' Returns a named numeric vector suitable for splicing into header_right.
#' Handles left, right, and interval (both-limits) censoring.
#' @noRd
.survreg_cens_counts <- function(model) {
  y <- model$y
  if (is.null(y)) return(NULL)

  surv_type <- attr(y, "type")

  if (surv_type %in% c("left", "right")) {
    # 2-column Surv: status = 1 means uncensored (exact event)
    status    <- y[, 2L]
    n_uncens  <- sum(status == 1L)
    n_cens    <- sum(status == 0L)
    cens_label <- if (surv_type == "left") "Left-censored" else "Right-censored"
    out <- c(n_uncens, n_cens)
    names(out) <- c("Uncensored", cens_label)
    return(out)
  }

  if (surv_type == "interval") {
    # 3-column Surv: status encoding:
    #   0 = right-censored, 1 = exact, 2 = left-censored, 3 = interval-censored
    status     <- y[, 3L]
    n_right    <- sum(status == 0L)
    n_exact    <- sum(status == 1L)
    n_left     <- sum(status == 2L)
    n_interval <- sum(status == 3L)

    out <- c("Uncensored" = n_exact)
    if (n_left     > 0L) out <- c(out, "Left-censored"     = n_left)
    if (n_right    > 0L) out <- c(out, "Right-censored"    = n_right)
    if (n_interval > 0L) out <- c(out, "Interval-censored" = n_interval)
    return(out)
  }

  NULL
}

#' Extract the dependent variable name from a survreg/tobit model
#'
#' For AER::tobit, the formula LHS returns a Surv() expression, not
#' the original variable name.  This helper recovers the bare name.
#' @noRd
.survreg_dep_var <- function(model) {
  # Try 1: For AER::tobit, the original formula is in model$call$formula
  dep <- tryCatch({
    fml <- eval(model$call$formula, envir = environment(stats::formula(model)))
    lhs <- fml[[2L]]
    d   <- deparse(lhs)
    # If it looks like Surv(...), extract the first argument
    m <- regmatches(d, regexec("^Surv\\(([^,)]+)", d))[[1L]]
    if (length(m) == 2L) trimws(m[2L]) else d
  }, error = function(e) NULL)

  if (!is.null(dep)) return(dep)

  # Try 2: Fallback — parse formula(model)[[2L]]
  tryCatch({
    lhs <- stats::formula(model)[[2L]]
    d   <- deparse(lhs)
    m <- regmatches(d, regexec("^Surv\\(([^,)]+)", d))[[1L]]
    if (length(m) == 2L) trimws(m[2L]) else d
  }, error = function(e) NULL)
}

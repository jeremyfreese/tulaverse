# tula.lmer() / tula.glmer() — Stata-inspired output for mixed-effects models
# fit with lme4 (classes "lmerMod" / "glmerMod").
#
# lme4 objects are S4 (merMod), so the usual `model$xlevels` / `model$call`
# accessors error. All model info needed by build_coef_df() is extracted
# explicitly via .mermod_coef_info() and passed in, mirroring tula.fixest().

# Extract the pieces build_coef_df() needs from an S4 merMod object.
.mermod_coef_info <- function(model) {
  mf <- stats::model.frame(model)           # returns model@frame (a data frame)
  tt <- attr(mf, "terms")
  list(
    assign_vec   = attr(stats::model.matrix(model), "assign"),
    term_labels  = attr(stats::terms(model), "term.labels"),
    data_classes = attr(tt, "dataClasses"),
    xlevels      = lapply(Filter(is.factor, mf), levels),
    model_frame  = mf
  )
}

# Build the random-effects parameters data frame from VarCorr().
# One group sub-header per grouping factor, indented sd()/corr() rows, then a
# residual sd() row (linear models only). estimate is the sd (variance terms)
# or the correlation (covariance terms); NA on sub-header rows.
# Stata writes the random-effects intercept term as "_cons" (shorter and more
# faithful than "(Intercept)", which also tends to truncate in narrow output).
.mermod_reparam <- function(x) if (identical(x, "(Intercept)")) "_cons" else x

.mermod_ranef_df <- function(model) {
  vc  <- as.data.frame(lme4::VarCorr(model))   # cols: grp, var1, var2, vcov, sdcor
  ng  <- lme4::ngrps(model)                    # named group counts

  labels   <- character(0)
  ests     <- numeric(0)
  indents  <- logical(0)

  grp_factors <- setdiff(unique(vc$grp), "Residual")
  for (g in grp_factors) {
    rows <- vc[vc$grp == g, , drop = FALSE]
    n_g  <- if (g %in% names(ng)) ng[[g]] else NA_integer_
    ghdr <- if (!is.na(n_g)) paste0(g, ": ", n_g, " groups") else paste0(g, ":")
    labels  <- c(labels, ghdr);  ests <- c(ests, NA_real_); indents <- c(indents, FALSE)
    for (i in seq_len(nrow(rows))) {
      r  <- rows[i, ]
      v1 <- .mermod_reparam(r$var1)
      lab <- if (is.na(r$var2)) {
        paste0("sd(", v1, ")")
      } else {
        paste0("corr(", v1, ",", .mermod_reparam(r$var2), ")")
      }
      labels  <- c(labels, lab); ests <- c(ests, r$sdcor); indents <- c(indents, TRUE)
    }
  }

  # Residual scale (linear models; absent for binomial/Poisson glmer).
  res <- vc[vc$grp == "Residual", , drop = FALSE]
  if (nrow(res) > 0L) {
    labels  <- c(labels, "sd(Residual)"); ests <- c(ests, res$sdcor[1L]); indents <- c(indents, FALSE)
  }

  data.frame(label = labels, estimate = ests, indent = indents,
             stringsAsFactors = FALSE)
}

#' @rdname tula
#' @export
tula.lmerMod <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                         width = NULL, exp = FALSE, level = 95,
                         robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  .tula_mermod(model, wide = wide, ref = ref, label = label, width = width,
               exp = exp, level = level, robust = robust, vcov = vcov,
               cluster = cluster, ...)
}

#' @rdname tula
#' @export
tula.glmerMod <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                          width = NULL, exp = FALSE, level = 95,
                          robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  .tula_mermod(model, wide = wide, ref = ref, label = label, width = width,
               exp = exp, level = level, robust = robust, vcov = vcov,
               cluster = cluster, ...)
}

# Shared implementation for lmer / glmer.
.tula_mermod <- function(model, wide, ref, label, width, exp, level,
                         robust, vcov, cluster, ...) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' is required for mixed-effects models.", call. = FALSE)
  }
  # Design-based clustering IS the random-effects structure; robust/cluster SEs
  # on top are not standard (Stata's mixed does not allow vce(cluster)).
  if (isTRUE(robust) || !is.null(vcov) || !is.null(cluster)) {
    warning("Mixed-effects model standard errors come from the random-effects ",
            "structure; the robust, vcov, and cluster arguments are ignored.",
            call. = FALSE)
  }

  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  is_lmm <- lme4::isLMM(model)
  fam    <- stats::family(model)

  # --- Fixed-effects coefficient table (Estimate, SE, z, p) -----------------
  ct_raw <- stats::coef(summary(model))
  est    <- ct_raw[, 1L]
  se     <- ct_raw[, 2L]
  zval   <- est / se
  # lme4 omits p-values for lmer; Stata's mixed reports z with a normal
  # approximation, so compute p from the normal distribution. glmer already
  # reports z / p on the same basis.
  pval   <- 2 * stats::pnorm(-abs(zval))
  ct <- cbind(Estimate = est, "Std. Error" = se, "z value" = zval, "Pr(>|z|)" = pval)
  rownames(ct) <- rownames(ct_raw)
  stat_label <- "z"

  # Wald CIs (fast; profile CIs are slow and not Stata's default).
  ci <- if (wide) {
    z_crit <- stats::qnorm(0.5 + level / 200)
    cbind(est - z_crit * se, est + z_crit * se)
  } else NULL

  # --- Header ---------------------------------------------------------------
  n_obs <- stats::nobs(model)
  ll    <- as.numeric(stats::logLik(model))
  ng    <- lme4::ngrps(model)

  header_left <- c(
    "Log likelihood" = ll,
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model)
  )
  # One "N groups" entry per grouping factor.
  grp_hdr <- stats::setNames(as.numeric(ng), paste0("N groups: ", names(ng)))
  header_right <- c("Number of obs" = n_obs, grp_hdr)

  family_label <- if (is_lmm) {
    paste0("Mixed-effects ", if (lme4::isREML(model)) "REML" else "ML",
           " regression")
  } else {
    paste0("Mixed-effects GLM / Family: ", fam$family, " / Link: ", fam$link)
  }

  # exp label for logit / log links (glmer).
  exp_label <- if (isTRUE(exp) && !is_lmm) {
    if (fam$link == "logit") "Odds Ratio"
    else if (fam$link == "log") "IRR"
    else NULL
  } else NULL

  # --- coef_df (explicit model info: merMod is S4) --------------------------
  info <- .mermod_coef_info(model)
  opts <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                           ref = opts$ref, label = opts$label,
                           assign_vec   = info$assign_vec,
                           term_labels  = info$term_labels,
                           data_classes = info$data_classes,
                           xlevels      = info$xlevels,
                           model_frame  = info$model_frame,
                           orig_data    = NULL)

  ranef_df <- .mermod_ranef_df(model)
  dep_var  <- tryCatch(deparse(stats::formula(model)[[2L]]),
                       error = function(e) NULL)

  out <- new_tula_output(
    model_type   = if (is_lmm) "lmer" else "glmer",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = stat_label,
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = c("Log likelihood" = "f3", AIC = "f3", BIC = "f3"),
    exp          = exp,
    dep_var      = dep_var,
    exp_label    = exp_label,
    level        = level,
    ranef_df     = ranef_df
  )
  .attach_select(out, ...)
}

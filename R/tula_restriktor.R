#' @rdname tula
#' @export
tula.restriktor <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                            width = NULL, exp = FALSE, level = 95,
                            robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  # Warn and ignore robust/vcov/cluster — restriktor handles its own SE types
  # via the se= argument at estimation time

  if (isTRUE(robust) || !is.null(vcov) || !is.null(cluster)) {
    warning(
      "Constrained model standard errors are computed by restriktor ",
      "(controlled via the se= argument at estimation time). ",
      "The robust, vcov, and cluster arguments are ignored.",
      call. = FALSE
    )
  }

  # The original (unrestricted) model
  orig <- model$model.org
  s    <- summary(model)
  ct   <- s$coefficients

  # Determine stat_label from column names
  stat_col_nm <- colnames(ct)[3L]
  is_boot     <- grepl("^Lower", stat_col_nm, ignore.case = TRUE)

  if (is_boot) {
    stop(
      "tula() does not yet support restriktor models fitted with bootstrap ",
      "standard errors (se = \"boot.*\"). Please refit with se = \"standard\" ",
      "or an HC type.",
      call. = FALSE
    )
  }

  stat_label <- if (grepl("^z", stat_col_nm, ignore.case = TRUE)) "z" else "t"

  # Observation count
  n_obs <- tryCatch(stats::nobs(orig), error = function(e) {
    tryCatch(nrow(stats::model.frame(orig)), error = function(e2) NA_integer_)
  })

  # Determine underlying model type and build headers/family_label
  is_glm <- inherits(orig, "glm") && !inherits(orig, "lm") ||
            (inherits(orig, "glm") && !is.null(orig$family))
  is_rlm <- inherits(orig, "rlm")

  if (is_glm) {
    fam <- orig$family$family
    lnk <- orig$family$link
    family_label <- paste0("Constrained / Family: ", fam, " / Link: ", lnk)

    ll <- tryCatch(as.numeric(stats::logLik(orig)), error = function(e) NA_real_)
    dev      <- orig$deviance
    null_dev <- orig$null.deviance
    mcfadden <- if (!is.null(dev) && !is.null(null_dev) && null_dev > 0)
      1 - dev / null_dev else NA_real_

    header_left <- c(
      if (!is.na(ll)) c("Log likelihood" = ll)
    )
    header_right <- c(
      "Number of obs" = n_obs,
      if (!is.na(mcfadden)) c("McFadden R-sq" = mcfadden)
    )
    value_fmts <- c("Log likelihood" = "f3")

    exp_label <- if (exp) {
      if (lnk == "logit") "Odds Ratio"
      else if (lnk == "log") "IRR"
      else NULL
    } else NULL

  } else {
    # lm or rlm path
    family_label <- if (is_rlm) "Constrained robust linear model" else
                      "Constrained linear model"

    r2_restr <- tryCatch(model$R2.reduced, error = function(e) NULL)
    r2_orig  <- tryCatch(model$R2.org, error = function(e) NULL)

    header_left <- c(
      if (!is.null(r2_orig))  c("R-sq (original)" = r2_orig)
    )
    header_right <- c(
      "Number of obs" = n_obs,
      if (!is.null(r2_restr)) c("R-sq (constrained)" = r2_restr)
    )
    value_fmts <- character(0L)
    exp_label  <- NULL
  }

  # Confidence intervals — compute Wald CIs manually (no confint method)
  df_resid <- tryCatch(model$df.residual, error = function(e) {
    tryCatch(stats::df.residual(orig), error = function(e2) Inf)
  })

  ci <- if (wide) {
    ests <- ct[, 1L]
    ses  <- ct[, 2L]
    crit <- if (stat_label == "t" && is.finite(df_resid) && df_resid > 0L) {
      stats::qt(0.5 + level / 200, df = df_resid)
    } else {
      stats::qnorm(0.5 + level / 200)
    }
    ci_mat <- cbind(ests - crit * ses, ests + crit * ses)
    colnames(ci_mat) <- c("2.5 %", "97.5 %")
    rownames(ci_mat) <- rownames(ct)
    ci_mat
  } else NULL

  # Build coef_df using the original model's infrastructure
  opts <- .parse_tula_opts(ref, label)

  # Extract model infrastructure from the original model
  mm         <- tryCatch(stats::model.matrix(orig), error = function(e) NULL)
  assign_vec <- if (!is.null(mm)) attr(mm, "assign") else NULL
  trm        <- tryCatch(stats::terms(orig), error = function(e) NULL)
  term_labels  <- if (!is.null(trm)) attr(trm, "term.labels") else NULL
  data_classes <- if (!is.null(trm)) attr(trm, "dataClasses") else NULL
  xlevels      <- orig$xlevels
  mf           <- tryCatch(stats::model.frame(orig), error = function(e) NULL)

  coef_df <- build_coef_df(
    model, ct, ci, wide,
    ref          = opts$ref,
    label        = opts$label,
    assign_vec   = assign_vec,
    term_labels  = term_labels,
    data_classes = data_classes,
    xlevels      = xlevels,
    model_frame  = mf
  )

  dep_var <- tryCatch(deparse(formula(orig)[[2L]]), error = function(e) NULL)

  # Number of constraints for display
  n_constraints <- tryCatch(nrow(model$constraints), error = function(e) NULL)
  if (!is.null(n_constraints) && n_constraints > 0L) {
    header_right <- c(header_right, "Num. constraints" = n_constraints)
  }

  out <- new_tula_output(
    model_type   = "restriktor",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = stat_label,
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = value_fmts,
    exp          = exp,
    dep_var      = dep_var,
    exp_label    = exp_label,
    level        = level,
    se_label     = NULL
  )
  .attach_select(out, ...)
}

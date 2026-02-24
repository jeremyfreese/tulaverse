#' @rdname tula
#' @export
tula.clm <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                     width = NULL, exp = FALSE, level = 95, ...) {
  level <- .resolve_level(level)
  # When exp = TRUE, suppress CIs (exponentiated CIs not yet supported)
  if (isTRUE(exp) && (isTRUE(wide) || is.null(wide))) {
    message("Note: wide output is not yet supported with exp = TRUE; CIs suppressed.")
    wide <- FALSE
  }
  wide <- .resolve_wide(wide, width)

  s <- summary(model)

  # summary(clm)$coefficients has 4 columns for ALL parameters:
  # "Estimate", "Std. Error", "z value", "Pr(>|z|)"
  # Rows: thresholds/cutpoints (model$alpha) then predictors (model$beta)
  ct_all <- s$coefficients

  alpha_names <- names(model$alpha)   # cutpoints, e.g. "1|2", "2|3"
  beta_names  <- names(model$beta)    # predictor coefficients

  # Standard 4-column predictor ct matrix
  ct      <- ct_all[beta_names,  , drop = FALSE]
  ct_zeta <- ct_all[alpha_names, , drop = FALSE]

  # Wald CIs via vcov (covers both predictors and cutpoints uniformly)
  if (wide) {
    v      <- stats::vcov(model)
    z_crit <- stats::qnorm(0.5 + level / 200)
    ses    <- sqrt(diag(v))

    all_est <- stats::coef(model)   # combined vector (alpha then beta)

    ci_pred <- cbind(
      "2.5 %"  = all_est[beta_names]  - z_crit * ses[beta_names],
      "97.5 %" = all_est[beta_names]  + z_crit * ses[beta_names]
    )
    rownames(ci_pred) <- beta_names

    ci_zeta <- cbind(
      "2.5 %"  = all_est[alpha_names] - z_crit * ses[alpha_names],
      "97.5 %" = all_est[alpha_names] + z_crit * ses[alpha_names]
    )
    rownames(ci_zeta) <- alpha_names
  } else {
    ci_pred <- NULL
    ci_zeta <- NULL
  }

  # Log-likelihood
  ll_fitted <- as.numeric(stats::logLik(model))

  # McFadden R²: LL_null from marginal proportions (no null model fitting needed)
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
    "McFadden R-sq" = pseudo_r2
  )

  # Extract design matrix info explicitly.
  # ordinal's model.matrix.clm() does NOT set the "assign" attribute, which
  # build_coef_df() needs to map coefficients to model terms for factor grouping.
  # Bypass S3 dispatch by calling model.matrix() with an explicit terms object and
  # data frame; this invokes model.matrix.default() which always sets assign.
  mf     <- tryCatch(model.frame(model), error = function(e) NULL)
  mm_raw <- tryCatch(
    model.matrix(terms(model), data = mf),
    error = function(e) NULL
  )
  assign_vec <- if (!is.null(mm_raw)) {
    av <- attr(mm_raw, "assign")
    if (!is.null(av)) av[av != 0L] else seq_along(beta_names)
  } else {
    seq_along(beta_names)
  }
  term_labels  <- attr(terms(model), "term.labels")
  data_classes <- tryCatch(
    attr(terms(model), "dataClasses"),
    error = function(e) character(0)
  )
  xlevels   <- if (!is.null(model$xlevels)) model$xlevels else list()

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

  # Append cutpoint rows
  cutpoint_rows <- .build_cutpoint_rows(ct_zeta, ci_zeta)
  coef_df <- rbind(coef_df, cutpoint_rows)

  # Link function label printed above the header block
  family_label <- paste0("Ordered regression / Link: ", model$link)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  new_tula_output(
    model_type   = "clm",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = "z",
    wide         = wide,
    family_label = family_label,
    width        = width,
    value_fmts   = c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3"),
    exp          = exp,
    dep_var      = dep_var,
    level        = level
  )
}

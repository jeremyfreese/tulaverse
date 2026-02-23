#' @rdname tula
#' @export
tula.glm <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                     width = NULL, exp = FALSE, ...) {
  # When exp = TRUE, suppress CIs (exponentiated CIs not yet supported)
  if (isTRUE(exp) && (isTRUE(wide) || is.null(wide))) {
    message("Note: wide output is not yet supported with exp = TRUE; CIs suppressed.")
    wide <- FALSE
  }
  wide   <- .resolve_wide(wide, width)
  s      <- summary(model)
  n_obs  <- stats::nobs(model)
  ll     <- as.numeric(stats::logLik(model))

  # Pseudo-R² measures
  dev     <- model$deviance
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

  # Left header block
  header_left <- c(
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model),
    "Log likelihood" = ll
  )

  # Right header block
  header_right <- c(
    "Number of obs"  = n_obs,
    "McFadden R-sq"  = mcfadden,
    "Nagelkerke R-sq" = nagelkerke
  )

  # Coefficient matrix
  ct <- stats::coef(s)

  # Use Wald CIs (confint.default) for speed; avoids slow profile likelihood
  # and matches Stata's default for logistic/Poisson regression
  ci <- if (wide) stats::confint.default(model) else NULL

  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide, ref = opts$ref, label = opts$label)

  # Determine stat label from the summary matrix column name
  # Gaussian glm uses t; all other families use z
  stat_col_nm <- colnames(ct)[3L]
  stat_label  <- if (grepl("^z", stat_col_nm, ignore.case = TRUE)) "z" else "t"

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  new_tula_output(
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
    dep_var      = dep_var
  )
}

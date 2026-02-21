#' @rdname tula
#' @export
tula.lm <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                    width = NULL, ...) {
  s      <- summary(model)
  n_obs  <- stats::nobs(model)
  r2     <- s$r.squared
  adj_r2 <- s$adj.r.squared

  # Left header block: model fit statistics
  header_left <- c(
    AIC = stats::AIC(model),
    BIC = stats::BIC(model)
  )

  # Right header block: sample statistics
  header_right <- c(
    "Number of obs" = n_obs,
    "R-squared"     = r2,
    "Adj R-squared" = adj_r2
  )

  # Coefficient matrix and optional CIs
  ct <- stats::coef(s)              # estimate, SE, t, p
  ci <- if (wide) stats::confint(model) else NULL

  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide, ref = opts$ref, label = opts$label)

  new_tula_output(
    model_type   = "lm",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = "t",
    wide         = wide,
    family_label = NULL,
    width    = width
  )
}

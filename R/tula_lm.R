#' @rdname tula
#' @export
tula.lm <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                    width = NULL, exp = FALSE, level = 95,
                    robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)
  s     <- summary(model)
  n_obs <- stats::nobs(model)
  r2    <- s$r.squared
  adj_r2 <- s$adj.r.squared

  # Coefficient matrix (estimate, SE, t, p)
  ct         <- stats::coef(s)
  stat_label <- "t"

  # Apply robust SE if requested
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, stat_label, df = df_resid)
    ci <- if (wide) .robust_ci(ct, level, stat_label, df = df_resid) else NULL
  } else {
    ci <- if (wide) stats::confint(model, level = level / 100) else NULL
  }

  # Header blocks
  header_left <- c(
    AIC = stats::AIC(model),
    BIC = stats::BIC(model)
  )
  header_right <- c(
    "Number of obs" = n_obs,
    if (!is.null(robust_info$cluster_n)) c("Num. clusters" = robust_info$cluster_n),
    "R-squared"     = r2,
    "Adj R-squared" = adj_r2,
    "Root MSE"      = s$sigma
  )

  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide, ref = opts$ref, label = opts$label)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  out <- new_tula_output(
    model_type   = "lm",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = stat_label,
    wide         = wide,
    family_label = NULL,
    width        = width,
    value_fmts   = c(AIC = "f3", BIC = "f3"),
    exp          = exp,
    dep_var      = dep_var,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
  )
  .attach_select(out, ...)
}

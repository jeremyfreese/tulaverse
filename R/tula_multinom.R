#' @rdname tula
#' @export
tula.multinom <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                          width = NULL, ...) {
  s   <- summary(model)
  mf  <- model.frame(model)
  n   <- nrow(mf)

  # Outcome levels: all levels, first is the base
  all_lev  <- model$lev          # e.g. c("4", "6", "8")
  base_lev <- all_lev[1L]        # e.g. "4"
  out_lev  <- all_lev[-1L]       # e.g. c("6", "8")  — one block each

  # Coefficient and SE matrices: rows = non-base outcomes, cols = predictors.
  # Ensure matrix form even when there is only one non-base outcome.
  coef_mat <- s$coefficients
  se_mat   <- s$standard.errors
  if (!is.matrix(coef_mat)) {
    coef_mat <- matrix(coef_mat, nrow = 1L,
                       dimnames = list(out_lev, names(coef_mat)))
    se_mat   <- matrix(se_mat,   nrow = 1L,
                       dimnames = list(out_lev, names(se_mat)))
  }

  # z-statistics and two-tailed p-values (multinom summary doesn't compute them)
  z_mat <- coef_mat / se_mat
  p_mat <- 2 * stats::pnorm(-abs(z_mat))

  # Confidence intervals: 3-D array [predictors x bounds x outcomes] or NULL
  ci_arr <- if (wide) tryCatch(confint(model), error = function(e) NULL) else NULL

  # Header metrics (same structure as glm)
  ll <- as.numeric(stats::logLik(model))

  # McFadden R² = 1 - LL_fitted / LL_null.
  # LL_null for a K-category outcome = N * log(1/K) (equal-probability baseline)
  k        <- length(all_lev)
  ll_null  <- n * log(1 / k)
  mcfadden <- 1 - ll / ll_null

  header_left <- c(
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model),
    "Log likelihood" = ll
  )
  header_right <- c(
    "Number of obs" = n,
    "McFadden R-sq" = mcfadden
  )

  # assign_vec, term_labels, data_classes, xlevels — extracted once and reused
  # across all outcome blocks (same design matrix for every outcome).
  mm          <- model.matrix(model)
  assign_vec  <- attr(mm, "assign")
  term_labels <- attr(terms(model), "term.labels")
  data_classes <- tryCatch(
    attr(terms(model), "dataClasses"),
    error = function(e) character(0)
  )
  xlevels <- if (!is.null(model$xlevels)) model$xlevels else list()

  opts <- .parse_tula_opts(ref, label)

  # Build one coef_df per non-base outcome level
  blocks <- lapply(out_lev, function(lv) {
    # ct: 4-column matrix with conventional column names, one row per predictor
    ct_lv <- cbind(
      "Estimate"   = coef_mat[lv, ],
      "Std. Error" = se_mat[lv, ],
      "z value"    = z_mat[lv, ],
      "Pr(>|z|)"   = p_mat[lv, ]
    )

    # ci: 2-column matrix for this outcome level, or NULL
    ci_lv <- if (!is.null(ci_arr)) ci_arr[ , , lv, drop = FALSE] |>
                matrix(ncol = 2L,
                       dimnames = list(rownames(ct_lv), c("2.5 %", "97.5 %")))
              else NULL

    coef_df <- build_coef_df(
      model        = model,
      ct           = ct_lv,
      ci           = ci_lv,
      wide         = wide,
      ref          = opts$ref,
      label        = opts$label,
      assign_vec   = assign_vec,
      term_labels  = term_labels,
      data_classes = data_classes,
      xlevels      = xlevels,
      model_frame  = mf
    )

    list(outcome = lv, coef_df = coef_df)
  })

  new_tula_multinom_output(
    header_left  = header_left,
    header_right = header_right,
    blocks       = blocks,
    base_outcome = base_lev,
    stat_label   = "z",
    wide         = wide,
    width        = width
  )
}

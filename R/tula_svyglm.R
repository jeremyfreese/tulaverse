#' @rdname tula
#' @export
tula.svyglm <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                        width = NULL, exp = FALSE, level = 95,
                        robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  # Survey SEs are already design-based (sandwich-type); additional robust
  # adjustment is neither needed nor supported (Stata also disallows
  # vce(robust) with the svy: prefix).
  if (isTRUE(robust) || !is.null(vcov) || !is.null(cluster)) {
    warning(
      "Survey model standard errors already incorporate design-based ",
      "(sandwich-type) variance estimation. ",
      "The robust, vcov, and cluster arguments are ignored.",
      call. = FALSE
    )
  }

  s     <- summary(model)
  n_obs <- stats::nobs(model)

  # --- Coefficient table (already has design-based SEs) ---------------------
  ct <- stats::coef(s)

  # Survey estimation always uses t-statistics with design df, regardless of
  # family — matching Stata's svy: prefix, which always reports t (not z).
  stat_label <- "t"

  # --- Confidence intervals (survey-adjusted via confint.svyglm) ------------
  # confint.svyglm() uses t-distribution with design-adjusted df.
  ci <- if (wide) {
    ci_raw <- stats::confint(model, level = level / 100)
    # confint.svyglm returns a matrix; ensure it is one
    if (!is.matrix(ci_raw)) as.matrix(ci_raw) else ci_raw
  } else {
    NULL
  }

  # --- Survey design info ---------------------------------------------------
  dinfo <- .svyglm_design_info(model)

  # --- Family label (prefixed with "Survey:") --------------------------------
  fam <- model$family$family
  lnk <- model$family$link
  family_label <- paste0("Survey: Family: ", fam, " / Link: ", lnk)

  # --- Header blocks ---------------------------------------------------------
  # Left: R-squared for gaussian only (matching Stata svy: regress)
  header_left <- numeric(0L)
  if (fam == "gaussian") {
    r_sq <- tryCatch(summary(model)$r.squared, error = function(e) NULL)
    if (!is.null(r_sq) && !is.na(r_sq)) {
      header_left <- c("R-squared" = r_sq)
    }
  }

  # Right: all counts and design info
  header_right <- c("Number of obs" = n_obs)
  if (!is.null(dinfo$pop_n))
    header_right <- c(header_right, "Population N" = dinfo$pop_n)
  if (!is.null(dinfo$n_strata))
    header_right <- c(header_right, "Num. strata" = dinfo$n_strata)
  if (!is.null(dinfo$n_psu))
    header_right <- c(header_right, "Num. PSUs" = dinfo$n_psu)
  if (!is.null(dinfo$design_df))
    header_right <- c(header_right, "Design df" = dinfo$design_df)

  # --- Coef data frame -------------------------------------------------------
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                            ref = opts$ref, label = opts$label)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  # --- exp_label for specific families ---------------------------------------
  exp_label <- NULL
  if (isTRUE(exp)) {
    if (fam %in% c("binomial", "quasibinomial") && lnk == "logit") {
      exp_label <- "Odds Ratio"
    } else if (fam %in% c("poisson", "quasipoisson")) {
      exp_label <- "IRR"
    }
  }

  # --- Output ----------------------------------------------------------------
  new_tula_output(
    model_type   = "svyglm",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = stat_label,
    wide         = wide,
    family_label = family_label,
    width        = width,
    exp          = exp,
    dep_var      = dep_var,
    exp_label    = exp_label,
    level        = level,
    se_label     = NULL,
    se_super     = "Linearized"
  )
}


# Internal: extract survey design metadata from a svyglm model object.
#
# Returns a list with n_strata, n_psu, pop_n, design_df.
# Fields are NULL when the information cannot be extracted (e.g. replicate
# weight designs that lack explicit strata/cluster structure).
.svyglm_design_info <- function(model) {
  design <- model$survey.design
  if (is.null(design)) return(list(n_strata = NULL, n_psu = NULL,
                                   pop_n = NULL, design_df = NULL))

  # Design degrees of freedom via survey::degf()
  design_df <- tryCatch({
    if (requireNamespace("survey", quietly = TRUE))
      survey::degf(design)
    else
      NULL
  }, error = function(e) NULL)

  # Population N = sum of sampling weights
  pop_n <- tryCatch(
    as.integer(round(sum(stats::weights(design)))),
    error = function(e) NULL
  )

  # Number of strata and PSUs (available for survey.design2 objects)
  n_strata <- tryCatch({
    strat <- design$strata
    if (!is.null(strat) && NCOL(strat) >= 1L)
      length(unique(strat[[1L]]))
    else
      NULL
  }, error = function(e) NULL)

  n_psu <- tryCatch({
    clus <- design$cluster
    if (!is.null(clus) && NCOL(clus) >= 1L)
      length(unique(clus[[1L]]))
    else
      NULL
  }, error = function(e) NULL)

  list(n_strata = n_strata, n_psu = n_psu, pop_n = pop_n,
       design_df = design_df)
}

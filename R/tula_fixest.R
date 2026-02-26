#' @rdname tula
#' @export
tula.fixest <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                        width = NULL, exp = FALSE, level = 95,
                        robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  # --- Guard: fixest_multi not supported ------------------------------------
  .fixest_guard_multi(model)

  method <- model$method  # "feols", "feglm", "fepois", "fenegbin"

  # --- Resolve vcov ---------------------------------------------------------
  vcov_info <- .fixest_resolve_vcov(model, robust, vcov, cluster)
  vcov_arg  <- vcov_info$vcov_arg

  # --- Coefficient table ----------------------------------------------------
  # Pass vcov to summary() so SEs reflect the requested vcov
  if (is.null(vcov_arg)) {
    s <- summary(model)
  } else {
    s <- summary(model, vcov = vcov_arg)
  }
  ct <- s$coeftable  # matrix: Estimate, Std. Error, t/z value, Pr(>|t/z|)

  # --- fenegbin: remove .theta row from ct ----------------------------------
  theta_row <- NULL
  if (method == "fenegbin" && ".theta" %in% rownames(ct)) {
    theta_row <- ct[".theta", , drop = FALSE]
    ct <- ct[rownames(ct) != ".theta", , drop = FALSE]
  }

  # --- stat label -----------------------------------------------------------
  stat_label <- .fixest_stat_label(ct)

  # --- Confidence intervals -------------------------------------------------
  if (wide) {
    if (is.null(vcov_arg)) {
      ci <- as.matrix(stats::confint(model, level = level / 100))
    } else {
      ci <- as.matrix(stats::confint(model, vcov = vcov_arg,
                                     level = level / 100))
    }
    # Remove .theta row from CI if present
    if (method == "fenegbin" && ".theta" %in% rownames(ci)) {
      ci <- ci[rownames(ci) != ".theta", , drop = FALSE]
    }
  } else {
    ci <- NULL
  }

  # --- Parse coefficient names → assign_vec, term_labels, etc. ---------------
  parsed <- .fixest_parse_coefnames(ct, model)

  # --- Headers & family_label -----------------------------------------------
  hdr <- .fixest_headers(model, s, method, vcov_info)
  header_left  <- hdr$header_left
  header_right <- hdr$header_right
  family_label <- hdr$family_label
  value_fmts   <- hdr$value_fmts

  # --- SE label -------------------------------------------------------------
  se_label <- vcov_info$se_label

  # --- Ancillary (fenegbin only) --------------------------------------------
  ancillary_df <- .fixest_ancillary(model, method, theta_row, wide, level)

  # --- exp_label ------------------------------------------------------------
  exp_label <- NULL
  if (isTRUE(exp)) {
    if (method %in% c("fepois", "fenegbin")) {
      exp_label <- "IRR"
    } else if (method == "feglm" && !is.null(model$family) &&
               model$family$link == "logit") {
      exp_label <- "Odds Ratio"
    }
  }

  # --- Dependent variable ---------------------------------------------------
  dep_var <- tryCatch(deparse(model$fml[[2L]]), error = function(e) NULL)

  # --- Rename i() coefficient names for build_coef_df() ---------------------
  # fixest i() coefficients use "var::level" naming (e.g. "cyl::6").
  # build_coef_df() computes the level suffix by stripping the term name
  # from the coefficient name: substring("cyl::6", nchar("cyl") + 1) = "::6".
  # We need "cyl6" → suffix "6". So rename "var::level" → "varlevel" in
  # ct and ci rownames.
  rename_map <- parsed$rename_map  # named character: old_name → new_name
  if (length(rename_map) > 0L) {
    old_rn <- rownames(ct)
    for (j in seq_along(old_rn)) {
      if (old_rn[j] %in% names(rename_map)) {
        old_rn[j] <- rename_map[old_rn[j]]
      }
    }
    rownames(ct) <- old_rn

    if (!is.null(ci)) {
      old_ci_rn <- rownames(ci)
      for (j in seq_along(old_ci_rn)) {
        if (old_ci_rn[j] %in% names(rename_map)) {
          old_ci_rn[j] <- rename_map[old_ci_rn[j]]
        }
      }
      rownames(ci) <- old_ci_rn
    }
  }

  # --- Coef data frame ------------------------------------------------------
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                           ref        = opts$ref,
                           label      = opts$label,
                           assign_vec = parsed$assign_vec,
                           term_labels = parsed$term_labels,
                           data_classes = parsed$data_classes,
                           xlevels    = parsed$xlevels)

  # --- Output ---------------------------------------------------------------
  new_tula_output(
    model_type   = "fixest",
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
    ancillary_df = ancillary_df,
    level        = level,
    se_label     = se_label
  )
}


#' @rdname tula
#' @export
tula.fixest_multi <- function(model, ...) {
  stop("tula() does not support fixest_multi objects. ",
       "Pass a single fixest model (e.g. one element of the list).",
       call. = FALSE)
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Guard against fixest_multi objects (belt-and-suspenders check)
#' @noRd
.fixest_guard_multi <- function(model) {
  if (inherits(model, "fixest_multi")) {
    stop("tula() does not support fixest_multi objects. ",
         "Pass a single fixest model (e.g. one element of the list).",
         call. = FALSE)
  }
}

#' Determine stat label from coefficient table column names
#' @noRd
.fixest_stat_label <- function(ct) {
  col3 <- colnames(ct)[3L]
  if (grepl("z", col3, ignore.case = TRUE)) "z" else "t"
}


# ---------------------------------------------------------------------------
# Vcov resolution
# ---------------------------------------------------------------------------

#' Translate tula's robust/vcov/cluster args to fixest vcov format
#'
#' @return list(vcov_arg, se_label, cluster_n)
#' @noRd
.fixest_resolve_vcov <- function(model, robust, vcov_arg, cluster_arg) {
  se_label  <- NULL
  cluster_n <- NULL

  # cluster implies robust
  if (!is.null(cluster_arg)) {
    # cluster_arg is a character string (variable name)
    vcov_resolved <- stats::as.formula(paste0("~", cluster_arg))
    se_label <- "Cluster SE"

    # Compute cluster count from original data
    cluster_n <- tryCatch({
      d <- eval(model$call$data, envir = model$call_env)
      if (!is.null(d) && cluster_arg %in% names(d)) {
        length(unique(d[[cluster_arg]]))
      } else {
        NA_integer_
      }
    }, error = function(e) NA_integer_)

    return(list(vcov_arg = vcov_resolved, se_label = se_label,
                cluster_n = cluster_n))
  }

  if (!is.null(vcov_arg)) {
    if (is.matrix(vcov_arg)) {
      stop("Pre-computed vcov matrices are not supported for fixest models. ",
           "Use fixest's native vcov options instead.",
           call. = FALSE)
    }
    if (is.character(vcov_arg)) {
      # Translate HC types: fixest uses "hetero" for heteroskedasticity-robust
      if (grepl("^HC", vcov_arg, ignore.case = TRUE)) {
        if (!grepl("^HC1$", vcov_arg, ignore.case = TRUE)) {
          warning("fixest only supports HC1 heteroskedasticity-robust SEs. ",
                  "Using 'hetero' (HC1) instead of '", vcov_arg, "'.",
                  call. = FALSE)
        }
        vcov_resolved <- "hetero"
        se_label <- "Robust SE"
      } else {
        # Pass through other fixest vcov types (e.g. "twoway", "NW", "DK")
        vcov_resolved <- vcov_arg
        se_label <- paste0(vcov_arg, " SE")
      }
      return(list(vcov_arg = vcov_resolved, se_label = se_label,
                  cluster_n = NULL))
    }
  }

  if (isTRUE(robust)) {
    return(list(vcov_arg = "hetero", se_label = "Robust SE",
                cluster_n = NULL))
  }

  # Default: use fixest's own default (NULL)
  list(vcov_arg = NULL, se_label = NULL, cluster_n = NULL)
}


# ---------------------------------------------------------------------------
# Coefficient name parsing → assign_vec, term_labels, data_classes, xlevels
# ---------------------------------------------------------------------------

#' Parse fixest coefficient names to produce factor grouping info
#'
#' Handles three patterns:
#' - i() variables: "var::level" (e.g. "cyl::6", "cyl::8")
#' - factor() variables: "factor(var)level" (e.g. "factor(cyl)6")
#' - plain variables: "wt", "hp"
#' - intercept: "(Intercept)"
#'
#' @return list(assign_vec, term_labels, data_classes, xlevels)
#' @noRd
.fixest_parse_coefnames <- function(ct, model) {
  coef_names  <- rownames(ct)
  n           <- length(coef_names)
  assign_vec  <- integer(n)
  term_labels <- character(0)
  data_classes <- character(0)
  xlevels     <- list()
  term_idx    <- 0L
  rename_map  <- character(0)  # maps "cyl::6" → "cyl6" for build_coef_df()

  # Track which terms we've already seen (maps term_name → term_idx)
  seen_terms <- list()

  for (i in seq_len(n)) {
    nm <- coef_names[i]

    # --- Intercept ---
    if (nm == "(Intercept)") {
      assign_vec[i] <- 0L
      next
    }

    # --- i() pattern: "var::level" ---
    m_i <- regmatches(nm, regexec("^(.+)::(.+)$", nm))[[1L]]
    if (length(m_i) == 3L) {
      term_nm <- m_i[2L]
      lv      <- m_i[3L]

      if (is.null(seen_terms[[term_nm]])) {
        term_idx <- term_idx + 1L
        seen_terms[[term_nm]] <- term_idx
        term_labels <- c(term_labels, term_nm)
        data_classes <- c(data_classes, stats::setNames("factor", term_nm))
        xlevels[[term_nm]] <- character(0)
      }
      assign_vec[i] <- seen_terms[[term_nm]]
      xlevels[[term_nm]] <- c(xlevels[[term_nm]], lv)

      # build_coef_df() computes level suffix as substring(nm, nchar(term) + 1),
      # so "cyl::6" → "::6". We rename to "cyl6" so the suffix is just "6".
      new_nm <- paste0(term_nm, lv)
      rename_map <- c(rename_map, stats::setNames(new_nm, nm))
      next
    }

    # --- factor() pattern: "factor(var)level" ---
    m_f <- regmatches(nm, regexec("^factor[(]([^)]+)[)](.+)$", nm))[[1L]]
    if (length(m_f) == 3L) {
      term_nm <- paste0("factor(", m_f[2L], ")")
      lv      <- m_f[3L]

      if (is.null(seen_terms[[term_nm]])) {
        term_idx <- term_idx + 1L
        seen_terms[[term_nm]] <- term_idx
        term_labels <- c(term_labels, term_nm)
        data_classes <- c(data_classes, stats::setNames("factor", term_nm))
        xlevels[[term_nm]] <- character(0)
      }
      assign_vec[i] <- seen_terms[[term_nm]]
      xlevels[[term_nm]] <- c(xlevels[[term_nm]], lv)
      next
    }

    # --- Plain continuous / interaction term ---
    term_idx <- term_idx + 1L
    seen_terms[[nm]] <- term_idx
    term_labels <- c(term_labels, nm)
    data_classes <- c(data_classes, stats::setNames("numeric", nm))
    assign_vec[i] <- term_idx
  }

  # --- Add reference levels for i() variables (from model_matrix_info) ---
  if (!is.null(model$model_matrix_info)) {
    for (info in model$model_matrix_info) {
      f_name <- info$f_name
      ref_val <- info$ref
      if (!is.null(f_name) && !is.null(ref_val) && f_name %in% names(xlevels)) {
        # Prepend the reference level (R convention: first element is ref)
        xlevels[[f_name]] <- c(as.character(ref_val), xlevels[[f_name]])
      }
    }
  }

  # --- For factor() terms, try to detect reference level from the data ---
  for (term_nm in names(xlevels)) {
    if (grepl("^factor[(]", term_nm)) {
      # Already has ref from model_matrix_info? Skip if so.
      # For factor(), ref is typically the first level. Try to get from data.
      bare_nm <- sub("^factor[(]([^)]+)[)]$", "\\1", term_nm)
      ref_val <- tryCatch({
        d <- eval(model$call$data, envir = model$call_env)
        if (!is.null(d) && bare_nm %in% names(d)) {
          v <- d[[bare_nm]]
          if (is.factor(v)) {
            levels(v)[1L]
          } else {
            as.character(sort(unique(v))[1L])
          }
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(ref_val) && !(ref_val %in% xlevels[[term_nm]])) {
        xlevels[[term_nm]] <- c(ref_val, xlevels[[term_nm]])
      }
    }
  }

  list(
    assign_vec   = assign_vec,
    term_labels  = term_labels,
    data_classes = data_classes,
    xlevels      = xlevels,
    rename_map   = rename_map
  )
}


# ---------------------------------------------------------------------------
# Header construction
# ---------------------------------------------------------------------------

#' Build header blocks and family_label for fixest models
#'
#' @return list(header_left, header_right, family_label, value_fmts)
#' @noRd
.fixest_headers <- function(model, s, method, vcov_info) {
  n_obs <- model$nobs

  # --- Fixed effects suffix for family_label --------------------------------
  fe_suffix <- ""
  if (!is.null(model$fixef_vars) && length(model$fixef_vars) > 0L) {
    fe_sizes <- model$fixef_sizes
    fe_parts <- paste0(model$fixef_vars, " (", fe_sizes, ")")
    fe_suffix <- paste0(" / Fixed effects: ", paste(fe_parts, collapse = ", "))
  }

  # --- Branch on method -----------------------------------------------------
  if (method == "feols") {
    # OLS / fixed effects
    family_label <- paste0("OLS", fe_suffix)

    r2_val  <- unname(fixest::r2(model, "r2"))
    ar2_val <- unname(fixest::r2(model, "ar2"))

    # Within R-sq on left (only with FE); R-sq stats on right
    header_left <- numeric(0)
    if (length(model$fixef_vars) > 0L) {
      wr2_val <- unname(fixest::r2(model, "wr2"))
      header_left <- c("Within R-sq" = wr2_val)
    }

    header_right <- c("Number of obs" = n_obs,
                      "R-squared" = r2_val,
                      "Adj. R-squared" = ar2_val)
    value_fmts   <- character(0L)

  } else if (method == "fepois") {
    family_label <- paste0("Poisson regression", fe_suffix)

    ll  <- as.numeric(stats::logLik(model))
    pr2  <- unname(fixest::r2(model, "pr2"))
    apr2 <- unname(fixest::r2(model, "apr2"))
    header_left <- c(
      AIC              = stats::AIC(model),
      BIC              = stats::BIC(model),
      "Log likelihood" = ll
    )
    header_right <- c("Number of obs" = n_obs,
                      "Pseudo R-sq" = pr2,
                      "Adj. Pseudo R-sq" = apr2)
    value_fmts   <- c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3")

  } else if (method == "fenegbin") {
    family_label <- paste0("Family: Negative Binomial / Link: log", fe_suffix)

    ll   <- as.numeric(stats::logLik(model))
    pr2  <- unname(fixest::r2(model, "pr2"))
    apr2 <- unname(fixest::r2(model, "apr2"))
    header_left <- c(
      AIC              = stats::AIC(model),
      BIC              = stats::BIC(model),
      "Log likelihood" = ll
    )
    header_right <- c("Number of obs" = n_obs,
                      "Pseudo R-sq" = pr2,
                      "Adj. Pseudo R-sq" = apr2)
    value_fmts   <- c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3")

  } else {
    # feglm (logit, probit, etc.)
    fam <- if (!is.null(model$family)) model$family$family else "unknown"
    lnk <- if (!is.null(model$family)) model$family$link else "unknown"
    family_label <- paste0("Family: ", fam, " / Link: ", lnk, fe_suffix)

    ll   <- as.numeric(stats::logLik(model))
    pr2  <- unname(fixest::r2(model, "pr2"))
    apr2 <- unname(fixest::r2(model, "apr2"))
    header_left <- c(
      AIC              = stats::AIC(model),
      BIC              = stats::BIC(model),
      "Log likelihood" = ll
    )
    header_right <- c("Number of obs" = n_obs,
                      "Pseudo R-sq" = pr2,
                      "Adj. Pseudo R-sq" = apr2)
    value_fmts   <- c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3")
  }

  # --- Add cluster count if applicable --------------------------------------
  if (!is.null(vcov_info$cluster_n) && !is.na(vcov_info$cluster_n)) {
    header_right <- c(header_right, "Num. clusters" = vcov_info$cluster_n)
  }

  list(
    header_left  = header_left,
    header_right = header_right,
    family_label = family_label,
    value_fmts   = value_fmts
  )
}


# ---------------------------------------------------------------------------
# Ancillary parameters (fenegbin only)
# ---------------------------------------------------------------------------

#' Build ancillary_df for fenegbin models
#'
#' @param theta_row One-row matrix extracted from coeftable (the .theta row),
#'   or NULL if not fenegbin.
#' @return data.frame or NULL
#' @noRd
.fixest_ancillary <- function(model, method, theta_row, wide, level) {
  if (method != "fenegbin") return(NULL)
  if (is.null(theta_row)) return(NULL)

  theta    <- theta_row[1L, 1L]  # Estimate
  se_theta <- theta_row[1L, 2L]  # Std. Error

  # Guard: if theta or SE is NA or non-positive, skip ancillary

  if (is.na(theta) || theta <= 0 || is.na(se_theta)) return(NULL)

  # lnalpha = -log(theta) = log(alpha)
  lnalpha    <- -log(theta)
  se_lnalpha <- se_theta / theta  # delta method: |d/dtheta(-log(theta))| = 1/theta

  # alpha = 1/theta = exp(lnalpha)
  alpha    <- 1 / theta
  se_alpha <- alpha * se_lnalpha  # delta method

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

  data.frame(
    label    = c("/lnalpha", "alpha"),
    estimate = c(lnalpha, alpha),
    std_err  = c(se_lnalpha, se_alpha),
    ci_lower = c(lnalpha_lo, alpha_lo),
    ci_upper = c(lnalpha_hi, alpha_hi),
    stringsAsFactors = FALSE
  )
}

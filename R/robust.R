# ---------------------------------------------------------------------------
# Robust (HC / cluster-robust) standard error helpers for tula()
#
# These internal helpers are called by individual tula.*() methods to apply
# sandwich-based robust variance-covariance matrices before build_coef_df().
#
# Requires the 'sandwich' package (listed in Suggests, not Imports).
# All three functions are internal (no roxygen export).
# ---------------------------------------------------------------------------


# Internal: resolve a robust vcov matrix and the SE column header label.
#
# Returns NULL if no robust adjustment is requested, or a list with:
#   $vcov_mat  - robust variance-covariance matrix
#   $se_label  - character: "Robust SE" (for both HC and cluster-robust)
#   $cluster_n - integer or NULL: number of unique clusters (cluster-robust only)
#
# Parameters:
#   model       - fitted model object
#   robust      - logical: if TRUE, use HC3 (default type)
#   vcov_arg    - NULL, a character HC type like "HC3" / "HC4", or a
#                 pre-computed numeric matrix (used as-is)
#   cluster_arg - NULL or a character variable name for cluster-robust SEs;
#                 presence of cluster_arg implies robust = TRUE regardless of
#                 the robust argument
.resolve_robust_vcov <- function(model, robust, vcov_arg, cluster_arg) {
  needs_robust <- isTRUE(robust) || !is.null(vcov_arg) || !is.null(cluster_arg)
  if (!needs_robust) return(NULL)

  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop(
      "Package 'sandwich' is required for robust standard errors.\n",
      "Install it with: install.packages(\"sandwich\")",
      call. = FALSE
    )
  }

  # Pre-computed vcov matrix supplied by the user --------------------------------
  if (is.matrix(vcov_arg)) {
    return(list(vcov_mat = vcov_arg, se_label = "Robust SE", cluster_n = NULL))
  }

  # Cluster-robust SEs via sandwich::vcovCL() ------------------------------------
  if (!is.null(cluster_arg)) {
    # Lookup the cluster variable: model.frame first, then original data
    cluster_vec <- tryCatch({
      mf <- stats::model.frame(model)
      if (cluster_arg %in% names(mf)) mf[[cluster_arg]] else NULL
    }, error = function(e) NULL)

    if (is.null(cluster_vec)) {
      cluster_vec <- tryCatch({
        od <- eval(model$call$data,
                   envir = environment(stats::formula(model)))
        if (!is.null(od) && cluster_arg %in% names(od)) {
          cv <- od[[cluster_arg]]
          # The raw data includes rows the model dropped for missingness.
          # Subset to the estimation sample using na.action (na.omit /
          # na.exclude record the dropped row indices) so the cluster vector
          # aligns with the model's residuals rather than the full data.
          na_act <- model$na.action
          if (!is.null(na_act) && length(na_act) > 0L) {
            cv <- cv[-as.integer(na_act)]
          }
          cv
        } else NULL
      }, error = function(e) NULL)
    }

    if (is.null(cluster_vec)) {
      stop(
        "cluster variable '", cluster_arg,
        "' not found in the model frame or data.",
        call. = FALSE
      )
    }

    # Guard against a cluster vector that doesn't match the estimation sample
    # (e.g. rows dropped for missingness that na.action didn't record). Erroring
    # here is clearer than letting sandwich::vcovCL() misalign silently.
    n_used <- tryCatch(stats::nobs(model), error = function(e) NA_integer_)
    if (!is.na(n_used) && length(cluster_vec) != n_used) {
      stop(
        "cluster variable '", cluster_arg, "' has length ",
        length(cluster_vec), " but the model used ", n_used,
        " observations.\nIt could not be aligned to the model's estimation ",
        "sample; supply a cluster vector matching the rows the model used.",
        call. = FALSE
      )
    }

    vcov_mat <- tryCatch(
      sandwich::vcovCL(model, cluster = cluster_vec),
      error = function(e) {
        stop(
          "sandwich::vcovCL() does not support models of class '",
          paste(class(model), collapse = "', '"), "'.\n",
          "Cluster-robust SEs are not available for this model type.",
          call. = FALSE
        )
      }
    )

    cluster_n <- length(unique(cluster_vec[!is.na(cluster_vec)]))
    return(list(vcov_mat  = vcov_mat,
                se_label  = "Robust SE",
                cluster_n = cluster_n))
  }

  # Heteroskedasticity-robust SEs via sandwich::vcovHC() -------------------------
  # HC corrections are only meaningful for OLS-like models; other model classes
  # (e.g., coxph, polr) are not supported by vcovHC.  When the user requests
  # generic robust = TRUE (no specific HC type), fall back to the generic
  # sandwich::sandwich() estimator, which covers a wider range of model classes.
  # When the user explicitly named an HC type (e.g., vcov = "HC4"), the error
  # is propagated so they know their specific request cannot be fulfilled.
  hc_type      <- if (is.character(vcov_arg) && nzchar(vcov_arg)) vcov_arg else "HC3"
  user_hc_type <- is.character(vcov_arg) && nzchar(vcov_arg)

  vcov_mat <- tryCatch(
    sandwich::vcovHC(model, type = hc_type),
    error = function(e) {
      if (user_hc_type) {
        # User requested a specific HC type — propagate the error clearly.
        stop(
          "sandwich::vcovHC() does not support models of class '",
          paste(class(model), collapse = "', '"), "'.\n",
          "Robust SEs are not available for this model type.",
          call. = FALSE
        )
      }
      # Generic robust = TRUE: fall back to sandwich::sandwich() for model
      # classes (e.g., coxph) where HC corrections do not apply.
      tryCatch(
        sandwich::sandwich(model),
        error = function(e2) {
          stop(
            "Robust SEs are not available for models of class '",
            paste(class(model), collapse = "', '"), "'.\n",
            "Neither sandwich::vcovHC() nor sandwich::sandwich() succeeded.",
            call. = FALSE
          )
        }
      )
    }
  )

  list(vcov_mat = vcov_mat, se_label = "Robust SE", cluster_n = NULL)
}


# Internal: recompute SE, test statistic, and p-value columns in ct from a
# robust variance-covariance matrix.
#
# rownames(ct) must be a subset of the row/column names of vcov_mat (used for
# indexing). The estimate column (column 1) is unchanged.
#
# stat_label : "t" uses pt() with df degrees of freedom; "z" uses pnorm().
# df         : residual degrees of freedom; use Inf for asymptotic (z) tests.
#
# Returns a modified copy of ct with columns 2 (SE), 3 (stat), 4 (p) updated.
.recompute_ct_robust <- function(ct, vcov_mat, stat_label, df = Inf) {
  coef_nms  <- rownames(ct)
  ses       <- sqrt(diag(vcov_mat)[coef_nms])
  ests      <- ct[, 1L]
  test_stat <- ests / ses

  p_vals <- if (stat_label == "t" && is.finite(df) && df > 0L) {
    2 * stats::pt(-abs(test_stat), df = df)
  } else {
    2 * stats::pnorm(-abs(test_stat))
  }

  ct_rob       <- ct
  ct_rob[, 2L] <- ses
  ct_rob[, 3L] <- test_stat
  ct_rob[, 4L] <- p_vals
  ct_rob
}


# Internal: compute Wald confidence intervals from the SE column of ct.
#
# Uses a t critical value when stat_label == "t" and df is finite; otherwise
# uses a normal critical value.  Rownames are preserved from ct.
#
# Returns a two-column matrix [lower, upper] with the same rownames as ct.
.robust_ci <- function(ct, level, stat_label, df = Inf) {
  ests <- ct[, 1L]
  ses  <- ct[, 2L]

  crit <- if (stat_label == "t" && is.finite(df) && df > 0L) {
    stats::qt(0.5 + level / 200, df = df)
  } else {
    stats::qnorm(0.5 + level / 200)
  }

  ci <- cbind(
    "2.5 %"  = ests - crit * ses,
    "97.5 %" = ests + crit * ses
  )
  rownames(ci) <- rownames(ct)
  ci
}

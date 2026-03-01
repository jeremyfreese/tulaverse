# ---------------------------------------------------------------------------
# tulatab() — Stata-inspired frequency tabulation (tula tabulate path)
#
# Exported entry point, S3 constructors, and print methods.
# Internal helpers for one-way tables live in R/tab_helpers.R.
# Internal helpers for two-way crosstabs live in R/crosstab_helpers.R.
# ---------------------------------------------------------------------------


#' Stata-Inspired Frequency Tabulation
#'
#' Produces a Stata-inspired one-way frequency table or two-way
#' crosstabulation.
#'
#' When called with one variable, produces a one-way frequency table with
#' counts, percentages, and (for ordinal / numeric variables) cumulative
#' percentages. When called with two variables, produces a two-way
#' crosstabulation with cell frequencies and column (or row / cell)
#' percentages. In either mode, the optional `mean` parameter replaces
#' frequencies and percentages with the mean and non-missing count of a
#' numeric variable.
#'
#' @param y A vector (factor, character, numeric, or haven-labelled) or an
#'   unquoted variable name when \code{data} is supplied.  In two-way mode,
#'   this is the row variable.
#' @param x Optional.  A second vector or unquoted variable name for the
#'   column variable.  When supplied, a two-way crosstab is produced.
#' @param data Optional data frame.  When supplied, \code{y} and \code{x}
#'   are evaluated inside \code{data} using non-standard evaluation.
#' @param missing Logical.  If \code{FALSE} (default), \code{NA} values are
#'   excluded.  If \code{TRUE}, missing values appear as extra rows (and
#'   columns in two-way mode).
#' @param sort Logical.  If \code{TRUE}, rows (and columns in two-way mode)
#'   are sorted by descending marginal frequency.
#' @param value Logical.  For haven-labelled variables: show numeric codes.
#'   In two-way mode, when both \code{value} and \code{label} are
#'   \code{TRUE}, labels take precedence (codes hidden).
#' @param label Logical.  For haven-labelled variables: show value labels.
#' @param width Integer, \code{Inf}, or \code{NULL}.  Maximum total character
#'   width.  Defaults to \code{getOption("width")}.
#' @param pct Character or \code{NULL}.  Two-way mode only.
#'   \code{"col"} (default) = column percentages,
#'   \code{"row"} = row percentages,
#'   \code{"cell"} = cell percentages,
#'   \code{NULL} = no percentages.
#' @param freq Logical.  Two-way mode only.  If \code{TRUE} (default),
#'   frequencies are shown.
#' @param by Optional.  A vector or unquoted variable name for a grouping
#'   variable.  When supplied (two-way mode only), a separate crosstab is
#'   produced for each level of the grouping variable.  For haven-labelled
#'   \code{by} variables, panel headers show \code{"name = code [label]"}.
#' @param mean Optional.  A numeric vector or unquoted variable name.  When
#'   supplied, replaces frequencies and percentages with the mean and
#'   non-missing count (N) of this variable per category (one-way) or per
#'   cell (two-way).  The \code{pct} and \code{freq} parameters are silently
#'   ignored in two-way mode when \code{mean} is specified.
#' @param ... Reserved for future extensions.
#'
#' @return Invisibly returns a \code{tula_tab} (one-way),
#'   \code{tula_crosstab} (two-way), or \code{tula_crosstab_by} (grouped
#'   two-way) S3 object.
#'
#' @examples
#' tulatab(mtcars$cyl)
#' tulatab(cyl, data = mtcars)
#' tulatab(mtcars$vs, mtcars$cyl)
#' tulatab(vs, cyl, data = mtcars, by = am)
#'
#' @export
tulatab <- function(y, x = NULL, data = NULL, missing = FALSE, sort = FALSE,
                    value = TRUE, label = TRUE, width = NULL,
                    pct = "col", freq = TRUE, by = NULL, mean = NULL,
                    listwise = TRUE, ...) {

  # Capture expressions before evaluation
  y_expr    <- substitute(y)
  x_expr    <- substitute(x)
  by_expr   <- substitute(by)
  mean_expr <- substitute(mean)

  # --- Resolve y vector and name -------------------------------------------
  if (!is.null(data)) {
    y_vec  <- eval(y_expr, envir = data, enclos = parent.frame())
    y_name <- deparse(y_expr)
  } else {
    y_vec  <- y
    y_name <- .extract_var_name(y_expr)
  }

  # --- Detect whether by was supplied ---------------------------------------
  has_by <- !missing(by) && !is.null(by_expr) && !identical(by_expr, quote(NULL))

  if (has_by) {
    if (!is.null(data)) {
      by_vec  <- eval(by_expr, envir = data, enclos = parent.frame())
      by_name <- deparse(by_expr)
    } else {
      by_vec  <- by
      by_name <- .extract_var_name(by_expr)
    }
  }

  # --- Detect whether mean was supplied -------------------------------------
  has_mean <- !missing(mean) && !is.null(mean_expr) &&
              !identical(mean_expr, quote(NULL))

  if (has_mean) {
    # Resolve mean= into a list of numeric vectors + names + labels.
    # Supports both single variable (mean = mpg) and c() syntax (mean = c(mpg, wt)).
    resolved <- .resolve_mean_args(mean_expr, mean, data, parent.frame())
    mean_list   <- resolved$vecs     # named list of numeric vectors
    mean_names  <- resolved$names    # character vector
    mean_labels <- resolved$labels   # character vector (NA where no label)

    # Backward-compat: single-variable case also sets mean_vec / mean_name / mean_label
    if (length(mean_list) == 1L) {
      mean_vec   <- mean_list[[1L]]
      mean_name  <- mean_names[1L]
      mean_label <- mean_labels[1L]
      if (is.na(mean_label)) mean_label <- NULL
    }
  }

  # --- Detect whether x was supplied (two-way mode) ------------------------
  has_x <- !missing(x) && !is.null(x_expr) && !identical(x_expr, quote(NULL))

  if (has_x) {
    # --- Two-way crosstab path ---------------------------------------------
    if (!is.null(data)) {
      x_vec  <- eval(x_expr, envir = data, enclos = parent.frame())
      x_name <- deparse(x_expr)
    } else {
      x_vec  <- x
      x_name <- .extract_var_name(x_expr)
    }

    # Guard: catch accidentally passing a data frame as x
    if (is.data.frame(x_vec)) {
      stop("tulatab(): 'x' appears to be a data frame. ",
           "Did you mean tulatab(y, data = ...)?", call. = FALSE)
    }

    # Guard: multi-variable mean= not supported in two-way mode
    if (has_mean && length(mean_list) > 1L) {
      stop("tulatab(): multiple mean variables are only supported in one-way mode.",
           call. = FALSE)
    }

    # Guard: mean variable must have same length as y and x
    if (has_mean && length(mean_vec) != length(y_vec)) {
      stop("tulatab(): 'mean' must have the same length as y and x.",
           call. = FALSE)
    }

    # Validate pct
    if (!is.null(pct)) {
      pct <- match.arg(pct, c("col", "row", "cell"))
    }

    # Haven display override for crosstabs: labels win when both TRUE
    show_value <- isTRUE(value)
    show_label <- isTRUE(label)
    if (show_value && show_label) {
      show_value <- FALSE
      show_label <- TRUE
    }

    y_type <- .detect_var_type(y_vec)
    x_type <- .detect_var_type(x_vec)

    y_label <- attr(y_vec, "label", exact = TRUE)
    x_label <- attr(x_vec, "label", exact = TRUE)

    # --- by= grouped crosstab path -----------------------------------------
    if (has_by) {
      if (length(by_vec) != length(y_vec)) {
        stop("tulatab(): 'by' must have the same length as y and x.",
             call. = FALSE)
      }

      by_type      <- .detect_var_type(by_vec)
      by_var_label <- attr(by_vec, "label", exact = TRUE)

      by_info <- .crosstab_by_levels(by_vec, by_type, by_name,
                                     by_var_label, missing)

      panels <- list()
      for (k in seq_along(by_info$level_keys)) {
        mask <- by_info$key_vec == by_info$level_keys[k]
        mask[is.na(mask)] <- FALSE

        if (!any(mask)) next   # skip empty by-groups

        y_sub <- y_vec[mask]
        x_sub <- x_vec[mask]

        # Preserve haven attributes through subsetting
        if (y_type == "haven_labelled") {
          attr(y_sub, "labels") <- attr(y_vec, "labels", exact = TRUE)
          attr(y_sub, "label")  <- attr(y_vec, "label", exact = TRUE)
          class(y_sub) <- class(y_vec)
        }
        if (x_type == "haven_labelled") {
          attr(x_sub, "labels") <- attr(x_vec, "labels", exact = TRUE)
          attr(x_sub, "label")  <- attr(x_vec, "label", exact = TRUE)
          class(x_sub) <- class(x_vec)
        }

        ct_info_k <- .build_crosstab(
          y_vec      = y_sub,
          x_vec      = x_sub,
          y_type     = y_type,
          x_type     = x_type,
          missing    = missing,
          sort       = sort,
          show_value = show_value,
          show_label = show_label
        )

        # Build mean matrices per by-group if mean= is specified
        mean_info_k <- NULL
        if (has_mean) {
          mean_sub_k <- mean_vec[mask][ct_info_k$keep_mask]
          mean_info_k <- .build_mean_crosstab(
            y_keys         = ct_info_k$y_keys,
            x_keys         = ct_info_k$x_keys,
            mean_vec       = mean_sub_k,
            row_levels     = ct_info_k$row_levels,
            col_levels     = ct_info_k$col_levels,
            sort_row_order = ct_info_k$sort_row_order,
            sort_col_order = ct_info_k$sort_col_order
          )
        }

        panels[[length(panels) + 1L]] <- list(
          by_header = by_info$level_headers[k],
          ct = new_tula_crosstab(
            ct_matrix          = ct_info_k$ct_matrix,
            row_levels         = ct_info_k$row_levels,
            col_levels         = ct_info_k$col_levels,
            row_name           = y_name,
            col_name           = x_name,
            row_label          = y_label,
            col_label          = x_label,
            missing            = missing,
            sort               = sort,
            pct                = pct,
            freq               = isTRUE(freq),
            value              = isTRUE(value),
            label              = isTRUE(label),
            width              = width,
            mean_mat           = if (!is.null(mean_info_k)) mean_info_k$mean_mat else NULL,
            n_mat              = if (!is.null(mean_info_k)) mean_info_k$n_mat else NULL,
            mean_name          = if (has_mean) mean_name else NULL,
            mean_label         = if (has_mean) mean_label else NULL,
            mean_row_marginals = if (!is.null(mean_info_k)) mean_info_k$mean_row_marginals else NULL,
            n_row_marginals    = if (!is.null(mean_info_k)) mean_info_k$n_row_marginals else NULL,
            mean_col_marginals = if (!is.null(mean_info_k)) mean_info_k$mean_col_marginals else NULL,
            n_col_marginals    = if (!is.null(mean_info_k)) mean_info_k$n_col_marginals else NULL,
            mean_grand         = if (!is.null(mean_info_k)) mean_info_k$mean_grand else NULL,
            n_grand            = if (!is.null(mean_info_k)) mean_info_k$n_grand else NULL
          )
        )
      }

      return(new_tula_crosstab_by(
        panels       = panels,
        by_name      = by_name,
        by_var_label = by_var_label,
        width        = width
      ))
    }

    # --- Ungrouped crosstab path --------------------------------------------
    ct_info <- .build_crosstab(
      y_vec      = y_vec,
      x_vec      = x_vec,
      y_type     = y_type,
      x_type     = x_type,
      missing    = missing,
      sort       = sort,
      show_value = show_value,
      show_label = show_label
    )

    # Build mean matrices if mean= is specified
    mean_info <- NULL
    if (has_mean) {
      mean_sub <- mean_vec[ct_info$keep_mask]
      mean_info <- .build_mean_crosstab(
        y_keys          = ct_info$y_keys,
        x_keys          = ct_info$x_keys,
        mean_vec        = mean_sub,
        row_levels      = ct_info$row_levels,
        col_levels      = ct_info$col_levels,
        sort_row_order  = ct_info$sort_row_order,
        sort_col_order  = ct_info$sort_col_order
      )
    }

    return(new_tula_crosstab(
      ct_matrix          = ct_info$ct_matrix,
      row_levels         = ct_info$row_levels,
      col_levels         = ct_info$col_levels,
      row_name           = y_name,
      col_name           = x_name,
      row_label          = y_label,
      col_label          = x_label,
      missing            = missing,
      sort               = sort,
      pct                = pct,
      freq               = isTRUE(freq),
      value              = isTRUE(value),
      label              = isTRUE(label),
      width              = width,
      mean_mat           = if (!is.null(mean_info)) mean_info$mean_mat else NULL,
      n_mat              = if (!is.null(mean_info)) mean_info$n_mat else NULL,
      mean_name          = if (has_mean) mean_name else NULL,
      mean_label         = if (has_mean) mean_label else NULL,
      mean_row_marginals = if (!is.null(mean_info)) mean_info$mean_row_marginals else NULL,
      n_row_marginals    = if (!is.null(mean_info)) mean_info$n_row_marginals else NULL,
      mean_col_marginals = if (!is.null(mean_info)) mean_info$mean_col_marginals else NULL,
      n_col_marginals    = if (!is.null(mean_info)) mean_info$n_col_marginals else NULL,
      mean_grand         = if (!is.null(mean_info)) mean_info$mean_grand else NULL,
      n_grand            = if (!is.null(mean_info)) mean_info$n_grand else NULL
    ))
  }

  # --- Guard: by= requires two-way mode ------------------------------------
  if (has_by) {
    stop("tulatab(): 'by' requires a two-way crosstab (supply both y and x).",
         call. = FALSE)
  }

  # --- Guard: mean= length check for one-way mode ---------------------------
  if (has_mean) {
    for (k in seq_along(mean_list)) {
      if (length(mean_list[[k]]) != length(y_vec)) {
        stop(sprintf("tulatab(): mean variable '%s' must have the same length as y.",
                     mean_names[k]), call. = FALSE)
      }
    }
  }

  # --- One-way path ---------------------------------------------------------

  vec      <- y_vec
  var_name <- y_name

  # Haven variable label
  var_label <- attr(vec, "label", exact = TRUE)

  # Detect variable type
  var_type <- .detect_var_type(vec)

  # Determine whether Cum column is shown
  show_cum <- var_type %in% c("ordered", "numeric", "haven_labelled")

  # Haven detection for the S3 object
  haven_labels_attr <- attr(vec, "labels", exact = TRUE)
  has_haven_labels  <- !is.null(haven_labels_attr) && length(haven_labels_attr) > 0L
  has_haven_values  <- var_type == "haven_labelled"

  # Build the tab_df
  tab_df <- .build_tab_df(vec, var_type, missing, sort, show_cum,
                          show_value = isTRUE(value),
                          show_label = isTRUE(label))

  # Compute per-category means if mean= is specified
  mean_info_ow <- NULL
  if (has_mean) {
    mean_info_ow <- .compute_oneway_multi_means(
      vec, mean_list, tab_df, var_type, listwise = isTRUE(listwise)
    )
  }

  # Construct and return the S3 object
  new_tula_tab(
    tab_df           = tab_df,
    var_name         = var_name,
    var_type         = var_type,
    show_cum         = show_cum,
    has_haven_labels = has_haven_labels,
    has_haven_values = has_haven_values,
    var_label        = var_label,
    width            = width,
    missing          = missing,
    value            = isTRUE(value),
    label            = isTRUE(label),
    mean_mat         = if (!is.null(mean_info_ow)) mean_info_ow$mean_mat else NULL,
    n_mat            = if (!is.null(mean_info_ow)) mean_info_ow$n_mat else NULL,
    mean_names       = if (has_mean) mean_names else NULL,
    mean_labels      = if (has_mean) mean_labels else NULL,
    mean_grands      = if (!is.null(mean_info_ow)) {
      list(means = mean_info_ow$mean_totals, ns = mean_info_ow$n_totals)
    } else NULL
  )
}


# ---------------------------------------------------------------------------
# Helper: resolve mean= argument into a uniform list structure.
#
# Handles both single-variable (mean = mpg) and multi-variable
# (mean = c(mpg, wt)) syntax via NSE.  Returns a list with:
#   $vecs   - named list of numeric vectors
#   $names  - character vector of variable names
#   $labels - character vector (NA where no haven label)
# ---------------------------------------------------------------------------
.resolve_mean_args <- function(mean_expr, mean_val, data, enclos) {
  # Determine argument expressions: either c(a, b, ...) or a single symbol/call
  if (is.call(mean_expr) && identical(mean_expr[[1L]], quote(c))) {
    # c(var1, var2, ...): iterate over arguments
    arg_exprs <- as.list(mean_expr)[-1L]  # drop the 'c'
  } else {
    # Single variable
    arg_exprs <- list(mean_expr)
  }

  vecs   <- vector("list", length(arg_exprs))
  names_ <- character(length(arg_exprs))
  labels <- character(length(arg_exprs))

  for (k in seq_along(arg_exprs)) {
    expr_k <- arg_exprs[[k]]
    if (!is.null(data)) {
      v <- eval(expr_k, envir = data, enclos = enclos)
      nm <- deparse(expr_k)
    } else {
      # Direct vector call: mean = c(df$mpg, df$wt) — eval in parent frame
      v  <- eval(expr_k, envir = enclos)
      nm <- .extract_var_name(expr_k)
    }

    # Validate: must be numeric
    if (!is.numeric(v)) {
      stop(sprintf("tulatab(): mean variable '%s' must be numeric.", nm),
           call. = FALSE)
    }

    vecs[[k]]   <- v
    names_[k]   <- nm
    lbl <- attr(v, "label", exact = TRUE)
    labels[k]   <- if (!is.null(lbl) && nzchar(lbl)) lbl else NA_character_
  }

  names(vecs) <- names_
  list(vecs = vecs, names = names_, labels = labels)
}


# ---------------------------------------------------------------------------
# One-way S3 constructor and print method
# ---------------------------------------------------------------------------

# Internal constructor for tula_tab S3 objects.
new_tula_tab <- function(tab_df, var_name, var_type, show_cum,
                         has_haven_labels, has_haven_values,
                         var_label, width, missing, value, label,
                         mean_mat = NULL, n_mat = NULL,
                         mean_names = NULL, mean_labels = NULL,
                         mean_grands = NULL) {
  structure(
    list(
      tab_df           = tab_df,
      var_name         = var_name,
      var_type         = var_type,
      show_cum         = show_cum,
      has_haven_labels = has_haven_labels,
      has_haven_values = has_haven_values,
      var_label        = var_label,
      width            = width,
      missing          = missing,
      value            = value,
      label            = label,
      mean_mat         = mean_mat,
      n_mat            = n_mat,
      mean_names       = mean_names,
      mean_labels      = mean_labels,
      mean_grands      = mean_grands
    ),
    class = "tula_tab"
  )
}


#' Print method for tula_tab objects
#'
#' Prints a Stata-inspired frequency table. Called automatically when a
#' \code{tula_tab} object is returned to the console.
#'
#' @param x A \code{tula_tab} object (from \code{tulatab()}).
#' @param ... Ignored (for S3 compatibility).
#' @return Invisibly returns \code{x}.
#' @export
print.tula_tab <- function(x, ...) {
  lines <- .format_tab_table(x)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}


# ---------------------------------------------------------------------------
# Two-way S3 constructor and print method
# ---------------------------------------------------------------------------

# Internal constructor for tula_crosstab S3 objects.
new_tula_crosstab <- function(ct_matrix, row_levels, col_levels,
                               row_name, col_name, row_label, col_label,
                               missing, sort, pct, freq,
                               value, label, width,
                               mean_mat = NULL, n_mat = NULL,
                               mean_name = NULL, mean_label = NULL,
                               mean_row_marginals = NULL,
                               n_row_marginals = NULL,
                               mean_col_marginals = NULL,
                               n_col_marginals = NULL,
                               mean_grand = NULL,
                               n_grand = NULL) {
  structure(
    list(
      ct_matrix          = ct_matrix,
      row_levels         = row_levels,
      col_levels         = col_levels,
      row_name           = row_name,
      col_name           = col_name,
      row_label          = row_label,
      col_label          = col_label,
      missing            = missing,
      sort               = sort,
      pct                = pct,
      freq               = freq,
      value              = value,
      label              = label,
      width              = width,
      mean_mat           = mean_mat,
      n_mat              = n_mat,
      mean_name          = mean_name,
      mean_label         = mean_label,
      mean_row_marginals = mean_row_marginals,
      n_row_marginals    = n_row_marginals,
      mean_col_marginals = mean_col_marginals,
      n_col_marginals    = n_col_marginals,
      mean_grand         = mean_grand,
      n_grand            = n_grand
    ),
    class = "tula_crosstab"
  )
}


#' Print method for tula_crosstab objects
#'
#' Prints a Stata-inspired two-way crosstabulation.
#'
#' @param x A \code{tula_crosstab} object (from \code{tulatab()}).
#' @param ... Ignored (for S3 compatibility).
#' @return Invisibly returns \code{x}.
#' @export
print.tula_crosstab <- function(x, ...) {
  # Print "Mean of <varname>" header when mean mode is active
  if (!is.null(x$mean_mat)) {
    mean_display <- if (!is.null(x$mean_label) && nzchar(x$mean_label)) {
      x$mean_label
    } else {
      x$mean_name
    }
    cat("Mean of ", mean_display, "\n\n", sep = "")
  }
  lines <- .format_crosstab_table(x)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}


# ---------------------------------------------------------------------------
# Grouped two-way S3 constructor and print method (by= support)
# ---------------------------------------------------------------------------

# Internal constructor for tula_crosstab_by S3 objects.
new_tula_crosstab_by <- function(panels, by_name, by_var_label, width) {
  structure(
    list(
      panels       = panels,       # list of list(by_header, ct)
      by_name      = by_name,
      by_var_label = by_var_label,
      width        = width
    ),
    class = "tula_crosstab_by"
  )
}


#' Print method for tula_crosstab_by objects
#'
#' Prints a series of Stata-inspired two-way crosstabulations, one per level
#' of the grouping variable.
#'
#' @param x A \code{tula_crosstab_by} object (from \code{tulatab()}).
#' @param ... Ignored (for S3 compatibility).
#' @return Invisibly returns \code{x}.
#' @export
print.tula_crosstab_by <- function(x, ...) {
  # Print "Mean of <varname>" header once when mean mode is active
  if (length(x$panels) > 0L && !is.null(x$panels[[1L]]$ct$mean_mat)) {
    ct1 <- x$panels[[1L]]$ct
    mean_display <- if (!is.null(ct1$mean_label) && nzchar(ct1$mean_label)) {
      ct1$mean_label
    } else {
      ct1$mean_name
    }
    cat("Mean of ", mean_display, "\n", sep = "")
  }
  for (i in seq_along(x$panels)) {
    panel <- x$panels[[i]]
    if (i > 1L) cat("\n")
    cat("-> ", panel$by_header, "\n\n", sep = "")
    lines <- .format_crosstab_table(panel$ct)
    cat(paste(lines, collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}


# ---------------------------------------------------------------------------
# Shared helper: extract a clean variable name from a captured expression
# ---------------------------------------------------------------------------

# Handles common patterns:
#   substitute(df$varname)   -> "varname"
#   substitute(df[["var"]])  -> "var"
#   substitute(varname)      -> "varname"
#   substitute(some_func(x)) -> full deparse
.extract_var_name <- function(expr) {
  expr_str <- deparse(expr)

  # df$varname -> extract "varname"
  if (grepl("\\$", expr_str)) {
    parts <- strsplit(expr_str, "\\$")[[1L]]
    return(parts[length(parts)])
  }

  # df[["varname"]] or df[['varname']] -> extract "varname"
  m <- regmatches(expr_str, regexpr("\\[\\[\"([^\"]+)\"\\]\\]|\\[\\['([^']+)'\\]\\]",
                                     expr_str, perl = TRUE))
  if (length(m) > 0L && nchar(m) > 0L) {
    inner <- gsub("^\\[\\[[\"\']|[\"\']\\]\\]$", "", m)
    return(inner)
  }

  # Plain name or complex expression: use as-is
  expr_str
}

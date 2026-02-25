# ---------------------------------------------------------------------------
# tulatab() — Stata-style frequency tabulation (tula tabulate path)
#
# Exported entry point, S3 constructors, and print methods.
# Internal helpers for one-way tables live in R/tab_helpers.R.
# Internal helpers for two-way crosstabs live in R/crosstab_helpers.R.
# ---------------------------------------------------------------------------


#' Stata-Style Frequency Tabulation
#'
#' Produces a Stata-style one-way frequency table or two-way crosstabulation.
#'
#' When called with one variable, produces a one-way frequency table with
#' counts, percentages, and (for ordinal / numeric variables) cumulative
#' percentages.
#'
#' When called with two variables, produces a two-way crosstabulation with
#' cell frequencies and column (or row / cell) percentages.
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
#' @param ... Reserved for future extensions.
#'
#' @return Invisibly returns a \code{tula_tab} (one-way) or
#'   \code{tula_crosstab} (two-way) S3 object.
#'
#' @examples
#' tulatab(mtcars$cyl)
#' tulatab(cyl, data = mtcars)
#' tulatab(mtcars$vs, mtcars$cyl)
#'
#' @export
tulatab <- function(y, x = NULL, data = NULL, missing = FALSE, sort = FALSE,
                    value = TRUE, label = TRUE, width = NULL,
                    pct = "col", freq = TRUE, ...) {

  # Capture expressions before evaluation
  y_expr <- substitute(y)
  x_expr <- substitute(x)

  # --- Resolve y vector and name -------------------------------------------
  if (!is.null(data)) {
    y_vec  <- eval(y_expr, envir = data, enclos = parent.frame())
    y_name <- deparse(y_expr)
  } else {
    y_vec  <- y
    y_name <- .extract_var_name(y_expr)
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

    y_label <- attr(y_vec, "label", exact = TRUE)
    x_label <- attr(x_vec, "label", exact = TRUE)

    return(new_tula_crosstab(
      ct_matrix  = ct_info$ct_matrix,
      row_levels = ct_info$row_levels,
      col_levels = ct_info$col_levels,
      row_name   = y_name,
      col_name   = x_name,
      row_label  = y_label,
      col_label  = x_label,
      missing    = missing,
      sort       = sort,
      pct        = pct,
      freq       = isTRUE(freq),
      value      = isTRUE(value),
      label      = isTRUE(label),
      width      = width
    ))
  }

  # --- One-way path (unchanged logic) --------------------------------------

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
    label            = isTRUE(label)
  )
}


# ---------------------------------------------------------------------------
# One-way S3 constructor and print method (unchanged)
# ---------------------------------------------------------------------------

# Internal constructor for tula_tab S3 objects.
new_tula_tab <- function(tab_df, var_name, var_type, show_cum,
                         has_haven_labels, has_haven_values,
                         var_label, width, missing, value, label) {
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
      label            = label
    ),
    class = "tula_tab"
  )
}


#' Print method for tula_tab objects
#'
#' Prints a Stata-style frequency table. Called automatically when a
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
                               value, label, width) {
  structure(
    list(
      ct_matrix  = ct_matrix,
      row_levels = row_levels,
      col_levels = col_levels,
      row_name   = row_name,
      col_name   = col_name,
      row_label  = row_label,
      col_label  = col_label,
      missing    = missing,
      sort       = sort,
      pct        = pct,
      freq       = freq,
      value      = value,
      label      = label,
      width      = width
    ),
    class = "tula_crosstab"
  )
}


#' Print method for tula_crosstab objects
#'
#' Prints a Stata-style two-way crosstabulation.
#'
#' @param x A \code{tula_crosstab} object (from \code{tulatab()}).
#' @param ... Ignored (for S3 compatibility).
#' @return Invisibly returns \code{x}.
#' @export
print.tula_crosstab <- function(x, ...) {
  lines <- .format_crosstab_table(x)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
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

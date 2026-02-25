# ---------------------------------------------------------------------------
# tulatab() — Stata-style frequency tabulation (tula tabulate path)
#
# Exported entry point, S3 constructor, and print method.
# Internal helpers live in R/tab_helpers.R.
# ---------------------------------------------------------------------------


#' Stata-Style Frequency Tabulation
#'
#' Produces a Stata-style one-way frequency table with counts, percentages,
#' and (for ordinal / numeric variables) cumulative percentages.
#'
#' @param x A vector (factor, character, numeric, or haven-labelled) or an
#'   unquoted variable name when \code{data} is supplied.
#' @param data Optional data frame. When supplied, \code{x} is evaluated
#'   inside \code{data} using non-standard evaluation.
#' @param missing Logical. If \code{FALSE} (default), \code{NA} values are
#'   excluded from the table and the denominator. If \code{TRUE}, missing
#'   values appear as rows at the bottom of the table and are counted in the
#'   denominator.
#' @param sort Logical. If \code{FALSE} (default), values are shown in their
#'   natural order (level order for factors, alphabetical for character,
#'   ascending for numeric). If \code{TRUE}, rows are sorted by descending
#'   frequency. Missing rows always appear at the bottom regardless.
#' @param value Logical. For haven-labelled variables, if \code{TRUE} (default),
#'   the numeric code is displayed. If \code{FALSE}, codes are hidden.
#' @param label Logical. For haven-labelled variables, if \code{TRUE} (default),
#'   value labels are displayed. If \code{FALSE}, labels are hidden.
#'   If both \code{value} and \code{label} are \code{FALSE}, codes are shown.
#' @param width Integer, \code{Inf}, or \code{NULL}. Maximum total character
#'   width. Defaults to \code{getOption("width")}.
#' @param ... Reserved for future extensions.
#'
#' @return Invisibly returns a \code{tula_tab} S3 object. The primary side
#'   effect is printing the frequency table to the console.
#'
#' @examples
#' tulatab(mtcars$cyl)
#' tulatab(factor(mtcars$gear, ordered = TRUE))
#' tulatab(cyl, data = mtcars)
#'
#' @export
tulatab <- function(x, data = NULL, missing = FALSE, sort = FALSE,
                    value = TRUE, label = TRUE, width = NULL, ...) {

  # Capture the expression before evaluation for variable name extraction

  x_expr <- substitute(x)

  # --- Resolve the vector and variable name ---------------------------------
  if (!is.null(data)) {
    # NSE path: evaluate x_expr inside data
    vec <- eval(x_expr, envir = data, enclos = parent.frame())
    var_name <- deparse(x_expr)
  } else {
    vec <- x
    # Extract a clean variable name from the expression
    # e.g. "mtcars$cyl" -> "cyl", "df[['var']]" -> "var"
    var_name <- .extract_var_name(x_expr)
  }

  # --- Haven variable label (attr(x, "label")) -----------------------------
  var_label <- attr(vec, "label", exact = TRUE)

  # --- Detect variable type -------------------------------------------------
  var_type <- .detect_var_type(vec)

  # --- Determine whether Cum column is shown --------------------------------
  show_cum <- var_type %in% c("ordered", "numeric", "haven_labelled")

  # --- Haven detection for the S3 object ------------------------------------
  haven_labels_attr <- attr(vec, "labels", exact = TRUE)
  has_haven_labels  <- !is.null(haven_labels_attr) && length(haven_labels_attr) > 0L
  has_haven_values  <- var_type == "haven_labelled"

  # --- Build the tab_df -----------------------------------------------------
  tab_df <- .build_tab_df(vec, var_type, missing, sort, show_cum,
                          show_value = isTRUE(value),
                          show_label = isTRUE(label))

  # --- Construct and return the S3 object -----------------------------------
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


# Extract a clean variable name from a captured expression.
#
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

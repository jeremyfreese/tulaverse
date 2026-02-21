#' Print Stata-Style Regression Output
#'
#' `tula()` prints regression output formatted to resemble Stata's regression
#' output, with a two-block header (model fit statistics on the left, sample
#' statistics on the right) and a coefficient table with optional confidence
#' intervals.
#'
#' Factor variables are displayed Stata-style: the variable name appears on
#' its own header row, and each non-reference level is indented below it.
#' The intercept is always placed last, separated from the other coefficients
#' by a horizontal line.
#'
#' @param model A fitted model object. Currently supported: `lm`, `glm`.
#' @param wide Logical. If `FALSE` (the default), confidence intervals are
#'   omitted and the output is narrower. If `TRUE`, 95% confidence interval
#'   columns are added.
#' @param ref Logical. If `FALSE` (the default), the reference category of
#'   each factor variable is omitted. If `TRUE`, it is shown as the last row
#'   within its group, with a coefficient of 0 and blanks for SE, statistic,
#'   and p-value.
#' @param label Logical. If `TRUE` (the default), value labels are used for
#'   factor levels when available (e.g. from `haven`-labelled variables).
#'   If `FALSE`, raw level values are always used.
#' @param width Integer. Maximum total character width of the output.
#'   Defaults to `getOption("width")` (typically 80 in an interactive R
#'   session, but respects whatever the user has set). When the natural width
#'   of the table or header would exceed this value, the label column is
#'   narrowed and long labels are truncated with a trailing `~`. Set to `Inf`
#'   to allow unlimited width.
#' @param ... Additional arguments passed to model-specific methods (reserved
#'   for future extensions).
#'
#' @return Invisibly returns a `tula_output` object (an S3 list). The primary
#'   side effect is printing to the console.
#'
#' @examples
#' m <- lm(mpg ~ cyl + wt, data = mtcars)
#' tula(m)
#' tula(m, wide = TRUE)
#' tula(m, width = 120)
#'
#' m2 <- glm(am ~ cyl + wt, data = mtcars, family = binomial)
#' tula(m2)
#'
#' @export
tula <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                 width = NULL, ...) {
  UseMethod("tula")
}

#' @rdname tula
#' @export
tula.default <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                         width = NULL, ...) {
  stop(
    "tula() does not support objects of class '",
    paste(class(model), collapse = "', '"),
    "'.\nSupported classes: lm, glm.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------------
# Internal constructor for the canonical tula_output S3 object.
#
# All tula.*() methods return one of these. The print method operates on
# this object without knowing anything about the original model type.
#
# Fields:
#   model_type   - character: "lm", "glm", etc.
#   header_left  - named numeric vector: left-column header entries
#   header_right - named numeric vector: right-column header entries
#   coef_df      - data.frame from build_coef_df()
#   stat_label   - character: "t" or "z"
#   wide         - logical: were CIs requested?
#   family_label - character or NULL: printed above header for glm
#   width        - integer or Inf: maximum output line width (NULL = use option)
# ---------------------------------------------------------------------------
new_tula_output <- function(model_type,
                            header_left,
                            header_right,
                            coef_df,
                            stat_label,
                            wide,
                            family_label = NULL,
                            width        = NULL) {
  structure(
    list(
      model_type   = model_type,
      header_left  = header_left,
      header_right = header_right,
      coef_df      = coef_df,
      stat_label   = stat_label,
      wide         = wide,
      family_label = family_label,
      width        = width
    ),
    class = "tula_output"
  )
}

# Thin wrapper so methods can pass ref/label through without storing them on
# the output object — they are consumed entirely by build_coef_df().
.parse_tula_opts <- function(ref, label) {
  list(ref = isTRUE(ref), label = isTRUE(label))
}


#' Print method for tula_output objects
#'
#' Assembles and prints the full Stata-style output. Called automatically
#' when a `tula_output` object is returned to the console.
#'
#' @param x A `tula_output` object (from `tula()`).
#' @param ... Ignored (for S3 compatibility).
#'
#' @return Invisibly returns `x`.
#' @export
print.tula_output <- function(x, ...) {
  # Resolve width: NULL means "read the option at print time", so that
  # the cap always reflects the user's current console width rather than the
  # width at the time tula() was called.
  max_w <- if (is.null(x$width)) getOption("width") else x$width

  # Compute the natural shared width, then cap at max_w.
  # min(..., Inf) passes through unchanged; any integer squeezes the label
  # column (truncation happens in format_coef_table via the existing ~ logic).
  natural_width <- compute_total_width(
    header_left  = x$header_left,
    header_right = x$header_right,
    coef_labels  = x$coef_df$label,
    wide         = x$wide
  )
  total_width <- min(natural_width, max_w)

  # Optional family/link line (glm only)
  if (!is.null(x$family_label)) {
    cat(x$family_label, "\n", sep = "")
  }

  # Two-column header block
  header_lines <- format_header(x$header_left, x$header_right,
                                total_width = total_width)
  cat(paste(header_lines, collapse = "\n"), "\n", sep = "")

  # Coefficient table
  table_lines <- format_coef_table(x$coef_df, x$stat_label, x$wide,
                                   total_width = total_width)
  cat(paste(table_lines, collapse = "\n"), "\n", sep = "")

  invisible(x)
}

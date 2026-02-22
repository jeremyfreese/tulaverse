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
#' @param width Integer or `Inf`. Maximum total character width of the output.
#'   Defaults to `getOption("width")` (typically 80 in an interactive R
#'   session, but respects whatever the user has set). When the natural width
#'   of the table or header would exceed this value, the label column is
#'   narrowed and long labels are truncated with a trailing `~`. Set to `Inf`
#'   to allow unlimited width.
#' @param sep Integer. For summarize output: number of variables between each
#'   horizontal separator line. Default 5. Set to 0 to suppress separators.
#'   Separators never split a factor variable's level rows; if a sep boundary
#'   falls inside a factor block, the separator is placed after the block.
#'   Ignored for regression output.
#' @param mad Logical. For summarize output: if `TRUE`, show the mean absolute
#'   deviation (MAD) instead of the standard deviation. Column header changes
#'   to `"MAD"`. Ignored for regression output.
#' @param median Logical. For summarize output: if `TRUE`, show the median
#'   instead of the mean and the IQR instead of SD/MAD. Column headers change
#'   to `"Median"` and `"IQR"`. Factor variable display is unchanged.
#'   Ignored for regression output.
#' @param digits Integer. For summarize output: number of significant digits
#'   used when formatting numeric values (mean, SD/MAD/IQR, min, max).
#'   Default 7, which avoids scientific notation for numbers up to 9,999,999.
#'   Ignored for regression output (which always uses 4 significant digits).
#' @param ... Additional arguments passed to model-specific methods (reserved
#'   for future extensions).
#'
#' @return For regression models, invisibly returns a `tula_output` object.
#'   For data frames and vectors, invisibly returns a `tula_summary` object.
#'   The primary side effect is printing to the console.
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
#' tula(mtcars)
#' tula(mtcars, sep = 3, median = TRUE)
#'
#' @export
tula <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                 width = NULL, sep = 5L, mad = FALSE, median = FALSE,
                 digits = 7L, ...) {
  # Capture the expression used for `model` before dispatch, so that vector
  # methods can display a meaningful variable name (e.g. "mtcars$mpg").
  .tula_call_nm <- deparse(substitute(model))
  UseMethod("tula")
}

#' @rdname tula
#' @export
tula.default <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                         width = NULL, sep = 5L, mad = FALSE, median = FALSE,
                         digits = 7L, ...) {
  # Atomic vectors (numeric, integer, logical, character, factor) are routed
  # to the summarize path.  Matrices and other dimensioned objects are not
  # supported.
  if (is.atomic(model) && is.null(dim(model))) {
    # Recover the expression the user typed for `model` from the generic's
    # call frame (UseMethod passes the generic's environment to the method).
    nm <- tryCatch({
      # sys.function(0) is the current method; sys.call(-1) is the generic call
      mc  <- sys.call(-1L)        # call to tula() (the generic)
      gfn <- sys.function(-1L)    # the generic function itself
      deparse(match.call(gfn, mc)$model)
    }, error = function(e) "x")
    df <- stats::setNames(list(model), nm)
    # Preserve factor class through as.data.frame
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    if (is.factor(model)) df[[nm]] <- model
    return(.tula_summarize(df, width = width, sep = sep,
                           mad = mad, median = median, digits = digits))
  }
  stop(
    "tula() does not support objects of class '",
    paste(class(model), collapse = "', '"),
    "'.\nSupported classes: lm, glm, data.frame, and atomic vectors.",
    call. = FALSE
  )
}

#' @rdname tula
#' @export
tula.data.frame <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                             width = NULL, sep = 5L, mad = FALSE,
                             median = FALSE, digits = 7L, ...) {
  .tula_summarize(model, width = width, sep = sep, mad = mad, median = median,
                  digits = digits)
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


# ---------------------------------------------------------------------------
# Internal constructor for tula_multinom_output S3 objects.
#
# Fields:
#   header_left  - named numeric vector: left-column header entries
#   header_right - named numeric vector: right-column header entries
#   blocks       - list of lists, one per non-base outcome:
#                    $outcome  character: outcome label
#                    $coef_df  data.frame from build_coef_df()
#   base_outcome - character: label of the base (reference) outcome
#   stat_label   - "z"
#   wide         - logical: were CIs requested?
#   width        - integer, Inf, or NULL
# ---------------------------------------------------------------------------
new_tula_multinom_output <- function(header_left,
                                     header_right,
                                     blocks,
                                     base_outcome,
                                     stat_label = "z",
                                     wide       = FALSE,
                                     width      = NULL) {
  structure(
    list(
      header_left  = header_left,
      header_right = header_right,
      blocks       = blocks,
      base_outcome = base_outcome,
      stat_label   = stat_label,
      wide         = wide,
      width        = width
    ),
    class = "tula_multinom_output"
  )
}


#' Print method for tula_multinom_output objects
#'
#' Prints Stata-style multinomial logit output: a shared header block,
#' then one coefficient table per non-base outcome separated by dashed
#' lines, then a "Base outcome:" footer line.
#'
#' @param x A `tula_multinom_output` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.tula_multinom_output <- function(x, ...) {
  max_w <- if (is.null(x$width)) getOption("width") else x$width

  # Compute total width from the shared header and the widest label across
  # all outcome blocks.
  all_labels <- unlist(lapply(x$blocks, function(b) b$coef_df$label))

  natural_width <- compute_total_width(
    header_left  = x$header_left,
    header_right = x$header_right,
    coef_labels  = all_labels,
    wide         = x$wide
  )
  total_width <- min(natural_width, max_w)

  sep_line <- char_rep("-", total_width)

  # Shared header (printed once above all outcome blocks)
  header_lines <- format_header(x$header_left, x$header_right,
                                total_width = total_width)
  cat(paste(header_lines, collapse = "\n"), "\n", sep = "")

  # One coefficient block per non-base outcome.
  # format_coef_table() returns: [sep, hdr, sep, ...rows..., sep]
  # We strip the first and last separator from each block, then wrap the
  # whole block with a single leading sep + outcome label and a single
  # trailing sep, so the layout between blocks is exactly one separator:
  #   sep
  #   <outcome>
  #   hdr
  #   sep
  #   ...rows...
  #   sep          <- this serves as both the block closer and the next block opener
  #   <next outcome>
  #   ...
  for (i in seq_along(x$blocks)) {
    blk <- x$blocks[[i]]
    table_lines <- format_coef_table(blk$coef_df, x$stat_label, x$wide,
                                     total_width = total_width)
    # table_lines: [sep, hdr, sep, ...rows..., sep]
    # Drop first (opening sep) — replaced by our sep_line + outcome label.
    # Drop last (closing sep) — we print it manually so we control the
    # boundary: for the last block we still want a final sep before the footer.
    inner_lines <- table_lines[-c(1L, length(table_lines))]
    cat(sep_line, "\n", sep = "")
    cat(blk$outcome, "\n", sep = "")
    cat(paste(inner_lines, collapse = "\n"), "\n", sep = "")
  }
  cat(sep_line, "\n", sep = "")

  # Base outcome footer
  cat("Base outcome: ", x$base_outcome, "\n", sep = "")

  invisible(x)
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

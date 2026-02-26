#' Stata-Inspired Regression and Summary Output
#'
#' `tula()` is an S3 generic that produces Stata-inspired console output for
#' two distinct input types: regression models (coefficient table with header
#' block) and data frames or vectors (descriptive statistics table).
#'
#' For regression models, the output includes a two-block header (model fit
#' statistics on the left, sample statistics on the right) and a coefficient
#' table with optional confidence intervals. Factor variables are grouped:
#' the variable name appears on its own header row, and each non-reference
#' level is indented below it. The intercept is always placed last.
#'
#' @param model A fitted model object or a data frame / vector. Supported
#'   model classes: `lm`, `glm`, `negbin` (negative binomial via
#'   [MASS::glm.nb()]), `multinom` (multinomial logit via [nnet::multinom()]),
#'   `polr` (ordered regression via [MASS::polr()]), `clm` (ordered regression
#'   via [ordinal::clm()]), `coxph` (Cox proportional hazards via
#'   [survival::coxph()]), `rq` and `rqs` (quantile regression via
#'   [quantreg::rq()]). When passed a data frame or atomic vector, produces
#'   descriptive statistics output.
#' @param wide Logical or `NULL`. If `TRUE`, 95% confidence interval columns
#'   are added. If `FALSE`, they are omitted. If `NULL` (the default),
#'   confidence intervals are shown automatically when the effective output
#'   width is 80 or more, and omitted when it is narrower.
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
#' @param exp Logical. If `TRUE`, coefficients are exponentiated: the estimate
#'   column shows `exp(b)` and the standard error column shows the delta-method
#'   standard error (`exp(b) * SE(b)`), labelled `"DMSE"`. The test statistic
#'   and p-value are unchanged. Reference-level rows show `1` instead of `0`.
#'   When `wide = TRUE`, CI bounds are also exponentiated.
#'   Default `FALSE`. Ignored for summarize output.
#' @param level Numeric. Confidence interval width as a percentage (e.g. 90,
#'   95, 99). Values less than 1 are multiplied by 100 (so `0.95` becomes 95).
#'   Default 95. Ignored for summarize output.
#' @param parallel Logical. For multinomial logit and multi-quantile models:
#'   if `TRUE`, outcomes/quantiles are shown as side-by-side columns with
#'   significance stars instead of stacked blocks. Default `FALSE`.
#' @param robust Logical. If `TRUE`, heteroskedasticity-robust (HC3) standard
#'   errors are used. Requires the `sandwich` package. Default `FALSE`.
#' @param vcov Character or matrix. When a character string (e.g. `"HC4"`),
#'   specifies the HC type for robust SEs. When a matrix, used directly as
#'   the variance-covariance matrix. Default `NULL`.
#' @param cluster Character. Variable name for cluster-robust standard errors.
#'   Implies `robust = TRUE`. Default `NULL`.
#' @param codebook Logical. For data frames and vectors: if `TRUE`, produces
#'   Stata-inspired codebook output (per-variable blocks with type, range,
#'   unique values, and tabulations or percentile distributions) instead of
#'   the default summary table. Default `FALSE`. Ignored for regression output.
#' @param ... Additional arguments passed to model-specific methods.
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
tula <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                 width = NULL, sep = 5L, mad = FALSE, median = FALSE,
                 digits = 7L, exp = FALSE, level = 95, parallel = FALSE,
                 robust = FALSE, vcov = NULL, cluster = NULL,
                 codebook = FALSE, ...) {
  # Capture the expression used for `model` before dispatch, so that vector
  # methods can display a meaningful variable name (e.g. "mtcars$mpg").
  .tula_call_nm <- deparse(substitute(model))
  UseMethod("tula")
}

#' @rdname tula
#' @export
tula.default <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                         width = NULL, sep = 5L, mad = FALSE, median = FALSE,
                         digits = 7L, exp = FALSE, level = 95,
                         codebook = FALSE, ...) {
  # Atomic vectors (numeric, integer, logical, character, factor) are routed
  # to the summarize path (or codebook path when codebook = TRUE).
  # Matrices and other dimensioned objects are not supported.
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
    if (isTRUE(codebook)) {
      return(.tula_codebook(df, width = width))
    }
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
tula.data.frame <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                             width = NULL, sep = 5L, mad = FALSE,
                             median = FALSE, digits = 7L, exp = FALSE,
                             codebook = FALSE, ...) {
  if (isTRUE(codebook)) {
    return(.tula_codebook(model, width = width))
  }
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
                            family_label   = NULL,
                            width          = NULL,
                            value_fmts     = character(0L),
                            exp            = FALSE,
                            dep_var        = NULL,
                            exp_label      = NULL,
                            ancillary_df   = NULL,
                            level          = 95,
                            outcome_levels = NULL,
                            se_label       = NULL) {
  structure(
    list(
      model_type     = model_type,
      header_left    = header_left,
      header_right   = header_right,
      coef_df        = coef_df,
      stat_label     = stat_label,
      wide           = wide,
      family_label   = family_label,
      width          = width,
      value_fmts     = value_fmts,
      exp            = exp,
      dep_var        = dep_var,
      exp_label      = exp_label,
      ancillary_df   = ancillary_df,
      level          = level,
      outcome_levels = outcome_levels,
      se_label       = se_label
    ),
    class = "tula_output"
  )
}

# Thin wrapper so methods can pass ref/label through without storing them on
# the output object — they are consumed entirely by build_coef_df().
.parse_tula_opts <- function(ref, label) {
  list(ref = isTRUE(ref), label = isTRUE(label))
}

# Resolve the effective output width at print time.
# - NULL    → read getOption("width") now (reflects current console width)
# - Inf     → pass through (unlimited width)
# - numeric → if < 60, warn and clamp to 60; otherwise use as-is
.resolve_width <- function(width) {
  w <- if (is.null(width)) getOption("width") else width
  if (!is.infinite(w) && w < 60L) {
    warning("width cannot be less than 60; using width = 60.", call. = FALSE)
    w <- 60L
  }
  w
}

# Resolve the effective wide setting at method-dispatch time.
# - NULL  → TRUE if the effective output width >= 80, FALSE otherwise
# - TRUE/FALSE → used as-is
# `width` here is the raw user-supplied width (NULL or a number), resolved
# the same way as .resolve_width() for the threshold comparison only.
.resolve_wide <- function(wide, width) {
  if (!is.null(wide)) return(isTRUE(wide))
  w <- if (is.null(width)) getOption("width") else width
  if (!is.infinite(w) && w < 60L) w <- 60L   # match clamping logic
  w >= 80L
}

# Resolve and validate the confidence interval level.
# - If level < 1, multiply by 100 (so 0.95 → 95).
# - After normalisation, must be between 50 and 99.9 (inclusive).
# Returns the percentage (e.g. 95, 90, 99.9).
.resolve_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level)) {
    stop("level must be a single numeric value.", call. = FALSE)
  }
  if (level < 1) level <- level * 100
  if (level < 50 || level > 99.9) {
    stop("level must be between 50 and 99.9 (or equivalently 0.50 to 0.999).",
         call. = FALSE)
  }
  level
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
                                     width      = NULL,
                                     value_fmts = character(0L),
                                     exp        = FALSE,
                                     parallel   = FALSE,
                                     dep_var    = NULL,
                                     level      = 95,
                                     se_label   = NULL) {
  structure(
    list(
      header_left  = header_left,
      header_right = header_right,
      blocks       = blocks,
      base_outcome = base_outcome,
      stat_label   = stat_label,
      wide         = wide,
      width        = width,
      value_fmts   = value_fmts,
      exp          = exp,
      parallel     = parallel,
      dep_var      = dep_var,
      level        = level,
      se_label     = se_label
    ),
    class = "tula_multinom_output"
  )
}


#' Print method for tula_multinom_output objects
#'
#' Prints Stata-inspired multinomial logit output: a shared header block,
#' then one coefficient table per non-base outcome separated by dashed
#' lines, then a "Base outcome:" footer line. Each outcome label appears
#' on the same line as the column headers (Coef, Std. Err., etc.),
#' in the label-column area, truncated if necessary so the | stays aligned.
#'
#' @param x A `tula_multinom_output` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.tula_multinom_output <- function(x, ...) {
  max_w <- .resolve_width(x$width)
  vf    <- if (is.null(x$value_fmts)) character(0L) else x$value_fmts

  # Compute total width from the shared header and the widest label across
  # all outcome blocks.
  all_labels <- unlist(lapply(x$blocks, function(b) b$coef_df$label))

  natural_width <- compute_total_width(
    header_left  = x$header_left,
    header_right = x$header_right,
    coef_labels  = all_labels,
    wide         = x$wide,
    value_fmts   = vf
  )
  # For parallel output, derive the label-column width the same way the
  # stacked layout would (natural_width capped at max_w, minus the stacked
  # numeric-column block), then set total_width = lbl_w + parallel numeric
  # columns.  This keeps "|" at the same horizontal position as in any other
  # tula output for the same model.  Cap at max_w if needed; the table
  # function will error if lbl_w < 8.  For stacked, cap at natural_width.
  if (isTRUE(x$parallel)) {
    n_out_p            <- length(x$blocks)
    cw_col_p           <- .parallel_nz_w(x$blocks, isTRUE(x$exp)) + 3L  # nz_w + star_w
    stacked_num_cols_w <- 2L + 10L + 1L + 10L + 1L + 10L + 1L + 9L  # = 44
    parallel_num_w     <- 2L + n_out_p * cw_col_p + (n_out_p - 1L) * 2L
    lbl_w_natural      <- min(natural_width, max_w) - stacked_num_cols_w
    total_width        <- min(lbl_w_natural + parallel_num_w, max_w)
  } else {
    total_width <- min(natural_width, max_w)
  }

  sep_line <- char_rep(.BOX_H, total_width)

  # Derive the label-column width (mirrors format_coef_table internals).
  # num_cols_w: " |"(2) + coef(10) sp(1) + se(10) sp(1) + stat(10) sp(1) + pval(9)
  num_cols_w <- 2L + 10L + 1L + 10L + 1L + 10L + 1L + 9L
  if (x$wide) num_cols_w <- num_cols_w + 1L + 10L + 1L + 10L
  lbl_w <- total_width - num_cols_w

  # Shared header (printed once above all outcome blocks / parallel table)
  header_lines <- format_header(x$header_left, x$header_right,
                                total_width = total_width,
                                value_fmts  = vf)
  cat(paste(header_lines, collapse = "\n"), "\n", sep = "")

  if (isTRUE(x$parallel)) {
    # --- Parallel layout: all non-base outcomes as side-by-side columns -----
    # .format_parallel_multinom_table() raises an informative error if there
    # are too many outcome categories to fit within total_width.
    table_lines <- .format_parallel_multinom_table(
      blocks      = x$blocks,
      dep_var     = x$dep_var,
      total_width = total_width,
      exp         = isTRUE(x$exp)
    )
    cat(paste(table_lines, collapse = "\n"), "\n", sep = "")
    cat("Base outcome: ", x$base_outcome, "\n", sep = "")
    cat("* p<0.05  ** p<0.01  *** p<0.001\n")

  } else {
    # --- Stacked layout: one coefficient block per non-base outcome ----------
    # format_coef_table() returns: [sep, hdr, sep, ...rows..., sep]
    #
    # Layout per block:
    #   sep
    #   <outcome label>  |  Coef  Std. Err.  z  P>|z|  ...
    #   sep
    #   ...rows...
    #   sep   <- serves as both block closer and next-block opener
    #
    # The outcome label is embedded in the label-column area of the column-
    # header row (inner_lines[1]), replacing the blank padding.
    for (i in seq_along(x$blocks)) {
      blk <- x$blocks[[i]]
      table_lines <- format_coef_table(blk$coef_df, x$stat_label, x$wide,
                                       total_width = total_width,
                                       exp       = isTRUE(x$exp),
                                       exp_label = x$exp_label,
                                       level     = x$level %||% 95L,
                                       se_label  = x$se_label)
      # Drop first sep (replaced by sep_line below) and last sep (printed
      # manually after the loop so there is always a final separator before
      # the "Base outcome:" footer).
      inner_lines <- table_lines[-c(1L, length(table_lines))]

      # Embed the outcome label in the label-column area of the header row.
      outcome_lbl <- .truncate_label(blk$outcome, lbl_w)
      hdr_line    <- inner_lines[1L]
      inner_lines[1L] <- paste0(pad_right(outcome_lbl, lbl_w),
                                substring(hdr_line, lbl_w + 1L))

      cat(sep_line, "\n", sep = "")
      cat(paste(inner_lines, collapse = "\n"), "\n", sep = "")
    }
    cat(sep_line, "\n", sep = "")
    cat("Base outcome: ", x$base_outcome, "\n", sep = "")
  }

  invisible(x)
}


#' Print method for tula_output objects
#'
#' Assembles and prints the full Stata-inspired output. Called automatically
#' when a `tula_output` object is returned to the console.
#'
#' @param x A `tula_output` object (from `tula()`).
#' @param ... Ignored (for S3 compatibility).
#'
#' @return Invisibly returns `x`.
#' @export
print.tula_output <- function(x, ...) {
  # Resolve width: NULL → getOption("width") at print time; validates >= 60.
  max_w <- .resolve_width(x$width)
  vf    <- if (is.null(x$value_fmts)) character(0L) else x$value_fmts

  # Include ancillary labels and dep_var (if any) in width computation
  anc_labels <- if (!is.null(x$ancillary_df)) x$ancillary_df$label else character(0)
  dep_label  <- if (!is.null(x$dep_var) && nchar(x$dep_var) > 0L) x$dep_var else character(0)
  all_labels <- c(x$coef_df$label, anc_labels, dep_label)

  natural_width <- compute_total_width(
    header_left  = x$header_left,
    header_right = x$header_right,
    coef_labels  = all_labels,
    wide         = x$wide,
    value_fmts   = vf
  )
  total_width <- min(natural_width, max_w)

  # Optional family/link line (glm only)
  if (!is.null(x$family_label)) {
    cat(x$family_label, "\n", sep = "")
  }

  # Two-column header block
  header_lines <- format_header(x$header_left, x$header_right,
                                total_width = total_width,
                                value_fmts  = vf)
  cat(paste(header_lines, collapse = "\n"), "\n", sep = "")

  # Coefficient table
  lv <- x$level %||% 95L
  table_lines <- format_coef_table(x$coef_df, x$stat_label, x$wide,
                                   total_width = total_width,
                                   exp       = isTRUE(x$exp),
                                   exp_label = x$exp_label,
                                   level     = lv,
                                   se_label  = x$se_label)

  # Embed the dependent variable name in the label-column area of the column
  # header row (table_lines[2]), mirroring the multinom outcome-label pattern.
  if (!is.null(x$dep_var) && nchar(x$dep_var) > 0L) {
    num_cols_w <- 2L + 10L + 1L + 10L + 1L + 10L + 1L + 9L
    if (x$wide) num_cols_w <- num_cols_w + 1L + 10L + 1L + 10L
    lbl_w   <- total_width - num_cols_w
    dep_lbl <- .truncate_label(x$dep_var, lbl_w)
    hdr_line        <- table_lines[2L]
    table_lines[2L] <- paste0(pad_right(dep_lbl, lbl_w),
                              substring(hdr_line, lbl_w + 1L))
  }

  # Splice ancillary parameter rows (if any) before the final separator.
  # table_lines ends with a separator; pop it, add ancillary lines, re-append.
  if (!is.null(x$ancillary_df) && nrow(x$ancillary_df) > 0L) {
    anc_lines <- format_ancillary_rows(x$ancillary_df, x$wide, total_width,
                                       level = lv)
    n_tl <- length(table_lines)
    table_lines <- c(table_lines[-n_tl], anc_lines, table_lines[n_tl])
  }

  cat(paste(table_lines, collapse = "\n"), "\n", sep = "")

  # Ordered-model footer: lowest and highest outcome levels, truncated to fit.
  if (!is.null(x$outcome_levels) && length(x$outcome_levels) >= 2L) {
    lo  <- as.character(x$outcome_levels[1L])
    hi  <- as.character(x$outcome_levels[length(x$outcome_levels)])
    prefix  <- "Lowest level: "
    sep_str <- ", Highest: "
    avail <- total_width - nchar(prefix) - nchar(sep_str)
    lo_w  <- max(4L, floor(avail / 2L))
    hi_w  <- max(4L, avail - lo_w)
    cat(prefix, .truncate_label(lo, lo_w), sep_str,
        .truncate_label(hi, hi_w), "\n", sep = "")
  }

  invisible(x)
}

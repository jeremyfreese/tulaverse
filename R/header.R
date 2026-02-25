#' @keywords internal
#'
#' Compute the minimum value-field width needed for a set of header values.
#'
#' Formats each value at width=1 (i.e. trimmed to its natural width), then
#' returns the width of the widest result. This gives the tightest right-
#' aligned value column that fits all values in the block without wasted space.
#'
#' @param vals Numeric vector of values to display (named).
#' @param fmts Named character vector of format overrides (e.g.
#'   `c("Log likelihood" = "f3")`). Names match `names(vals)`. Missing entries
#'   use the default `"g4"` format.
#' @return Integer. Minimum value-field width for this block.
.header_val_width <- function(vals, fmts = character(0L)) {
  if (length(vals) == 0L) return(0L)
  # fmt_header_val with width=1 returns the value at its natural width
  # (no space flag, so no extra leading space for positives).
  formatted <- mapply(
    function(x, nm) {
      fmt <- if (!is.null(nm) && nm %in% names(fmts)) fmts[[nm]] else "g4"
      fmt_header_val(x, width = 1L, fmt = fmt)
    },
    vals, names(vals),
    USE.NAMES = FALSE
  )
  max(nchar(formatted))
}


#' @keywords internal
#'
#' Format the two-column header block (Stata-inspired layout).
#'
#' Renders a left column (model fit stats) and a right column (sample stats)
#' side by side, with exactly `total_width` characters per line.
#'
#' Label widths are tight (just wide enough for the longest label name).
#' Value widths are tight per block (just wide enough for the widest formatted
#' value in that block), with values right-aligned. The gap between the two
#' blocks is widened as needed to reach total_width.
#'
#' @param header_left  Named numeric vector. Names are display labels, values
#'   are the numbers to show.
#' @param header_right Named numeric vector. Same structure as header_left.
#' @param total_width  Integer. Exact total character width for every output
#'   line. Must equal the coefficient table width.
#' @param col_gap_min  Integer. Minimum spaces between the two header columns
#'   (default 3). The actual gap may be wider if total_width requires it.
#' @param value_fmts   Named character vector of format overrides keyed by
#'   header label name (e.g. `c("Log likelihood" = "f3")`). Missing entries
#'   use the default `"g4"` format (4 significant digits). `"f3"` uses fixed
#'   notation with up to 3 decimal places.
#'
#' @return Character vector, one element per line, each exactly total_width
#'   characters wide.
format_header <- function(header_left, header_right,
                          total_width, col_gap_min = 3L,
                          value_fmts = character(0L)) {
  # Tight label widths: just wide enough for the longest label name
  left_lbl_w  <- if (length(header_left)  > 0L) max(nchar(names(header_left)))  else 0L
  right_lbl_w <- if (length(header_right) > 0L) max(nchar(names(header_right))) else 0L

  # Tight value widths: just wide enough for the widest formatted value
  left_val_w  <- .header_val_width(header_left,  fmts = value_fmts)
  right_val_w <- .header_val_width(header_right, fmts = value_fmts)

  # Block widths: label + " = " (3) + value
  left_block_w  <- left_lbl_w  + 3L + left_val_w
  right_block_w <- right_lbl_w + 3L + right_val_w

  # Expand the gap to hit total_width exactly
  col_gap <- total_width - left_block_w - right_block_w
  col_gap <- max(col_gap, col_gap_min)

  # Format one "label = value" pair with its block's specific widths
  fmt_pair <- function(nm, val, lbl_w, val_w) {
    fmt <- if (nm %in% names(value_fmts)) value_fmts[[nm]] else "g4"
    sprintf("%s = %s", pad_right(nm, lbl_w), fmt_header_val(val, width = val_w, fmt = fmt))
  }

  left_lines <- if (length(header_left) > 0L) {
    mapply(fmt_pair, names(header_left), header_left,
           MoreArgs = list(lbl_w = left_lbl_w, val_w = left_val_w),
           USE.NAMES = FALSE)
  } else character(0)

  right_lines <- if (length(header_right) > 0L) {
    mapply(fmt_pair, names(header_right), header_right,
           MoreArgs = list(lbl_w = right_lbl_w, val_w = right_val_w),
           USE.NAMES = FALSE)
  } else character(0)

  # Pad both sides to the same number of rows with blank entries
  n <- max(length(left_lines), length(right_lines), 1L)
  blank_left  <- strrep(" ", left_block_w)
  blank_right <- strrep(" ", right_block_w)

  left_lines  <- c(left_lines,  rep(blank_left,  n - length(left_lines)))
  right_lines <- c(right_lines, rep(blank_right, n - length(right_lines)))

  gap <- strrep(" ", col_gap)
  paste0(left_lines, gap, right_lines)
}


#' @keywords internal
#'
#' Compute the shared total output width for header and coefficient table.
#'
#' This is the single source of truth for the width used by both
#' format_header() and format_coef_table(). Both blocks will have identical
#' total widths, ensuring their right edges are perfectly flush.
#'
#' Algorithm:
#'   1. Compute minimum header width using tight label AND tight value widths,
#'      plus the minimum inter-column gap.
#'   2. Compute minimum table width from longest coef label + numeric columns.
#'   3. total_width = max of the two minimums.
#'
#' @param header_left  Named numeric vector (left header block).
#' @param header_right Named numeric vector (right header block).
#' @param coef_labels  Character vector of all coefficient display labels.
#' @param wide         Logical. TRUE if CI columns will be shown.
#' @param col_gap_min  Integer. Minimum gap between header columns (default 3).
#' @param value_fmts   Named character vector of format overrides (see
#'   `format_header()`). Must match what is passed to `format_header()`.
#'
#' @return Integer. The shared total width for the output.
compute_total_width <- function(header_left, header_right, coef_labels,
                                wide, col_gap_min = 3L,
                                value_fmts = character(0L)) {
  # Tight label widths
  left_lbl_w  <- if (length(header_left)  > 0L) max(nchar(names(header_left)))  else 0L
  right_lbl_w <- if (length(header_right) > 0L) max(nchar(names(header_right))) else 0L

  # Tight value widths (same logic as format_header uses)
  left_val_w  <- .header_val_width(header_left,  fmts = value_fmts)
  right_val_w <- .header_val_width(header_right, fmts = value_fmts)

  left_block_w  <- left_lbl_w  + 3L + left_val_w
  right_block_w <- right_lbl_w + 3L + right_val_w
  header_min_w  <- left_block_w + col_gap_min + right_block_w

  # Numeric portion of the table:
  # " |" (2) + coef(10) sp(1) + se(10) sp(1) + stat(10) sp(1) + pval(9)
  num_cols_w <- 2L + 10L + 1L + 10L + 1L + 10L + 1L + 9L
  if (wide) num_cols_w <- num_cols_w + 1L + 10L + 1L + 10L

  # Minimum table width = longest coef label + numeric columns
  min_coef_lbl_w <- max(nchar(coef_labels), 1L, na.rm = TRUE)
  table_min_w    <- min_coef_lbl_w + num_cols_w

  max(header_min_w, table_min_w)
}

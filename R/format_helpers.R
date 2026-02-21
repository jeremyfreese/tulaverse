#' @keywords internal
#'
#' Format a numeric value for coefficient table columns.
#'
#' Uses formatC with 'g' format (significant figures, drops trailing zeros).
#' Returns a blank string of the same width for NA values.
#'
#' @param x Numeric scalar.
#' @param digits Number of significant digits (default 4).
#' @param width Column width in characters (default 10).
#' @return Character string of exactly `width` characters.
fmt_num <- function(x, digits = 4, width = 10) {
  if (is.na(x)) return(strrep(" ", width))
  formatC(x, digits = digits, format = "g", width = width, flag = " ")
}

#' @keywords internal
#'
#' Format a p-value for display.
#'
#' Shows "< .0001" when p < 0.0001; otherwise 4 decimal places.
#'
#' @param p Numeric scalar.
#' @param width Column width in characters (default 8).
#' @return Character string of exactly `width` characters.
fmt_pval <- function(p, width = 8) {
  if (is.na(p)) return(strrep(" ", width))
  s <- if (p < 0.0001) "< .0001" else sprintf("%.4f", p)
  formatC(s, width = width, flag = " ")
}

#' @keywords internal
#'
#' Format a scalar for the header block (AIC, BIC, R2, N, etc.).
#'
#' Integers print without decimals; all others use 'g' format with 4
#' significant digits. Values are right-aligned within the given width.
#' No space flag is used, so the width reflects the true character count
#' (important for computing tight column widths in the header layout).
#'
#' @param x Numeric scalar.
#' @param width Minimum field width (right-aligned). Pass 1 to get the
#'   natural/trimmed width.
#' @return Character string of at least `width` characters, right-aligned.
fmt_header_val <- function(x, width = 10) {
  if (is.na(x) || is.null(x)) return(strrep(" ", width))
  if (is.integer(x) || (is.numeric(x) && x == round(x) && abs(x) < 1e9)) {
    return(formatC(as.integer(x), format = "d", width = width))
  }
  formatC(x, digits = 4, format = "g", width = width)
}

#' @keywords internal
#'
#' Left-align a string in a field of given width (pad right with spaces).
#'
#' @param s Character scalar.
#' @param width Integer field width.
#' @return Character string of exactly `width` characters.
pad_right <- function(s, width) {
  formatC(as.character(s), width = -width, flag = "-")
}

#' @keywords internal
#'
#' Right-align a string in a field of given width (pad left with spaces).
#'
#' @param s Character scalar.
#' @param width Integer field width.
#' @return Character string of exactly `width` characters.
pad_left <- function(s, width) {
  formatC(as.character(s), width = width, flag = " ")
}

#' @keywords internal
#'
#' Repeat a character `n` times to build separator lines.
#'
#' @param char Single character.
#' @param n Integer count.
#' @return Character scalar.
char_rep <- function(char, n) {
  paste(rep(char, n), collapse = "")
}

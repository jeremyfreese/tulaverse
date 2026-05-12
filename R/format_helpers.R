# --- Box-drawing characters ------------------------------------------------
# Unicode box-drawing characters for smooth table borders.
# To revert to ASCII, change these three lines to:  "-", "|", "+"
.BOX_H     <- "\u2500"   # ─  horizontal line
.BOX_V     <- "\u2502"   # │  vertical line
.BOX_CROSS <- "\u253C"   # ┼  intersection / junction


# Strip the leading zero from a formatted numeric string.
#
# Converts "0.43" -> ".43", "-0.43" -> "-.43". Leaves strings that do not
# match (integers, scientific notation, "<.0001", etc.) unchanged.
# Applied after formatting so the column-width padding is recalculated
# against the shorter string.
.strip_lead_zero <- function(s) {
  # Positive: " 0." or "0." at the start (may be preceded by spaces)
  s <- sub("^( *)0\\.", "\\1.", s)
  # Negative: "-0." anywhere (only occurs right after optional spaces)
  s <- sub("^( *)-0\\.", "\\1-.", s)
  s
}

# Format a numeric value for coefficient table columns.
#
# Uses formatC with 'g' format (significant figures, drops trailing zeros).
# Returns a blank string of the same width for NA values.
# Leading zeros are suppressed ("0.43" -> ".43").
#
# x      - numeric scalar
# digits - number of significant digits (default 4)
# width  - column width in characters (default 10)
# Returns: character string of exactly `width` characters.
fmt_num <- function(x, digits = 4, width = 10) {
  if (is.na(x)) return(strrep(" ", width))
  s <- formatC(x, digits = digits, format = "fg", flag = " ")
  # Fall back to scientific notation if fixed form overflows column width
  if (nchar(trimws(s)) > width)
    s <- formatC(x, digits = digits, format = "g", flag = " ")
  s <- .strip_lead_zero(s)
  formatC(s, width = width, flag = " ")
}

# Format a p-value for display.
#
# Shows "<.0001" when p < 0.0001; otherwise 4 decimal places.
# Leading zeros are suppressed ("0.0324" -> ".0324").
#
# p     - numeric scalar
# width - column width in characters (default 8)
# Returns: character string of exactly `width` characters.
fmt_pval <- function(p, width = 8) {
  if (is.na(p)) return(strrep(" ", width))
  s <- if (p < 0.0001) "<.0001" else sprintf("%.4f", p)
  s <- .strip_lead_zero(s)
  formatC(s, width = width, flag = " ")
}

# Format a scalar for the header block (AIC, BIC, R2, N, etc.).
#
# Integers print without decimals; all others use 'g' format with 4
# significant digits. Values are right-aligned within the given width.
# No space flag is used, so the width reflects the true character count
# (important for computing tight column widths in the header layout).
#
# x     - numeric scalar
# width - minimum field width (right-aligned). Pass 1 for natural width.
# fmt   - format override: "g4" (default) uses 4 significant digits;
#         "f3" uses fixed notation with up to 3 decimal places (suitable
#         for log-likelihoods).
# Returns: character string of at least `width` characters, right-aligned.
fmt_header_val <- function(x, width = 10, fmt = "g4") {
  if (is.na(x) || is.null(x)) return(strrep(" ", width))
  if (fmt == "f3") {
    return(formatC(round(x, 3L), format = "f", digits = 3L, width = width))
  }
  if (is.integer(x) || (is.numeric(x) && x == round(x) && abs(x) < 1e9)) {
    return(formatC(as.integer(x), format = "d", width = width))
  }
  s <- formatC(x, digits = 4, format = "fg", width = width)
  # Fall back to scientific notation if fixed form overflows column width
  if (nchar(trimws(s)) > width)
    s <- formatC(x, digits = 4, format = "g", width = width)
  s
}

# Left-align a string in a field of given width (pad right with spaces).
pad_right <- function(s, width) {
  formatC(as.character(s), width = -width, flag = "-")
}

# Right-align a string in a field of given width (pad left with spaces).
pad_left <- function(s, width) {
  formatC(as.character(s), width = width, flag = " ")
}

# Repeat a character `n` times to build separator lines.
char_rep <- function(char, n) {
  paste(rep(char, n), collapse = "")
}

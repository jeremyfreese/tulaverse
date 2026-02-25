# ---------------------------------------------------------------------------
# tulatab() internal helpers — tula tabulate path
#
# These functions build and format Stata-style frequency tables.
# All are internal (no roxygen export).
# ---------------------------------------------------------------------------


# Detect the variable type for tabulation.
#
# Returns one of: "haven_labelled", "ordered", "factor", "character", "numeric"
# Haven-labelled must be checked first because it inherits from numeric.
.detect_var_type <- function(vec) {
  if (inherits(vec, "haven_labelled")) return("haven_labelled")
  if (is.ordered(vec)) return("ordered")
  if (is.factor(vec))  return("factor")
  if (is.character(vec)) return("character")
  if (is.numeric(vec)) return("numeric")
  stop("tulatab() does not support objects of class '",
       paste(class(vec), collapse = "', '"), "'.", call. = FALSE)
}


# Simple right-truncation for tab labels.
# Unlike .truncate_label() (word-boundary logic), this just cuts and appends ~.
.truncate_tab_label <- function(lbl, width) {
  if (nchar(lbl) <= width) return(lbl)
  paste0(substr(lbl, 1L, width - 1L), "~")
}


# Build the canonical tab_df data frame from a vector.
#
# Returns a data frame with 7 columns:
#   value, num_value, label, freq, percent, cum, is_missing
#
# Parameters:
#   vec      - the data vector
#   var_type - character from .detect_var_type()
#   missing  - logical: include NA rows?
#   sort     - logical: sort by descending frequency?
#   show_cum - logical: compute cumulative percentages?
#   show_value - logical: display numeric codes (haven)?
#   show_label - logical: display haven value labels?
.build_tab_df <- function(vec, var_type, missing, sort, show_cum,
                          show_value = TRUE, show_label = TRUE) {

  tab_df <- switch(var_type,
    "ordered"        = .tab_factor(vec, missing),
    "factor"         = .tab_factor(vec, missing),
    "character"      = .tab_character(vec, missing),
    "numeric"        = .tab_numeric(vec, missing),
    "haven_labelled" = .tab_haven(vec, missing, show_value, show_label),
    stop("Unsupported var_type: ", var_type, call. = FALSE)
  )

  # Sort: default order is already set per type.
  # sort = TRUE overrides to descending frequency (missing rows stay at bottom).
  if (isTRUE(sort)) {
    non_miss <- tab_df[!tab_df$is_missing, , drop = FALSE]
    miss     <- tab_df[ tab_df$is_missing, , drop = FALSE]
    non_miss <- non_miss[order(-non_miss$freq), , drop = FALSE]
    tab_df   <- rbind(non_miss, miss)
    rownames(tab_df) <- NULL
  }

  # Percentages (always sum to 100%)
  total <- sum(tab_df$freq)
  tab_df$percent <- if (total > 0L) tab_df$freq / total * 100 else rep(0, nrow(tab_df))

  # Cumulative percentages (only for non-missing rows when show_cum is TRUE)
  if (show_cum) {
    non_miss_idx <- which(!tab_df$is_missing)
    miss_idx     <- which(tab_df$is_missing)
    tab_df$cum   <- NA_real_
    if (length(non_miss_idx) > 0L) {
      tab_df$cum[non_miss_idx] <- cumsum(tab_df$percent[non_miss_idx])
    }
    # Missing rows also get cumulative values (continuing from non-missing)
    if (length(miss_idx) > 0L) {
      base_cum <- if (length(non_miss_idx) > 0L) {
        tab_df$cum[non_miss_idx[length(non_miss_idx)]]
      } else 0
      tab_df$cum[miss_idx] <- base_cum + cumsum(tab_df$percent[miss_idx])
    }
  } else {
    tab_df$cum <- NA_real_
  }

  tab_df
}


# --- Per-type tabulation helpers -------------------------------------------

# Factor (ordered or unordered)
.tab_factor <- function(vec, missing) {
  use_na <- if (missing) "ifany" else "no"
  tbl    <- table(vec, useNA = use_na)
  vals   <- names(tbl)
  freqs  <- as.integer(tbl)

  # Tag missing rows
  is_miss <- is.na(vals)
  vals[is_miss] <- "."

  data.frame(
    value      = vals,
    num_value  = NA_real_,
    label      = NA_character_,
    freq       = freqs,
    percent    = NA_real_,
    cum        = NA_real_,
    is_missing = is_miss,
    stringsAsFactors = FALSE
  )
}


# Character
.tab_character <- function(vec, missing) {
  use_na <- if (missing) "ifany" else "no"
  tbl    <- table(vec, useNA = use_na)
  vals   <- names(tbl)
  freqs  <- as.integer(tbl)

  is_miss <- is.na(vals)
  vals[is_miss] <- "."

  # Default order for character: alphabetical (non-missing), then missing at bottom
  # table() already alphabetises for character; missing (if any) is appended last by table.

  data.frame(
    value      = vals,
    num_value  = NA_real_,
    label      = NA_character_,
    freq       = freqs,
    percent    = NA_real_,
    cum        = NA_real_,
    is_missing = is_miss,
    stringsAsFactors = FALSE
  )
}


# Plain numeric
.tab_numeric <- function(vec, missing) {
  use_na <- if (missing) "ifany" else "no"
  tbl    <- table(vec, useNA = use_na)
  vals   <- names(tbl)
  freqs  <- as.integer(tbl)

  is_miss  <- is.na(vals)
  num_vals <- suppressWarnings(as.numeric(vals))
  num_vals[is_miss] <- NA_real_
  vals[is_miss] <- "."

  # Order by numeric value ascending (missing at bottom) — table() default
  # for numeric is already ascending, and NA goes last, so no reorder needed.

  data.frame(
    value      = vals,
    num_value  = num_vals,
    label      = NA_character_,
    freq       = freqs,
    percent    = NA_real_,
    cum        = NA_real_,
    is_missing = is_miss,
    stringsAsFactors = FALSE
  )
}


# Haven-labelled numeric
.tab_haven <- function(vec, missing, show_value, show_label) {
  # Extract haven value labels: named numeric where names = label text,
  # values = numeric codes
  haven_labels <- attr(vec, "labels", exact = TRUE)

  # Build a code -> label lookup
  if (!is.null(haven_labels) && length(haven_labels) > 0L) {
    code_to_label <- stats::setNames(names(haven_labels), as.character(haven_labels))
  } else {
    code_to_label <- character(0)
  }

  # Handle tagged NAs if haven is available
  has_haven <- requireNamespace("haven", quietly = TRUE)

  # Get non-tagged-NA mask and tagged NA info
  if (has_haven) {
    is_tagged_na <- haven::is_tagged_na(vec)
    na_tags      <- ifelse(is_tagged_na, haven::na_tag(vec), NA_character_)
  } else {
    is_tagged_na <- rep(FALSE, length(vec))
    na_tags      <- rep(NA_character_, length(vec))
  }

  is_regular_na <- is.na(vec) & !is_tagged_na

  # Separate the vector into: non-NA values, regular NAs, tagged NAs
  non_na_vec <- vec[!is.na(vec) | is_tagged_na]
  # For tabulation, strip the haven class to get numeric values
  non_na_numeric <- as.numeric(non_na_vec[!is_tagged_na[!is.na(vec) | is_tagged_na]])

  # Actually, let's use a cleaner approach: tabulate all unique values
  # including tagged NAs

  # Step 1: Create a classification vector
  # For each element, create a string key for grouping
  n <- length(vec)
  keys <- character(n)

  for (i in seq_len(n)) {
    if (is_tagged_na[i]) {
      keys[i] <- paste0("tagged_na:", na_tags[i])
    } else if (is_regular_na[i]) {
      keys[i] <- "regular_na"
    } else {
      keys[i] <- paste0("val:", as.character(as.numeric(vec[i])))
    }
  }

  # Step 2: Tabulate
  tbl <- table(keys)
  key_names <- names(tbl)
  freqs     <- as.integer(tbl)

  # Step 3: Build output rows (store raw_code for alignment pass below)
  rows <- vector("list", length(key_names))
  for (k in seq_along(key_names)) {
    key <- key_names[k]

    if (startsWith(key, "val:")) {
      num_val <- as.numeric(sub("^val:", "", key))
      code_str <- as.character(num_val)
      # Integer display: if the numeric value is a whole number, show without decimal
      if (!is.na(num_val) && num_val == as.integer(num_val)) {
        code_str <- as.character(as.integer(num_val))
      }
      lbl_text <- code_to_label[code_str]
      if (is.na(lbl_text)) lbl_text <- NA_character_

      rows[[k]] <- data.frame(
        value      = code_str,   # rebuilt after alignment pass
        raw_code   = code_str,
        num_value  = num_val,
        label      = if (is.na(lbl_text)) NA_character_ else lbl_text,
        freq       = freqs[k],
        percent    = NA_real_,
        cum        = NA_real_,
        is_missing = FALSE,
        stringsAsFactors = FALSE
      )

    } else if (key == "regular_na") {
      rows[[k]] <- data.frame(
        value      = ".",
        raw_code   = ".",
        num_value  = NA_real_,
        label      = NA_character_,
        freq       = freqs[k],
        percent    = NA_real_,
        cum        = NA_real_,
        is_missing = TRUE,
        stringsAsFactors = FALSE
      )

    } else if (startsWith(key, "tagged_na:")) {
      tag      <- sub("^tagged_na:", "", key)
      tag_code <- paste0(".", tag)
      # Look up the label for this tagged NA
      # Tagged NAs in haven have special codes; check if there's a label
      tag_label <- NA_character_
      if (has_haven && !is.null(haven_labels)) {
        # Check if any of the haven_labels values are tagged NAs with this tag
        for (j in seq_along(haven_labels)) {
          if (haven::is_tagged_na(haven_labels[j]) &&
              haven::na_tag(haven_labels[j]) == tag) {
            tag_label <- names(haven_labels)[j]
            break
          }
        }
      }

      rows[[k]] <- data.frame(
        value      = tag_code,   # rebuilt after alignment pass
        raw_code   = tag_code,
        num_value  = NA_real_,
        label      = if (is.na(tag_label)) NA_character_ else tag_label,
        freq       = freqs[k],
        percent    = NA_real_,
        cum        = NA_real_,
        is_missing = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  tab_df <- do.call(rbind, rows)
  rownames(tab_df) <- NULL

  # Exclude missing rows if missing = FALSE
  if (!missing) {
    tab_df <- tab_df[!tab_df$is_missing, , drop = FALSE]
    rownames(tab_df) <- NULL
  }

  # Order: non-missing by num_value ascending, then missing at bottom
  non_miss <- tab_df[!tab_df$is_missing, , drop = FALSE]
  miss     <- tab_df[ tab_df$is_missing, , drop = FALSE]
  non_miss <- non_miss[order(non_miss$num_value), , drop = FALSE]
  tab_df   <- rbind(non_miss, miss)
  rownames(tab_df) <- NULL

  # Alignment pass: right-align the code field so numeric codes (1, 2, ...)
  # and tagged-NA codes (.a, .b, ...) all start at the same column.
  if (show_value && nrow(tab_df) > 0L) {
    max_code_w <- max(nchar(tab_df$raw_code), na.rm = TRUE)
    tab_df$value <- mapply(
      function(rc, lbl) .haven_display_value(pad_left(rc, max_code_w), lbl,
                                             show_value, show_label),
      tab_df$raw_code, tab_df$label,
      SIMPLIFY = TRUE, USE.NAMES = FALSE
    )
  } else {
    # show_value = FALSE: build value from label (or code fallback) unpadded
    tab_df$value <- mapply(
      function(rc, lbl) .haven_display_value(rc, lbl, show_value, show_label),
      tab_df$raw_code, tab_df$label,
      SIMPLIFY = TRUE, USE.NAMES = FALSE
    )
  }

  tab_df$raw_code <- NULL  # drop temporary column
  tab_df
}


# Build haven display value from code and label per show_value/show_label settings
.haven_display_value <- function(code_str, label_text, show_value, show_label) {
  has_label <- !is.na(label_text) && nzchar(label_text)

  if (show_value && show_label && has_label) {
    paste0(code_str, " ", label_text)
  } else if (show_value && !show_label) {
    code_str
  } else if (!show_value && show_label && has_label) {
    label_text
  } else {
    # Both FALSE, or label=TRUE but no label text available: show code
    code_str
  }
}


# Format a frequency count with commas, right-aligned.
.fmt_freq <- function(n, width = 10L) {
  if (is.na(n)) return(strrep(" ", width))
  formatted <- formatC(as.integer(n), format = "d", big.mark = ",")
  formatC(formatted, width = width, flag = " ")
}


# Format a percentage with exactly 2 decimal places, right-aligned.
.fmt_pct <- function(x, width = 10L) {
  if (is.na(x)) return(strrep(" ", width))
  formatted <- sprintf("%.2f", x)
  formatC(formatted, width = width, flag = " ")
}


# Format the complete frequency table as a character vector of lines.
#
# Parameters:
#   tab_obj - a tula_tab S3 object
#
# Returns a character vector, one element per output line.
.format_tab_table <- function(tab_obj) {
  tab_df   <- tab_obj$tab_df
  show_cum <- tab_obj$show_cum
  width    <- .resolve_width(tab_obj$width)

  # Fixed column widths
  cw_freq <- 10L
  cw_pct  <- 10L
  cw_cum  <- 10L

  # Numeric portion width (includes " |" separator)
  # " |" = 1 char for space + 1 for pipe = 2; but actually let's look at Stata:
  # label_area " |" freq sp pct [sp cum]
  # pipe_w = 2 (space + pipe)
  pipe_w <- 2L
  num_w  <- cw_freq + 1L + cw_pct
  if (show_cum) num_w <- num_w + 1L + cw_cum

  total_num_w <- pipe_w + num_w   # total chars from pipe onwards

  # Label column width
  # Use the variable name (or haven var label) for the header
  var_display <- if (!is.null(tab_obj$var_label) && nzchar(tab_obj$var_label)) {
    tab_obj$var_label
  } else {
    tab_obj$var_name
  }

  # Natural label width: max of all display values, "Total", and var_display
  all_display_vals <- tab_df$value
  natural_lbl_w <- max(
    nchar(all_display_vals),
    nchar("Total"),
    nchar(var_display),
    1L,
    na.rm = TRUE
  )
  # Add 2 chars indent for factor/character values (they get "  " prefix)
  # Add 1 char indent for haven_labelled values (they get " " prefix)
  if (tab_obj$var_type %in% c("factor", "ordered", "character")) {
    natural_lbl_w <- max(natural_lbl_w + 2L, nchar(var_display), nchar("Total"))
  } else if (tab_obj$var_type == "haven_labelled") {
    natural_lbl_w <- max(natural_lbl_w + 1L, nchar(var_display), nchar("Total"))
  }

  # Total width = label + numeric; cap at console width
  natural_total_w <- natural_lbl_w + total_num_w
  total_width     <- min(natural_total_w, width)

  # Derive actual label column width

  lbl_w <- total_width - total_num_w
  if (lbl_w < 5L) lbl_w <- 5L   # minimum label width

  # Separators: dashes with + at pipe position
  sep_line <- paste0(char_rep("-", lbl_w + 1L), "+", char_rep("-", num_w))

  # Column header line
  var_hdr <- .truncate_tab_label(var_display, lbl_w)
  if (show_cum) {
    hdr_line <- sprintf(
      "%s |%s %s %s",
      pad_left(var_hdr, lbl_w),
      pad_left("Freq.", cw_freq),
      pad_left("Percent", cw_pct),
      pad_left("Cum.", cw_cum)
    )
  } else {
    hdr_line <- sprintf(
      "%s |%s %s",
      pad_left(var_hdr, lbl_w),
      pad_left("Freq.", cw_freq),
      pad_left("Percent", cw_pct)
    )
  }

  lines <- c(sep_line, hdr_line, sep_line)

  # Data rows
  for (i in seq_len(nrow(tab_df))) {
    row <- tab_df[i, ]

    # Build display label
    display_lbl <- row$value

    # For factor/ordered/character, indent with "  " unless it's a missing row
    if (tab_obj$var_type %in% c("factor", "ordered", "character")) {
      if (row$is_missing) {
        display_lbl <- paste0("  ", display_lbl)
      } else {
        display_lbl <- paste0("  ", display_lbl)
      }
    } else if (tab_obj$var_type == "haven_labelled") {
      # Haven values: add a leading space for visual separation from pipe
      display_lbl <- paste0(" ", display_lbl)
    } else if (tab_obj$var_type == "numeric") {
      # Numeric values: right-align in the label column
      # (handled below via pad_left)
    }

    # Truncate if needed
    display_lbl <- .truncate_tab_label(display_lbl, lbl_w)

    # Alignment: haven and numeric use right-alignment (pad_left);
    # factor/ordered/character use left-alignment (pad_right)
    if (tab_obj$var_type %in% c("numeric")) {
      lbl_fmt <- pad_left(display_lbl, lbl_w)
    } else {
      lbl_fmt <- pad_right(display_lbl, lbl_w)
    }

    if (show_cum) {
      line <- sprintf(
        "%s |%s %s %s",
        lbl_fmt,
        .fmt_freq(row$freq, cw_freq),
        .fmt_pct(row$percent, cw_pct),
        .fmt_pct(row$cum, cw_cum)
      )
    } else {
      line <- sprintf(
        "%s |%s %s",
        lbl_fmt,
        .fmt_freq(row$freq, cw_freq),
        .fmt_pct(row$percent, cw_pct)
      )
    }

    lines <- c(lines, line)
  }

  # Separator before total
  lines <- c(lines, sep_line)

  # Total row
  total_freq <- sum(tab_df$freq)
  total_lbl  <- pad_left("Total", lbl_w)

  if (show_cum) {
    total_line <- sprintf(
      "%s |%s %s %s",
      total_lbl,
      .fmt_freq(total_freq, cw_freq),
      .fmt_pct(100, cw_pct),
      .fmt_pct(100, cw_cum)
    )
  } else {
    total_line <- sprintf(
      "%s |%s %s",
      total_lbl,
      .fmt_freq(total_freq, cw_freq),
      .fmt_pct(100, cw_pct)
    )
  }

  lines <- c(lines, total_line)

  lines
}

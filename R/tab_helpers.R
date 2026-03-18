# ---------------------------------------------------------------------------
# tulatab() internal helpers — tula tabulate path
#
# These functions build and format Stata-inspired frequency tables.
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


# Compute per-category means and Ns for one-way mean= mode.
#
# Returns a list with:
#   mean_vals  - numeric vector aligned to tab_df rows
#   n_vals     - integer vector aligned to tab_df rows
#   mean_total - overall mean (scalar)
#   n_total    - overall N (scalar)
.compute_oneway_means <- function(vec, mean_vec, tab_df, var_type) {
  n_rows    <- nrow(tab_df)
  mean_vals <- rep(NA_real_, n_rows)
  n_vals    <- rep(0L, n_rows)

  # Compute per-group means via tapply, then align to tab_df rows.
  # The alignment key depends on variable type.

  # Handle non-missing observations
  if (var_type == "haven_labelled") {
    # Group by underlying numeric codes
    raw_codes     <- as.numeric(unclass(vec))
    non_na        <- !is.na(vec)  # haven NAs (regular + tagged) are NA
    grp_means     <- tapply(mean_vec[non_na], raw_codes[non_na], mean, na.rm = TRUE)
    grp_ns        <- tapply(!is.na(mean_vec[non_na]), raw_codes[non_na], sum)
  } else if (var_type == "numeric") {
    non_na    <- !is.na(vec)
    grp_means <- tapply(mean_vec[non_na], vec[non_na], mean, na.rm = TRUE)
    grp_ns    <- tapply(!is.na(mean_vec[non_na]), vec[non_na], sum)
  } else {
    # factor, ordered, character — tapply on the vector gives names = level names
    non_na    <- !is.na(vec)
    grp_means <- tapply(mean_vec[non_na], as.character(vec[non_na]), mean, na.rm = TRUE)
    grp_ns    <- tapply(!is.na(mean_vec[non_na]), as.character(vec[non_na]), sum)
  }

  # Align tapply results to tab_df rows
  for (i in seq_len(n_rows)) {
    row <- tab_df[i, ]

    if (row$is_missing) {
      # Missing row: compute from observations where vec is NA
      na_mask <- is.na(vec)
      vals    <- mean_vec[na_mask]
      n_vals[i]    <- sum(!is.na(vals))
      mean_vals[i] <- if (n_vals[i] > 0L) mean(vals, na.rm = TRUE) else NA_real_
    } else if (var_type %in% c("numeric", "haven_labelled") && !is.na(row$num_value)) {
      key <- as.character(row$num_value)
      if (key %in% names(grp_means)) {
        mean_vals[i] <- grp_means[[key]]
        n_vals[i]    <- as.integer(grp_ns[[key]])
      }
    } else {
      # factor / ordered / character: match by trimmed display label
      key <- trimws(row$value)
      if (key %in% names(grp_means)) {
        mean_vals[i] <- grp_means[[key]]
        n_vals[i]    <- as.integer(grp_ns[[key]])
      }
    }
  }

  # Replace NaN with NA
  mean_vals[is.nan(mean_vals)] <- NA_real_

  # Overall
  mean_total <- mean(mean_vec, na.rm = TRUE)
  if (is.nan(mean_total)) mean_total <- NA_real_
  n_total    <- sum(!is.na(mean_vec))

  list(
    mean_vals  = mean_vals,
    n_vals     = n_vals,
    mean_total = mean_total,
    n_total    = as.integer(n_total)
  )
}


# Compute per-category means and Ns for one-way mean= mode with multiple
# mean variables and listwise deletion support.
#
# Returns a list with:
#   mean_mat    - numeric matrix (nrow = nrow(tab_df), ncol = n_vars)
#   n_mat       - integer matrix (same dims)
#   mean_totals - numeric vector (one grand mean per variable)
#   n_totals    - integer vector (one grand N per variable)
.compute_oneway_multi_means <- function(vec, mean_list, tab_df, var_type,
                                         listwise = TRUE) {
  n_vars <- length(mean_list)
  n_rows <- nrow(tab_df)

  # Build a complete-cases mask when listwise = TRUE
  if (listwise && n_vars > 1L) {
    complete <- rep(TRUE, length(vec))
    for (k in seq_len(n_vars)) {
      complete <- complete & !is.na(mean_list[[k]])
    }
  } else {
    complete <- NULL
  }

  mean_mat <- matrix(NA_real_, nrow = n_rows, ncol = n_vars)
  n_mat    <- matrix(0L,       nrow = n_rows, ncol = n_vars)
  mean_totals <- rep(NA_real_, n_vars)
  n_totals    <- rep(0L, n_vars)

  for (k in seq_len(n_vars)) {
    mv <- mean_list[[k]]

    # Apply listwise mask: set values to NA where any variable is missing
    if (!is.null(complete)) {
      mv_use <- ifelse(complete, mv, NA_real_)
    } else {
      mv_use <- mv
    }

    # Compute per-group via the existing single-variable helper's logic
    info_k <- .compute_oneway_means(vec, mv_use, tab_df, var_type)
    mean_mat[, k] <- info_k$mean_vals
    n_mat[, k]    <- info_k$n_vals
    mean_totals[k] <- info_k$mean_total
    n_totals[k]    <- info_k$n_total
  }

  colnames(mean_mat) <- names(mean_list)
  colnames(n_mat)    <- names(mean_list)

  list(
    mean_mat    = mean_mat,
    n_mat       = n_mat,
    mean_totals = mean_totals,
    n_totals    = n_totals
  )
}


# Format a frequency count with commas, right-aligned.
.fmt_freq <- function(n, width = 10L) {
  if (is.na(n)) return(strrep(" ", width))
  formatted <- formatC(as.integer(n), format = "d", big.mark = ",")
  formatC(formatted, width = width, flag = " ")
}


# Format a percentage with a specified number of decimal places, right-aligned.
.fmt_pct <- function(x, width = 10L, dec = 2L) {
  if (is.na(x)) return(strrep(" ", width))
  formatted <- sprintf("%.*f", dec, x)
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

  # Resolve dec: NULL -> 2 for pct, 3 for mean
  pct_dec  <- if (!is.null(tab_obj$dec)) tab_obj$dec else 2L
  mean_dec <- tab_obj$dec   # NULL when user didn't set dec= (smart formatting)

  # --- Stat mode: custom stat columns (stat= was specified) ------------------
  stat_mode <- !is.null(tab_obj$stat_mat)
  if (stat_mode) return(.format_tab_stat_table(tab_obj, tab_df, width, dec = mean_dec))

  # --- Mean mode: completely different column layout -------------------------
  mean_mode <- !is.null(tab_obj$mean_mat)
  if (mean_mode) return(.format_tab_mean_table(tab_obj, tab_df, width, dec = mean_dec))

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
  sep_line <- paste0(char_rep(.BOX_H, lbl_w + 1L), .BOX_CROSS, char_rep(.BOX_H, num_w))

  # Column header line
  var_hdr <- .truncate_tab_label(var_display, lbl_w)
  if (show_cum) {
    hdr_line <- sprintf(
      paste0("%s ", .BOX_V, "%s %s %s"),
      pad_left(var_hdr, lbl_w),
      pad_left("Freq.", cw_freq),
      pad_left("Percent", cw_pct),
      pad_left("Cum.", cw_cum)
    )
  } else {
    hdr_line <- sprintf(
      paste0("%s ", .BOX_V, "%s %s"),
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
        paste0("%s ", .BOX_V, "%s %s %s"),
        lbl_fmt,
        .fmt_freq(row$freq, cw_freq),
        .fmt_pct(row$percent, cw_pct, dec = pct_dec),
        .fmt_pct(row$cum, cw_cum, dec = pct_dec)
      )
    } else {
      line <- sprintf(
        paste0("%s ", .BOX_V, "%s %s"),
        lbl_fmt,
        .fmt_freq(row$freq, cw_freq),
        .fmt_pct(row$percent, cw_pct, dec = pct_dec)
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
      paste0("%s ", .BOX_V, "%s %s %s"),
      total_lbl,
      .fmt_freq(total_freq, cw_freq),
      .fmt_pct(100, cw_pct, dec = pct_dec),
      .fmt_pct(100, cw_cum, dec = pct_dec)
    )
  } else {
    total_line <- sprintf(
      paste0("%s ", .BOX_V, "%s %s"),
      total_lbl,
      .fmt_freq(total_freq, cw_freq),
      .fmt_pct(100, cw_pct, dec = pct_dec)
    )
  }

  lines <- c(lines, total_line)

  lines
}


# Format a one-way tab table in mean mode (Mean + N columns).
#
# Supports both single-variable (backward-compatible) and multi-variable
# mean modes.  When ncol(mean_mat) == 1, produces the classic layout with
# "Mean of ..." header.  When ncol > 1, adds a "Means" super-header row
# above per-variable column names.
#
# Called from .format_tab_table() when mean_mode is detected.
# Returns a character vector of output lines.
.format_tab_mean_table <- function(tab_obj, tab_df, width, dec = NULL) {
  mean_mat    <- tab_obj$mean_mat      # matrix: nrow(tab_df) x n_vars
  n_mat       <- tab_obj$n_mat         # matrix: same dims
  mean_names  <- tab_obj$mean_names    # character vector
  mean_labels <- tab_obj$mean_labels   # character vector (NA = no label)
  mean_grands <- tab_obj$mean_grands   # list(means = numeric, ns = integer)

  n_vars  <- ncol(mean_mat)
  n_rows  <- nrow(tab_df)
  multi   <- n_vars > 1L

  # --- Detect whether all Ns are identical across variables -----------------
  # When listwise=TRUE (or single variable), all N columns are identical.
  # In that case, show only one N column at the far right.
  shared_n <- multi && .all_n_cols_equal(n_mat, mean_grands$ns)

  # --- Per-variable display names -------------------------------------------
  var_displays <- character(n_vars)
  for (k in seq_len(n_vars)) {
    lbl <- mean_labels[k]
    var_displays[k] <- if (!is.na(lbl) && nzchar(lbl)) lbl else mean_names[k]
  }

  # --- Pre-format all mean and N strings ------------------------------------
  mean_strs <- matrix("", nrow = n_rows, ncol = n_vars)
  n_strs    <- matrix("", nrow = n_rows, ncol = n_vars)
  total_mean_strs <- character(n_vars)
  total_n_strs    <- character(n_vars)

  for (k in seq_len(n_vars)) {
    for (i in seq_len(n_rows)) {
      mean_strs[i, k] <- if (is.na(mean_mat[i, k])) "" else
        trimws(.fmt_sum(mean_mat[i, k], digits = 7L, width = 1L, dec = dec))
      n_strs[i, k] <- formatC(as.integer(n_mat[i, k]),
                               format = "d", big.mark = ",")
    }
    total_mean_strs[k] <- if (is.na(mean_grands$means[k])) "" else
      trimws(.fmt_sum(mean_grands$means[k], digits = 7L, width = 1L, dec = dec))
    total_n_strs[k] <- formatC(as.integer(mean_grands$ns[k]),
                                format = "d", big.mark = ",")
  }

  # --- Column widths --------------------------------------------------------
  cw_mean <- integer(n_vars)
  for (k in seq_len(n_vars)) {
    hdr_text <- if (multi) mean_names[k] else "Mean"
    cw_mean[k] <- max(
      nchar(mean_strs[, k]),
      nchar(total_mean_strs[k]),
      nchar(hdr_text),
      6L,
      na.rm = TRUE
    ) + 2L
  }

  if (shared_n) {
    # Single N column: width based on rightmost variable's N values (all same)
    cw_shared_n <- max(
      nchar(n_strs[, n_vars]),
      nchar(total_n_strs[n_vars]),
      nchar("N"),
      4L,
      na.rm = TRUE
    ) + 2L

    # Layout: pipe(2) + mean1 + '  ' + mean2 + ... + ' ' + N
    num_w <- sum(cw_mean) + (n_vars - 1L) * 2L + 1L + cw_shared_n
  } else {
    # Per-variable N columns
    cw_n <- integer(n_vars)
    for (k in seq_len(n_vars)) {
      cw_n[k] <- max(
        nchar(n_strs[, k]),
        nchar(total_n_strs[k]),
        nchar("N"),
        4L,
        na.rm = TRUE
      ) + 2L
    }

    # Layout: pipe(2) + [mean1 + ' ' + n1] + '  ' + [mean2 + ' ' + n2] + ...
    per_var_w <- cw_mean + 1L + cw_n
    num_w     <- sum(per_var_w) + (n_vars - 1L) * 2L
  }

  pipe_w      <- 2L
  total_num_w <- pipe_w + num_w

  # --- Label column width ---------------------------------------------------
  var_display <- if (!is.null(tab_obj$var_label) && nzchar(tab_obj$var_label)) {
    tab_obj$var_label
  } else {
    tab_obj$var_name
  }

  all_display_vals <- tab_df$value
  natural_lbl_w <- max(
    nchar(all_display_vals),
    nchar("Total"),
    nchar(var_display),
    1L,
    na.rm = TRUE
  )
  if (tab_obj$var_type %in% c("factor", "ordered", "character")) {
    natural_lbl_w <- max(natural_lbl_w + 2L, nchar(var_display), nchar("Total"))
  } else if (tab_obj$var_type == "haven_labelled") {
    natural_lbl_w <- max(natural_lbl_w + 1L, nchar(var_display), nchar("Total"))
  }

  natural_total_w <- natural_lbl_w + total_num_w
  total_width     <- min(natural_total_w, width)
  lbl_w           <- total_width - total_num_w
  if (lbl_w < 5L) lbl_w <- 5L

  # --- Header lines ---------------------------------------------------------
  sep_line <- paste0(char_rep(.BOX_H, lbl_w + 1L), .BOX_CROSS,
                     char_rep(.BOX_H, num_w))

  if (multi) {
    # Multi-variable: "Means" super-header + per-variable column names
    super_padded <- .center_text("Means", num_w)
    super_line <- paste0(strrep(" ", lbl_w + 1L), .BOX_V, super_padded)

    var_hdr <- .truncate_tab_label(var_display, lbl_w)

    if (shared_n) {
      # mean1  mean2  ...  N
      col_parts <- vapply(seq_len(n_vars), function(k)
        pad_left(mean_names[k], cw_mean[k]), character(1L))
      col_data <- paste0(paste(col_parts, collapse = "  "),
                          " ", pad_left("N", cw_shared_n))
    } else {
      # mean1  N  mean2  N  ...
      col_parts <- character(n_vars)
      for (k in seq_len(n_vars)) {
        col_parts[k] <- paste0(pad_left(mean_names[k], cw_mean[k]),
                                " ", pad_left("N", cw_n[k]))
      }
      col_data <- paste(col_parts, collapse = "  ")
    }
    hdr_line <- paste0(pad_left(var_hdr, lbl_w), " ", .BOX_V, col_data)

    lines <- c(super_line, hdr_line, sep_line)
  } else {
    # Single-variable: classic "Mean of ..." header
    mean_display <- var_displays[1L]
    mean_hdr_line <- paste0("Mean of ", mean_display)

    cw_n_single <- max(
      nchar(n_strs[, 1L]),
      nchar(total_n_strs[1L]),
      nchar("N"),
      4L,
      na.rm = TRUE
    ) + 2L

    var_hdr <- .truncate_tab_label(var_display, lbl_w)
    hdr_line <- paste0(pad_left(var_hdr, lbl_w), " ", .BOX_V,
                       pad_left("Mean", cw_mean[1L]), " ",
                       pad_left("N", cw_n_single))

    lines <- c(mean_hdr_line, sep_line, hdr_line, sep_line)
  }

  # --- Data rows ------------------------------------------------------------
  for (i in seq_len(n_rows)) {
    row <- tab_df[i, ]

    display_lbl <- row$value
    if (tab_obj$var_type %in% c("factor", "ordered", "character")) {
      display_lbl <- paste0("  ", display_lbl)
    } else if (tab_obj$var_type == "haven_labelled") {
      display_lbl <- paste0(" ", display_lbl)
    }
    display_lbl <- .truncate_tab_label(display_lbl, lbl_w)

    if (tab_obj$var_type %in% c("numeric")) {
      lbl_fmt <- pad_left(display_lbl, lbl_w)
    } else {
      lbl_fmt <- pad_right(display_lbl, lbl_w)
    }

    if (multi && shared_n) {
      mean_parts <- vapply(seq_len(n_vars), function(k)
        pad_left(mean_strs[i, k], cw_mean[k]), character(1L))
      data_str <- paste0(paste(mean_parts, collapse = "  "),
                          " ", pad_left(n_strs[i, n_vars], cw_shared_n))
    } else if (multi) {
      data_parts <- character(n_vars)
      for (k in seq_len(n_vars)) {
        data_parts[k] <- paste0(pad_left(mean_strs[i, k], cw_mean[k]),
                                 " ", pad_left(n_strs[i, k], cw_n[k]))
      }
      data_str <- paste(data_parts, collapse = "  ")
    } else {
      data_str <- paste0(pad_left(mean_strs[i, 1L], cw_mean[1L]),
                          " ", pad_left(n_strs[i, 1L], cw_n_single))
    }

    lines <- c(lines, paste0(lbl_fmt, " ", .BOX_V, data_str))
  }

  # --- Separator before total -----------------------------------------------
  lines <- c(lines, sep_line)

  # --- Total row ------------------------------------------------------------
  total_lbl <- pad_left("Total", lbl_w)

  if (multi && shared_n) {
    mean_parts <- vapply(seq_len(n_vars), function(k)
      pad_left(total_mean_strs[k], cw_mean[k]), character(1L))
    total_str <- paste0(paste(mean_parts, collapse = "  "),
                         " ", pad_left(total_n_strs[n_vars], cw_shared_n))
  } else if (multi) {
    total_parts <- character(n_vars)
    for (k in seq_len(n_vars)) {
      total_parts[k] <- paste0(pad_left(total_mean_strs[k], cw_mean[k]),
                                " ", pad_left(total_n_strs[k], cw_n[k]))
    }
    total_str <- paste(total_parts, collapse = "  ")
  } else {
    total_str <- paste0(pad_left(total_mean_strs[1L], cw_mean[1L]),
                         " ", pad_left(total_n_strs[1L], cw_n_single))
  }

  lines <- c(lines, paste0(total_lbl, " ", .BOX_V, total_str))

  lines
}


# Check whether all N columns in the matrix are identical (and grand Ns match).
.all_n_cols_equal <- function(n_mat, grand_ns) {
  if (ncol(n_mat) <= 1L) return(FALSE)
  ref_col <- n_mat[, 1L]
  ref_grand <- grand_ns[1L]
  for (k in 2L:ncol(n_mat)) {
    if (!identical(as.integer(n_mat[, k]), as.integer(ref_col))) return(FALSE)
    if (grand_ns[k] != ref_grand) return(FALSE)
  }
  TRUE
}


# Center text within a given width, padding with spaces.
.center_text <- function(text, width) {
  n <- nchar(text)
  if (n >= width) return(text)
  left_pad  <- (width - n) %/% 2L
  right_pad <- width - n - left_pad
  paste0(strrep(" ", left_pad), text, strrep(" ", right_pad))
}


# ---------------------------------------------------------------------------
# stat= support — parse, compute, and render arbitrary summary statistics
# ---------------------------------------------------------------------------

# Parse a stat specification into validated tokens.
#
# Accepts a single string "mean sd n" (space-separated) or a character
# vector c("mean", "sd", "n").  Returns a character vector of tokens.
.parse_stat_tokens <- function(stat) {
  if (length(stat) == 1L) {
    tokens <- strsplit(trimws(stat), "\\s+")[[1L]]
  } else {
    tokens <- stat
  }

  valid_base <- c("mean", "median", "n", "sd")

  for (tok in tokens) {
    if (tok %in% valid_base) next
    if (grepl("^p\\d{1,2}$", tok)) {
      pct <- as.integer(sub("^p", "", tok))
      if (pct >= 1L && pct <= 99L) next
    }
    stop(sprintf(
      "tulatab(): unknown stat '%s'. Valid: mean, median, sd, n, p## (e.g. p25).",
      tok), call. = FALSE)
  }

  tokens
}


# Map a stat token to a display column header.
.stat_display_name <- function(token) {
  switch(token,
    mean   = "Mean",
    median = "Median",
    sd     = "SD",
    n      = "N",
    {
      if (startsWith(token, "p")) toupper(token) else token
    }
  )
}


# Evaluate a single stat on a numeric vector (NA-aware).
.eval_stat <- function(token, vals) {
  vals <- vals[!is.na(vals)]
  n <- length(vals)
  if (n == 0L && token != "n") return(NA_real_)

  if (token == "mean")   return(mean(vals))
  if (token == "median") return(stats::median(vals))
  if (token == "sd")     return(if (n > 1L) stats::sd(vals) else NA_real_)
  if (token == "n")      return(as.numeric(n))
  if (startsWith(token, "p")) {
    pct <- as.numeric(sub("^p", "", token))
    return(stats::quantile(vals, probs = pct / 100, names = FALSE))
  }
  NA_real_
}


# Compute per-category stats for one-way stat= mode.
#
# Returns a list with:
#   stat_mat    - numeric matrix (nrow(tab_df) x n_stats)
#   stat_grands - numeric vector (one grand stat per token)
.compute_oneway_stats <- function(vec, stat_vec, tab_df, var_type,
                                   stat_tokens) {
  n_rows  <- nrow(tab_df)
  n_stats <- length(stat_tokens)
  stat_mat <- matrix(NA_real_, nrow = n_rows, ncol = n_stats)
  colnames(stat_mat) <- stat_tokens

  # Build grouping key (same approach as .compute_oneway_means)
  if (var_type == "haven_labelled") {
    grp_key <- as.character(as.numeric(unclass(vec)))
  } else if (var_type == "numeric") {
    grp_key <- as.character(vec)
  } else {
    grp_key <- as.character(vec)
  }

  non_na   <- !is.na(vec)
  grp_vals <- split(stat_vec[non_na], grp_key[non_na])

  for (i in seq_len(n_rows)) {
    row <- tab_df[i, ]

    if (row$is_missing) {
      vals <- stat_vec[is.na(vec)]
    } else if (var_type %in% c("numeric", "haven_labelled") &&
               !is.na(row$num_value)) {
      key  <- as.character(row$num_value)
      vals <- if (key %in% names(grp_vals)) grp_vals[[key]] else numeric(0)
    } else {
      key  <- trimws(row$value)
      vals <- if (key %in% names(grp_vals)) grp_vals[[key]] else numeric(0)
    }

    for (k in seq_len(n_stats)) {
      stat_mat[i, k] <- .eval_stat(stat_tokens[k], vals)
    }
  }

  # Grand totals
  stat_grands <- vapply(stat_tokens, function(tok) {
    .eval_stat(tok, stat_vec)
  }, numeric(1L))

  list(stat_mat = stat_mat, stat_grands = stat_grands)
}


# Format a one-way tab table in stat= mode (arbitrary stat columns).
#
# Called from .format_tab_table() when stat_mat is present.
# Returns a character vector of output lines.
.format_tab_stat_table <- function(tab_obj, tab_df, width, dec = NULL) {
  stat_mat    <- tab_obj$stat_mat
  stat_names  <- tab_obj$stat_names     # display names: "Mean", "SD", etc.
  stat_tokens <- tab_obj$stat_tokens    # raw tokens: "mean", "sd", etc.
  stat_grands <- tab_obj$stat_grands

  n_stats <- ncol(stat_mat)
  n_rows  <- nrow(tab_df)

  # Variable display name (for header line above table)
  mean_display <- if (!is.null(tab_obj$mean_labels) &&
                      !is.na(tab_obj$mean_labels[1L]) &&
                      nzchar(tab_obj$mean_labels[1L])) {
    tab_obj$mean_labels[1L]
  } else {
    tab_obj$mean_names[1L]
  }

  # --- Pre-format all values ------------------------------------------------
  val_strs   <- matrix("", nrow = n_rows, ncol = n_stats)
  total_strs <- character(n_stats)

  for (k in seq_len(n_stats)) {
    is_n <- (stat_tokens[k] == "n")
    for (i in seq_len(n_rows)) {
      val <- stat_mat[i, k]
      if (is.na(val)) {
        val_strs[i, k] <- ""
      } else if (is_n) {
        val_strs[i, k] <- formatC(as.integer(val), format = "d", big.mark = ",")
      } else {
        val_strs[i, k] <- trimws(.fmt_sum(val, digits = 7L, width = 1L, dec = dec))
      }
    }

    grand <- stat_grands[k]
    if (is.na(grand)) {
      total_strs[k] <- ""
    } else if (is_n) {
      total_strs[k] <- formatC(as.integer(grand), format = "d", big.mark = ",")
    } else {
      total_strs[k] <- trimws(.fmt_sum(grand, digits = 7L, width = 1L, dec = dec))
    }
  }

  # --- Column widths --------------------------------------------------------
  cw <- integer(n_stats)
  for (k in seq_len(n_stats)) {
    cw[k] <- max(
      nchar(val_strs[, k]),
      nchar(total_strs[k]),
      nchar(stat_names[k]),
      4L,
      na.rm = TRUE
    ) + 2L
  }

  # Layout: pipe(2) + stat1 + ' ' + stat2 + ...
  num_w       <- sum(cw) + (n_stats - 1L)
  pipe_w      <- 2L
  total_num_w <- pipe_w + num_w

  # --- Label column width ---------------------------------------------------
  var_display_tab <- if (!is.null(tab_obj$var_label) &&
                        nzchar(tab_obj$var_label)) {
    tab_obj$var_label
  } else {
    tab_obj$var_name
  }

  all_display_vals <- tab_df$value
  natural_lbl_w <- max(
    nchar(all_display_vals),
    nchar("Total"),
    nchar(var_display_tab),
    1L,
    na.rm = TRUE
  )
  if (tab_obj$var_type %in% c("factor", "ordered", "character")) {
    natural_lbl_w <- max(natural_lbl_w + 2L, nchar(var_display_tab), nchar("Total"))
  } else if (tab_obj$var_type == "haven_labelled") {
    natural_lbl_w <- max(natural_lbl_w + 1L, nchar(var_display_tab), nchar("Total"))
  }

  natural_total_w <- natural_lbl_w + total_num_w
  total_width     <- min(natural_total_w, width)
  lbl_w           <- total_width - total_num_w
  if (lbl_w < 5L) lbl_w <- 5L

  # --- Header ---------------------------------------------------------------
  sep_line <- paste0(char_rep(.BOX_H, lbl_w + 1L), .BOX_CROSS,
                     char_rep(.BOX_H, num_w))

  var_hdr_line <- mean_display

  var_hdr <- .truncate_tab_label(var_display_tab, lbl_w)
  col_parts <- vapply(seq_len(n_stats), function(k)
    pad_left(stat_names[k], cw[k]), character(1L))
  col_data <- paste(col_parts, collapse = " ")
  hdr_line <- paste0(pad_left(var_hdr, lbl_w), " ", .BOX_V, col_data)

  lines <- c(var_hdr_line, sep_line, hdr_line, sep_line)

  # --- Data rows ------------------------------------------------------------
  for (i in seq_len(n_rows)) {
    row <- tab_df[i, ]

    display_lbl <- row$value
    if (tab_obj$var_type %in% c("factor", "ordered", "character")) {
      display_lbl <- paste0("  ", display_lbl)
    } else if (tab_obj$var_type == "haven_labelled") {
      display_lbl <- paste0(" ", display_lbl)
    }
    display_lbl <- .truncate_tab_label(display_lbl, lbl_w)

    if (tab_obj$var_type %in% c("numeric")) {
      lbl_fmt <- pad_left(display_lbl, lbl_w)
    } else {
      lbl_fmt <- pad_right(display_lbl, lbl_w)
    }

    data_parts <- vapply(seq_len(n_stats), function(k)
      pad_left(val_strs[i, k], cw[k]), character(1L))
    data_str <- paste(data_parts, collapse = " ")

    lines <- c(lines, paste0(lbl_fmt, " ", .BOX_V, data_str))
  }

  # --- Separator before total -----------------------------------------------
  lines <- c(lines, sep_line)

  # --- Total row ------------------------------------------------------------
  total_lbl <- pad_left("Total", lbl_w)
  total_parts <- vapply(seq_len(n_stats), function(k)
    pad_left(total_strs[k], cw[k]), character(1L))
  total_str <- paste(total_parts, collapse = " ")

  lines <- c(lines, paste0(total_lbl, " ", .BOX_V, total_str))

  lines
}

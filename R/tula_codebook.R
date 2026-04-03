# ---------------------------------------------------------------------------
# tula() codebook path — Stata-inspired -codebook- output for data frames
# and atomic vectors.
#
# Entry point: .tula_codebook(df, width)
# Output object: tula_codebook (S3), printed by print.tula_codebook()
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Internal entry point called by tula.data.frame() and tula.default()
# when codebook = TRUE.
# ---------------------------------------------------------------------------

.tula_codebook <- function(df, width = NULL) {
  stopifnot(is.data.frame(df))

  entries <- vector("list", ncol(df))
  for (j in seq_along(df)) {
    entries[[j]] <- .build_codebook_entry(
      col     = df[[j]],
      varname = names(df)[j]
    )
  }

  new_tula_codebook(entries = entries, width = width)
}


# ---------------------------------------------------------------------------
# S3 constructor
# ---------------------------------------------------------------------------

new_tula_codebook <- function(entries, width) {
  structure(
    list(
      entries = entries,
      width   = width
    ),
    class = "tula_codebook"
  )
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' Print method for tula_codebook objects
#'
#' Prints Stata-inspired codebook output: one block per variable showing
#' type, range, unique values, missing counts, and either a tabulation
#' or summary statistics with percentiles.
#'
#' @param x A `tula_codebook` object.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @export
print.tula_codebook <- function(x, ...) {
  width <- .resolve_width(x$width)

  for (i in seq_along(x$entries)) {
    entry <- x$entries[[i]]
    lines <- .format_codebook_entry(entry, width)
    cat(paste(lines, collapse = "\n"), "\n", sep = "")
  }

  invisible(x)
}


# ---------------------------------------------------------------------------
# Per-variable data builder
# ---------------------------------------------------------------------------

.build_codebook_entry <- function(col, varname) {
  # --- Haven variable label (displayed in header) --------------------------
  var_label <- attr(col, "label", exact = TRUE)
  if (!is.null(var_label) && (!is.character(var_label) || !nzchar(var_label))) {
    var_label <- NULL
  }

  # --- Type detection ------------------------------------------------------
  is_haven      <- inherits(col, "haven_labelled")
  haven_labels  <- attr(col, "labels", exact = TRUE)
  has_val_labels <- !is.null(haven_labels) && length(haven_labels) > 0L

  if (is_haven) {
    raw_vec   <- unclass(col)
    r_storage <- if (is.integer(raw_vec)) "int" else "float"
    type_desc <- paste0("Numeric (", r_storage, ")")
    base_type <- "numeric"
  } else if (is.ordered(col)) {
    type_desc <- "Ordered factor"
    base_type <- "ordered"
  } else if (is.factor(col)) {
    type_desc <- "Factor"
    base_type <- "factor"
  } else if (is.logical(col)) {
    type_desc <- "Boolean"
    base_type <- "logical"
  } else if (is.character(col)) {
    non_na_chr <- col[!is.na(col)]
    max_len    <- if (length(non_na_chr) > 0L) max(nchar(non_na_chr)) else 0L
    type_desc  <- paste0("String (str", max_len, ")")
    base_type  <- "character"
  } else if (is.integer(col)) {
    type_desc <- "Numeric (int)"
    base_type <- "integer"
  } else if (is.numeric(col)) {
    type_desc <- "Numeric (float)"
    base_type <- "numeric"
  } else {
    type_desc <- paste0("Unknown (", class(col)[1L], ")")
    base_type <- "other"
  }

  # --- Basic counts --------------------------------------------------------
  n_total   <- length(col)
  n_missing <- sum(is.na(col))
  non_na    <- col[!is.na(col)]

  if (base_type == "character") {
    n_unique <- length(unique(non_na))
  } else if (base_type %in% c("factor", "ordered")) {
    # Count only levels that actually appear (not unused levels)
    n_unique <- length(unique(as.character(non_na)))
  } else {
    # numeric, integer, logical, haven_labelled
    n_unique <- length(unique(as.numeric(non_na)))
  }

  # --- Display mode decision -----------------------------------------------
  if (base_type == "character") {
    display_mode <- "string"
  } else if (base_type %in% c("factor", "ordered")) {
    display_mode <- "tabulation"
  } else if (is_haven && has_val_labels) {
    display_mode <- "tabulation"
  } else if (base_type %in% c("numeric", "integer", "logical", "other") &&
             n_unique <= 9L) {
    display_mode <- "tabulation"
  } else {
    display_mode <- "continuous"
  }

  # --- Range and units (numeric types only) ---------------------------------
  range_min <- NULL
  range_max <- NULL
  units     <- NULL

  if (base_type %in% c("numeric", "integer") || is_haven) {
    num_vals <- as.numeric(non_na)
    if (length(num_vals) > 0L) {
      range_min <- min(num_vals)
      range_max <- max(num_vals)
      # Units: only for integer-like data
      all_integer_like <- all(num_vals == round(num_vals))
      if (all_integer_like) {
        units <- .compute_units(num_vals)
      }
    }
  }

  # --- Continuous fields ---------------------------------------------------
  mean_val <- NULL
  sd_val   <- NULL
  pctiles  <- NULL

  if (display_mode == "continuous") {
    num_vals <- as.numeric(non_na)
    if (length(num_vals) > 0L) {
      mean_val <- mean(num_vals)
      sd_val   <- if (length(num_vals) > 1L) stats::sd(num_vals) else NA_real_
      pctiles  <- stats::quantile(num_vals,
                                  probs = c(0.10, 0.25, 0.50, 0.75, 0.90),
                                  names = TRUE, na.rm = TRUE)
      names(pctiles) <- c("10%", "25%", "50%", "75%", "90%")
    }
  }

  # --- Tabulation fields ---------------------------------------------------
  tab_df <- NULL
  if (display_mode == "tabulation") {
    tab_df <- .build_codebook_tab(col, base_type, is_haven, haven_labels)
  }

  # --- String examples -----------------------------------------------------
  examples <- NULL
  if (display_mode == "string") {
    sorted <- sort(unique(non_na))
    n_ex   <- min(4L, length(sorted))
    if (n_ex > 0L) {
      # Evenly-spaced examples from the sorted unique values
      idx      <- round(seq(1, length(sorted), length.out = n_ex))
      examples <- sorted[idx]
    }
  }

  list(
    var_name       = varname,
    var_label      = var_label,
    var_type       = base_type,
    type_desc      = type_desc,
    is_haven       = is_haven,
    has_val_labels = has_val_labels,
    n_total        = as.integer(n_total),
    n_missing      = as.integer(n_missing),
    n_unique       = as.integer(n_unique),
    range_min      = range_min,
    range_max      = range_max,
    units          = units,
    display_mode   = display_mode,
    mean_val       = mean_val,
    sd_val         = sd_val,
    pctiles        = pctiles,
    tab_df         = tab_df,
    examples       = examples
  )
}


# ---------------------------------------------------------------------------
# Units computation: GCD of differences between sorted unique values
# ---------------------------------------------------------------------------

.compute_units <- function(x) {
  ux <- sort(unique(x))
  if (length(ux) <= 1L) return(1)

  diffs <- diff(ux)
  diffs <- round(diffs, digits = 10L)
  diffs <- diffs[diffs > 0]
  if (length(diffs) == 0L) return(1)

  # Euclidean GCD for positive reals, with tolerance
  gcd2 <- function(a, b) {
    if (abs(b) < 1e-10) return(a)
    gcd2(b, a %% b)
  }

  result <- diffs[1L]
  for (i in seq_along(diffs)[-1L]) {
    result <- gcd2(result, diffs[i])
  }

  # Clean up: if very close to a round number, round it
  if (abs(result - round(result)) < 1e-8) result <- round(result)
  result
}


# ---------------------------------------------------------------------------
# Tabulation builder for codebook display
# ---------------------------------------------------------------------------

.build_codebook_tab <- function(col, base_type, is_haven, haven_labels) {
  non_na <- col[!is.na(col)]

  if (base_type %in% c("factor", "ordered")) {
    lvls  <- levels(col)
    freqs <- tabulate(factor(non_na, levels = lvls), nbins = length(lvls))
    return(data.frame(
      value     = lvls,
      num_value = NA_real_,
      label     = NA_character_,
      freq      = as.integer(freqs),
      stringsAsFactors = FALSE
    ))
  }

  if (is_haven) {
    # Build code -> label lookup from haven labels attribute
    code_to_label <- if (!is.null(haven_labels) && length(haven_labels) > 0L) {
      stats::setNames(names(haven_labels),
                      as.character(as.numeric(haven_labels)))
    } else {
      character(0)
    }

    tbl   <- table(as.numeric(non_na))
    freqs <- as.integer(tbl)
    vals  <- names(tbl)

    labels_vec <- vapply(vals, function(v) {
      lbl <- code_to_label[v]
      if (is.na(lbl)) NA_character_ else lbl
    }, character(1L), USE.NAMES = FALSE)

    return(data.frame(
      value     = vals,
      num_value = as.numeric(vals),
      label     = labels_vec,
      freq      = freqs,
      stringsAsFactors = FALSE
    ))
  }

  # Plain numeric / integer / logical with few unique values
  if (is.logical(non_na)) {
    # Boolean: show TRUE/FALSE as values, ordered FALSE then TRUE
    tbl   <- table(non_na)
    vals  <- names(tbl)
    freqs <- as.integer(tbl)
    return(data.frame(
      value     = vals,
      num_value = NA_real_,
      label     = NA_character_,
      freq      = freqs,
      stringsAsFactors = FALSE
    ))
  }
  tbl   <- table(non_na)
  vals  <- names(tbl)
  freqs <- as.integer(tbl)

  data.frame(
    value     = vals,
    num_value = suppressWarnings(as.numeric(vals)),
    label     = NA_character_,
    freq      = freqs,
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------
# Per-variable renderer
# ---------------------------------------------------------------------------

.format_codebook_entry <- function(entry, width) {
  sep_line <- char_rep(.BOX_H, width)

  lines <- character(0)

  # --- Separator + variable name/label header ------------------------------
  lines <- c(lines, sep_line)

  if (!is.null(entry$var_label) && nzchar(entry$var_label)) {
    name_part  <- entry$var_name
    label_part <- entry$var_label
    gap <- width - nchar(name_part) - nchar(label_part)
    if (gap < 2L) {
      # Truncate the label to fit
      avail <- width - nchar(name_part) - 2L
      if (avail > 3L) {
        label_part <- paste0(substr(label_part, 1L, avail - 1L), "~")
      } else {
        label_part <- ""
      }
      gap <- width - nchar(name_part) - nchar(label_part)
    }
    header_line <- paste0(name_part, strrep(" ", max(gap, 2L)), label_part)
  } else {
    header_line <- entry$var_name
  }
  lines <- c(lines, header_line)
  lines <- c(lines, sep_line)

  # --- Blank line ----------------------------------------------------------
  lines <- c(lines, "")

  # --- Key-value field width -----------------------------------------------
  # 18 characters for the label area (right-aligned), matching Stata
  kv_w <- 18L

  # --- Type line -----------------------------------------------------------
  lines <- c(lines, .cb_kv("Type", entry$type_desc, kv_w))

  # --- Haven label set name (if applicable) --------------------------------
  if (isTRUE(entry$is_haven) && isTRUE(entry$has_val_labels)) {
    lines <- c(lines, .cb_kv("Label", entry$var_name, kv_w))
  }

  # --- Blank line before Range/Unique values -------------------------------
  lines <- c(lines, "")

  # --- Range and Units (numeric types, not factors/strings/booleans) -------
  is_numeric_type <- entry$var_type %in% c("numeric", "integer") ||
                     isTRUE(entry$is_haven)

  if (is_numeric_type && !is.null(entry$range_min) && !is.null(entry$range_max)) {
    range_str <- paste0("[", .cb_fmt_num(entry$range_min), ",",
                        .cb_fmt_num(entry$range_max), "]")
    if (!is.null(entry$units)) {
      units_str <- .cb_fmt_num(entry$units)
      range_line <- .cb_kv_pair("Range", range_str, "Units", units_str,
                                kv_w, width)
    } else {
      range_line <- .cb_kv("Range", range_str, kv_w)
    }
    lines <- c(lines, range_line)
  }

  # --- Unique values and Missing -------------------------------------------
  unique_str <- as.character(entry$n_unique)
  miss_char  <- if (entry$var_type == "character") '""' else "."
  miss_str   <- paste0(entry$n_missing, "/", entry$n_total)
  uniq_miss_line <- .cb_kv_pair("Unique values", unique_str,
                                paste0("Missing ", miss_char), miss_str,
                                kv_w, width)
  lines <- c(lines, uniq_miss_line)

  # --- Display-mode-specific content ---------------------------------------

  if (entry$display_mode == "continuous") {
    lines <- c(lines, "")
    lines <- c(lines, .cb_kv("Mean", .cb_fmt_num(entry$mean_val), kv_w))
    sd_str <- if (!is.null(entry$sd_val) && !is.na(entry$sd_val)) {
      .cb_fmt_num(entry$sd_val)
    } else {
      ""
    }
    lines <- c(lines, .cb_kv("Std. dev.", sd_str, kv_w))

    if (!is.null(entry$pctiles)) {
      lines <- c(lines, "")
      pct_cw    <- 10L
      pct_names <- names(entry$pctiles)
      pct_vals  <- entry$pctiles

      # Percentile header line: "Percentiles:     10%       25% ..."
      pct_hdrs <- paste0(vapply(pct_names,
                                function(nm) pad_left(nm, pct_cw),
                                character(1L)),
                         collapse = "")
      lines <- c(lines, .cb_kv_inline("Percentiles", pct_hdrs, kv_w))

      # Percentile value line
      pct_val_strs <- paste0(
        vapply(pct_vals,
               function(v) pad_left(.cb_fmt_num(v), pct_cw),
               character(1L)),
        collapse = "")
      lines <- c(lines, paste0(strrep(" ", kv_w + 1L), pct_val_strs))
    }

  } else if (entry$display_mode == "tabulation") {
    lines <- c(lines, "")

    # Determine tabulation format based on variable type
    if (entry$var_type %in% c("factor", "ordered")) {
      # Factor: Freq. + Level (no numeric codes)
      # Ordered factors with many levels get truncated (first N-2, ..., last)
      lines <- c(lines, .cb_format_factor_tab(entry$tab_df, kv_w,
                                               is_ordered = (entry$var_type == "ordered")))
    } else if (entry$var_type == "logical") {
      # Boolean: Freq. + Value (TRUE/FALSE labels, like factor)
      lines <- c(lines, .cb_format_factor_tab(entry$tab_df, kv_w))
    } else if (isTRUE(entry$is_haven) && isTRUE(entry$has_val_labels)) {
      # Haven with value labels: Freq. + Numeric + Label
      lines <- c(lines, .cb_format_haven_tab(entry$tab_df, kv_w))
    } else {
      # Plain numeric with few unique values: Freq. + Value
      lines <- c(lines, .cb_format_numeric_tab(entry$tab_df, kv_w))
    }

  } else if (entry$display_mode == "string") {
    lines <- c(lines, "")
    if (!is.null(entry$examples) && length(entry$examples) > 0L) {
      for (k in seq_along(entry$examples)) {
        prefix <- if (k == 1L) "Examples" else ""
        ex_str <- paste0('"', entry$examples[k], '"')
        lines <- c(lines, .cb_kv(prefix, ex_str, kv_w))
      }
    }
  }

  # Trailing blank line between variables
  lines <- c(lines, "")

  lines
}


# ---------------------------------------------------------------------------
# Tabulation formatters (one per display variant)
# ---------------------------------------------------------------------------

# Factor: Freq.  Level
# For ordered factors with > 9 levels, truncates to first 8 + "..." + last.
.cb_format_factor_tab <- function(tab_df, kv_w, is_ordered = FALSE) {
  if (is.null(tab_df) || nrow(tab_df) == 0L) return(character(0))

  max_show <- 9L
  nr       <- nrow(tab_df)
  truncate <- is_ordered && nr > max_show

  # Determine which rows to show
  if (truncate) {
    show_idx <- c(seq_len(max_show - 1L), nr)  # first 8 + last
  } else {
    show_idx <- seq_len(nr)
  }

  freq_strs  <- formatC(tab_df$freq, format = "d", big.mark = ",")
  freq_w     <- max(nchar(freq_strs[show_idx]), nchar("Freq."))

  # Header
  hdr <- paste0(pad_left(paste0("Tabulation:"), kv_w), " ",
                pad_left("Freq.", freq_w), "  Level")
  lines <- hdr

  # Data rows (first block, before ellipsis)
  n_before <- if (truncate) max_show - 1L else nr
  for (i in seq_len(n_before)) {
    row_str <- paste0(strrep(" ", kv_w + 1L),
                      pad_left(freq_strs[i], freq_w),
                      "  ", tab_df$value[i])
    lines <- c(lines, row_str)
  }

  # Ellipsis row + last row (only for truncated ordered factors)
  if (truncate) {
    dot_str <- paste0(strrep(" ", kv_w + 1L),
                      pad_left(".", freq_w),
                      "  ...")
    lines <- c(lines, dot_str)
    last_str <- paste0(strrep(" ", kv_w + 1L),
                       pad_left(freq_strs[nr], freq_w),
                       "  ", tab_df$value[nr])
    lines <- c(lines, last_str)
  }

  lines
}

# Haven-labelled: Freq.   Numeric  Label
.cb_format_haven_tab <- function(tab_df, kv_w) {
  if (is.null(tab_df) || nrow(tab_df) == 0L) return(character(0))

  freq_strs <- formatC(tab_df$freq, format = "d", big.mark = ",")
  freq_w    <- max(nchar(freq_strs), nchar("Freq."))

  num_strs  <- .cb_fmt_num_vec(tab_df$num_value)
  num_w     <- max(nchar(num_strs), nchar("Numeric"))

  # Header
  hdr <- paste0(pad_left("Tabulation:", kv_w), " ",
                pad_left("Freq.", freq_w), "  ",
                pad_left("Numeric", num_w), "  Label")
  lines <- hdr

  # Data rows
  for (i in seq_len(nrow(tab_df))) {
    lbl <- if (!is.na(tab_df$label[i])) tab_df$label[i] else ""
    row_str <- paste0(strrep(" ", kv_w + 1L),
                      pad_left(freq_strs[i], freq_w), "  ",
                      pad_left(num_strs[i], num_w), "  ",
                      lbl)
    lines <- c(lines, row_str)
  }

  lines
}

# Numeric with few unique values: Freq.  Value
.cb_format_numeric_tab <- function(tab_df, kv_w) {
  if (is.null(tab_df) || nrow(tab_df) == 0L) return(character(0))

  freq_strs <- formatC(tab_df$freq, format = "d", big.mark = ",")
  freq_w    <- max(nchar(freq_strs), nchar("Freq."))

  val_strs  <- .cb_fmt_num_vec(tab_df$num_value)
  val_w     <- max(nchar(val_strs), nchar("Value"))

  # Header
  hdr <- paste0(pad_left("Tabulation:", kv_w), " ",
                pad_left("Freq.", freq_w), "  ",
                pad_left("Value", val_w))
  lines <- hdr

  # Data rows
  for (i in seq_len(nrow(tab_df))) {
    row_str <- paste0(strrep(" ", kv_w + 1L),
                      pad_left(freq_strs[i], freq_w), "  ",
                      pad_left(val_strs[i], val_w))
    lines <- c(lines, row_str)
  }

  lines
}


# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

# Format a key-value line:  "             Key: value"
.cb_kv <- function(key, value, kv_w) {
  if (nzchar(key)) {
    paste0(pad_left(paste0(key, ":"), kv_w), " ", value)
  } else {
    paste0(strrep(" ", kv_w + 1L), value)
  }
}

# Format a two-column key-value line:
#   "             Range: [min,max]                       Units: 1"
.cb_kv_pair <- function(key_l, val_l, key_r, val_r, kv_w, total_width) {
  left_part  <- paste0(pad_left(paste0(key_l, ":"), kv_w), " ", val_l)
  right_part <- paste0(key_r, ": ", val_r)
  gap <- total_width - nchar(left_part) - nchar(right_part)
  if (gap < 2L) gap <- 2L
  paste0(left_part, strrep(" ", gap), right_part)
}

# Format a key with inline continuation:
#   "       Percentiles:     10%       25%  ..."
.cb_kv_inline <- function(key, rest, kv_w) {
  paste0(pad_left(paste0(key, ":"), kv_w), " ", rest)
}

# Format a numeric value for codebook display.
# Integers shown without decimals; floats with up to 6 significant digits.
.cb_fmt_num <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("")
  if (x == round(x) && abs(x) < 1e15) {
    # Integer-like: no decimals
    formatC(x, format = "f", digits = 0, big.mark = "")
  } else {
    trimws(formatC(x, digits = 6, format = "fg"))
  }
}

# Vectorized .cb_fmt_num for tabulation columns
.cb_fmt_num_vec <- function(x) {
  vapply(x, .cb_fmt_num, character(1L), USE.NAMES = FALSE)
}

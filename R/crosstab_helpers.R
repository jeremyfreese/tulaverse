# ---------------------------------------------------------------------------
# Crosstab helpers — internal functions for two-way tulatab() output
#
# All functions here are internal (not exported).  Reuses helpers from
# tab_helpers.R (.detect_var_type, .truncate_tab_label, .fmt_freq, .fmt_pct)
# and format_helpers.R (pad_left, pad_right, char_rep).
# ---------------------------------------------------------------------------


# Wrap text on word boundaries, returning at most `max_lines` lines.
# Each line fits within `width` characters.  Right-aligned output is
# the caller's job — this just splits.
.wrap_row_header <- function(text, width, max_lines = 3L) {
  if (nchar(text) <= width) return(text)
  words <- strsplit(text, "\\s+")[[1L]]
  lines <- character(0)
  current <- ""
  for (i in seq_along(words)) {
    w <- words[i]
    candidate <- if (nchar(current) == 0L) w else paste(current, w)
    if (nchar(candidate) <= width) {
      current <- candidate
    } else {
      if (nchar(current) > 0L) lines <- c(lines, current)
      current <- w
      if (length(lines) >= max_lines - 1L) {
        # Last line: join remaining words and truncate
        remaining <- paste(words[i:length(words)], collapse = " ")
        lines <- c(lines, .truncate_tab_label(remaining, width))
        return(lines)
      }
    }
  }
  if (nchar(current) > 0L) lines <- c(lines, current)
  lines[seq_len(min(length(lines), max_lines))]
}


# Hard-split a column label into at most 2 lines of `width` characters.
# Second line truncated with ~ if needed.  Right-alignment is caller's job.
.wrap_col_label <- function(text, width) {
  if (nchar(text) <= width) return(text)
  line1 <- substr(text, 1L, width)
  line2 <- substr(text, width + 1L, nchar(text))
  if (nchar(line2) > width) line2 <- .truncate_tab_label(line2, width)
  c(line1, line2)
}


# -----------------------------------------------------------------------
# Display-level extraction for crosstab variables
#
# Returns a list:
#   display_order  chr vector — ordered display strings (natural or haven order)
#   key_vec        chr vector — same length as vec, each obs mapped to its
#                               display string; NA for missing when missing=FALSE
#   is_missing     lgl vector — aligned with display_order
# -----------------------------------------------------------------------
.crosstab_display_levels <- function(vec, var_type, show_value, show_label,
                                     missing) {
  if (var_type == "haven_labelled") {
    .ct_levels_haven(vec, show_value, show_label, missing)
  } else if (var_type %in% c("factor", "ordered")) {
    .ct_levels_factor(vec, missing)
  } else if (var_type == "character") {
    .ct_levels_character(vec, missing)
  } else {
    # numeric
    .ct_levels_numeric(vec, missing)
  }
}


# --- Factor / ordered ---------------------------------------------------
.ct_levels_factor <- function(vec, missing) {
  lvls <- levels(vec)
  key_vec <- as.character(vec)  # NA stays NA
  is_na <- is.na(key_vec)

  display_order <- lvls
  is_miss <- rep(FALSE, length(lvls))

  if (missing && any(is_na)) {
    display_order <- c(display_order, ".")
    is_miss <- c(is_miss, TRUE)
    key_vec[is_na] <- "."
  }

  list(display_order = display_order, key_vec = key_vec, is_missing = is_miss)
}


# --- Character -----------------------------------------------------------
.ct_levels_character <- function(vec, missing) {
  is_na <- is.na(vec)
  non_na_vals <- sort(unique(vec[!is_na]))

  key_vec <- vec
  display_order <- non_na_vals
  is_miss <- rep(FALSE, length(non_na_vals))

  if (missing && any(is_na)) {
    display_order <- c(display_order, ".")
    is_miss <- c(is_miss, TRUE)
    key_vec[is_na] <- "."
  }

  list(display_order = display_order, key_vec = key_vec, is_missing = is_miss)
}


# --- Numeric -------------------------------------------------------------
.ct_levels_numeric <- function(vec, missing) {
  is_na <- is.na(vec)
  non_na <- vec[!is_na]
  uvals <- sort(unique(non_na))

  # Display: integer-looking values without decimal
  display <- vapply(uvals, function(v) {
    if (v == as.integer(v)) as.character(as.integer(v)) else as.character(v)
  }, character(1))

  key_vec <- character(length(vec))
  for (i in seq_along(uvals)) {
    key_vec[!is_na & vec == uvals[i]] <- display[i]
  }
  key_vec[is_na] <- NA_character_

  display_order <- display
  is_miss <- rep(FALSE, length(display))

  if (missing && any(is_na)) {
    display_order <- c(display_order, ".")
    is_miss <- c(is_miss, TRUE)
    key_vec[is_na] <- "."
  }

  list(display_order = display_order, key_vec = key_vec, is_missing = is_miss)
}


# --- Haven labelled ------------------------------------------------------
.ct_levels_haven <- function(vec, show_value, show_label, missing) {
  haven_labels <- attr(vec, "labels", exact = TRUE)
  has_haven <- requireNamespace("haven", quietly = TRUE)

  # code -> label lookup
  if (!is.null(haven_labels) && length(haven_labels) > 0L) {
    code_to_label <- stats::setNames(names(haven_labels), as.character(haven_labels))
  } else {
    code_to_label <- character(0)
  }

  # Tagged NA detection
  if (has_haven) {
    is_tagged <- haven::is_tagged_na(vec)
    na_tags   <- ifelse(is_tagged, haven::na_tag(vec), NA_character_)
  } else {
    is_tagged <- rep(FALSE, length(vec))
    na_tags   <- rep(NA_character_, length(vec))
  }
  is_regular_na <- is.na(vec) & !is_tagged

  # --- Non-missing values ---
  non_na_vals <- vec[!is.na(vec) | is_tagged]
  non_na_vals <- non_na_vals[!is_tagged[!is.na(vec) | is_tagged]]
  uvals <- sort(unique(as.numeric(non_na_vals)))

  # Build code strings and display strings for non-missing
  code_strs <- vapply(uvals, function(v) {
    if (v == as.integer(v)) as.character(as.integer(v)) else as.character(v)
  }, character(1))

  # Right-align codes (will finalize after including tagged NAs)
  raw_codes <- code_strs

  display_strs <- vapply(seq_along(uvals), function(i) {
    lbl <- code_to_label[code_strs[i]]
    if (is.na(lbl)) lbl <- NA_character_
    .haven_display_value(code_strs[i], lbl, show_value, show_label)
  }, character(1))

  is_miss <- rep(FALSE, length(uvals))

  # --- Tagged NAs ---
  tagged_codes <- character(0)
  tagged_displays <- character(0)
  if (has_haven && any(is_tagged)) {
    unique_tags <- sort(unique(na_tags[is_tagged]))
    for (tag in unique_tags) {
      tag_code <- paste0(".", tag)
      # Look up label
      tag_label <- NA_character_
      if (!is.null(haven_labels)) {
        for (j in seq_along(haven_labels)) {
          if (haven::is_tagged_na(haven_labels[j]) &&
              haven::na_tag(haven_labels[j]) == tag) {
            tag_label <- names(haven_labels)[j]
            break
          }
        }
      }
      tagged_codes <- c(tagged_codes, tag_code)
      disp <- if (!is.na(tag_label)) {
        .haven_display_value(tag_code, tag_label, show_value, show_label)
      } else {
        tag_code
      }
      tagged_displays <- c(tagged_displays, disp)
    }
  }

  # --- Regular NA ---
  reg_na_display <- character(0)
  has_reg_na <- any(is_regular_na)

  # --- Now right-align all code fields if show_value ---
  all_raw_codes <- c(raw_codes, tagged_codes)
  if (has_reg_na && missing) all_raw_codes <- c(all_raw_codes, ".")

  if (show_value && length(all_raw_codes) > 0L) {
    max_code_w <- max(nchar(all_raw_codes), na.rm = TRUE)
    # Rebuild display strings with aligned codes
    display_strs <- vapply(seq_along(uvals), function(i) {
      lbl <- code_to_label[code_strs[i]]
      if (is.na(lbl)) lbl <- NA_character_
      .haven_display_value(pad_left(code_strs[i], max_code_w), lbl,
                           show_value, show_label)
    }, character(1))

    if (length(tagged_codes) > 0L) {
      tagged_displays <- vapply(seq_along(tagged_codes), function(i) {
        tc <- tagged_codes[i]
        # Look up label again
        tag_label <- NA_character_
        tag <- sub("^\\.", "", tc)
        if (has_haven && !is.null(haven_labels)) {
          for (j in seq_along(haven_labels)) {
            if (haven::is_tagged_na(haven_labels[j]) &&
                haven::na_tag(haven_labels[j]) == tag) {
              tag_label <- names(haven_labels)[j]
              break
            }
          }
        }
        if (!is.na(tag_label)) {
          .haven_display_value(pad_left(tc, max_code_w), tag_label,
                               show_value, show_label)
        } else {
          pad_left(tc, max_code_w)
        }
      }, character(1))
    }

    if (has_reg_na && missing) {
      reg_na_display <- pad_left(".", max_code_w)
    }
  } else {
    if (has_reg_na && missing) reg_na_display <- "."
  }

  # --- Assemble display order ---
  display_order <- display_strs
  is_miss_vec <- rep(FALSE, length(display_strs))

  if (missing) {
    if (length(tagged_displays) > 0L) {
      display_order <- c(display_order, tagged_displays)
      is_miss_vec <- c(is_miss_vec, rep(TRUE, length(tagged_displays)))
    }
    if (has_reg_na) {
      display_order <- c(display_order, reg_na_display)
      is_miss_vec <- c(is_miss_vec, TRUE)
    }
  }

  # --- Build key_vec mapping observations to display strings ---
  key_vec <- character(length(vec))
  for (i in seq_along(uvals)) {
    mask <- !is.na(vec) & !is_tagged & as.numeric(vec) == uvals[i]
    key_vec[mask] <- display_strs[i]
  }

  if (missing) {
    if (length(tagged_codes) > 0L) {
      unique_tags <- sort(unique(na_tags[is_tagged]))
      for (i in seq_along(unique_tags)) {
        mask <- is_tagged & na_tags == unique_tags[i]
        key_vec[mask] <- tagged_displays[i]
      }
    }
    if (has_reg_na) {
      key_vec[is_regular_na] <- reg_na_display
    }
  } else {
    key_vec[is.na(vec)] <- NA_character_
  }

  list(display_order = display_order, key_vec = key_vec,
       is_missing = is_miss_vec)
}


# -----------------------------------------------------------------------
# Build the crosstab count matrix
# -----------------------------------------------------------------------
.build_crosstab <- function(y_vec, x_vec, y_type, x_type,
                            missing, sort, show_value, show_label) {

  y_info <- .crosstab_display_levels(y_vec, y_type, show_value, show_label,
                                     missing)
  x_info <- .crosstab_display_levels(x_vec, x_type, show_value, show_label,
                                     missing)

  y_keys <- y_info$key_vec
  x_keys <- x_info$key_vec

  # Drop observations with NA in either variable when missing=FALSE
  if (!missing) {
    keep <- !is.na(y_keys) & !is.na(x_keys)
    y_keys <- y_keys[keep]
    x_keys <- x_keys[keep]
  }

  y_fac <- factor(y_keys, levels = y_info$display_order)
  x_fac <- factor(x_keys, levels = x_info$display_order)

  ct <- as.matrix(table(y_fac, x_fac))

  # Sort by descending marginals if requested
  sort_row_order <- NULL
  sort_col_order <- NULL
  if (sort) {
    row_sums <- rowSums(ct)
    col_sums <- colSums(ct)

    y_miss <- y_info$is_missing
    x_miss <- x_info$is_missing

    # Non-missing rows sorted by descending total; missing rows at end
    nm_idx <- which(!y_miss)
    m_idx  <- which(y_miss)
    sort_row_order <- c(nm_idx[order(-row_sums[nm_idx])], m_idx)

    nm_idx <- which(!x_miss)
    m_idx  <- which(x_miss)
    sort_col_order <- c(nm_idx[order(-col_sums[nm_idx])], m_idx)

    ct <- ct[sort_row_order, sort_col_order, drop = FALSE]
  }

  list(
    ct_matrix      = ct,
    row_levels     = rownames(ct),
    col_levels     = colnames(ct),
    y_keys         = y_keys,
    x_keys         = x_keys,
    keep_mask      = if (!missing) keep else rep(TRUE, length(y_keys)),
    sort_row_order = sort_row_order,
    sort_col_order = sort_col_order
  )
}


# -----------------------------------------------------------------------
# Compute percentage matrices
# -----------------------------------------------------------------------
.compute_crosstab_pct <- function(ct_matrix, pct, row_totals, col_totals,
                                  grand_total) {
  if (is.null(pct)) return(NULL)
  nr <- nrow(ct_matrix)
  nc <- ncol(ct_matrix)
  pct_mat <- matrix(0, nrow = nr, ncol = nc)

  if (pct == "col") {
    for (j in seq_len(nc)) {
      denom <- col_totals[j]
      pct_mat[, j] <- if (denom > 0) ct_matrix[, j] / denom * 100 else 0
    }
  } else if (pct == "row") {
    for (i in seq_len(nr)) {
      denom <- row_totals[i]
      pct_mat[i, ] <- if (denom > 0) ct_matrix[i, ] / denom * 100 else 0
    }
  } else if (pct == "cell") {
    denom <- if (grand_total > 0) grand_total else 1
    pct_mat <- ct_matrix / denom * 100
  }

  pct_mat
}


# -----------------------------------------------------------------------
# Build mean and N matrices for mean= mode
#
# y_keys, x_keys: character vectors (already filtered for missing if needed)
# mean_vec:       numeric vector (same length as y_keys / x_keys)
# row_levels, col_levels: display-order level strings (from ct_matrix)
# sort_row_order, sort_col_order: integer reordering vectors or NULL
#
# Returns list(mean_mat, n_mat, mean_row_marginals, n_row_marginals,
#              mean_col_marginals, n_col_marginals, mean_grand, n_grand)
# -----------------------------------------------------------------------
.build_mean_crosstab <- function(y_keys, x_keys, mean_vec,
                                  row_levels, col_levels,
                                  sort_row_order = NULL,
                                  sort_col_order = NULL) {

  y_fac <- factor(y_keys, levels = row_levels)
  x_fac <- factor(x_keys, levels = col_levels)

  # Cell means and Ns
  mean_mat <- tapply(mean_vec, list(y_fac, x_fac), mean, na.rm = TRUE)
  n_mat    <- tapply(!is.na(mean_vec), list(y_fac, x_fac), sum)

  # tapply returns NaN for cells with 0 non-missing obs; replace with NA
  mean_mat[is.nan(mean_mat)] <- NA_real_
  # tapply returns NULL/NA for empty factor-level combinations; coerce to matrix
  mean_mat <- as.matrix(mean_mat)
  n_mat    <- as.matrix(n_mat)
  # Empty cells should show N=0, not NA
  n_mat[is.na(n_mat)] <- 0L

  # Apply sort order if requested (must match ct_matrix sort)
  if (!is.null(sort_row_order)) {
    mean_mat <- mean_mat[sort_row_order, , drop = FALSE]
    n_mat    <- n_mat[sort_row_order, , drop = FALSE]
  }
  if (!is.null(sort_col_order)) {
    mean_mat <- mean_mat[, sort_col_order, drop = FALSE]
    n_mat    <- n_mat[, sort_col_order, drop = FALSE]
  }

  # Marginal means (observation-weighted, NOT averages of cell means)
  # After sort, row/col levels of mean_mat match the sorted ct_matrix
  sorted_row_levels <- rownames(mean_mat)
  sorted_col_levels <- colnames(mean_mat)

  mean_row_marginals <- vapply(sorted_row_levels, function(lv) {
    mask <- y_keys == lv & !is.na(y_keys)
    vals <- mean_vec[mask]
    if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
  }, numeric(1L))

  n_row_marginals <- vapply(sorted_row_levels, function(lv) {
    mask <- y_keys == lv & !is.na(y_keys)
    sum(!is.na(mean_vec[mask]))
  }, integer(1L))

  mean_col_marginals <- vapply(sorted_col_levels, function(lv) {
    mask <- x_keys == lv & !is.na(x_keys)
    vals <- mean_vec[mask]
    if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
  }, numeric(1L))

  n_col_marginals <- vapply(sorted_col_levels, function(lv) {
    mask <- x_keys == lv & !is.na(x_keys)
    sum(!is.na(mean_vec[mask]))
  }, integer(1L))

  mean_grand <- if (all(is.na(mean_vec))) NA_real_ else mean(mean_vec, na.rm = TRUE)
  n_grand    <- sum(!is.na(mean_vec))

  list(
    mean_mat           = mean_mat,
    n_mat              = n_mat,
    mean_row_marginals = mean_row_marginals,
    n_row_marginals    = n_row_marginals,
    mean_col_marginals = mean_col_marginals,
    n_col_marginals    = n_col_marginals,
    mean_grand         = mean_grand,
    n_grand            = n_grand
  )
}


# -----------------------------------------------------------------------
# Master crosstab renderer — returns character vector of output lines
# -----------------------------------------------------------------------
.format_crosstab_table <- function(ct_obj) {
  width <- .resolve_width(ct_obj$width)

  ct_mat     <- ct_obj$ct_matrix
  row_levels <- ct_obj$row_levels
  col_levels <- ct_obj$col_levels

  # Mean mode: when mean_mat is non-NULL, display means + Ns instead of freq + pct
  mean_mode  <- !is.null(ct_obj$mean_mat)

  # Resolve dec: NULL -> 2 for pct, 3 for mean
  pct_dec  <- if (!is.null(ct_obj$dec)) ct_obj$dec else 2L
  mean_dec <- ct_obj$dec   # NULL when user didn't set dec= (smart formatting)

  if (mean_mode) {
    pct_mode  <- NULL
    show_freq <- FALSE
  } else {
    pct_mode  <- ct_obj$pct
    show_freq <- ct_obj$freq
  }

  n_rows <- nrow(ct_mat)
  n_cols <- ncol(ct_mat)

  # Totals
  row_totals  <- rowSums(ct_mat)
  col_totals  <- colSums(ct_mat)
  grand_total <- sum(ct_mat)

  # Percentage matrix (NULL if pct_mode is NULL or mean mode)
  pct_mat <- .compute_crosstab_pct(ct_mat, pct_mode, row_totals, col_totals,
                                   grand_total)

  # Total-column percentages and total-row percentages
  total_col_pct <- NULL
  total_row_pct <- NULL
  if (!is.null(pct_mode)) {
    if (pct_mode == "col") {
      # Total column: row_total / grand_total
      total_col_pct <- if (grand_total > 0) row_totals / grand_total * 100 else rep(0, n_rows)
      # Total row: 100.00 per column
      total_row_pct <- rep(100, n_cols)
      total_row_total_pct <- 100
    } else if (pct_mode == "row") {
      total_col_pct <- rep(100, n_rows)
      total_row_pct <- if (grand_total > 0) col_totals / grand_total * 100 else rep(0, n_cols)
      total_row_total_pct <- 100
    } else if (pct_mode == "cell") {
      total_col_pct <- if (grand_total > 0) row_totals / grand_total * 100 else rep(0, n_rows)
      total_row_pct <- if (grand_total > 0) col_totals / grand_total * 100 else rep(0, n_cols)
      total_row_total_pct <- 100
    }
  }

  # --- Column width calculations -----------------------------------------

  if (mean_mode) {
    # Width driven by formatted mean values and Ns
    all_means <- c(as.numeric(ct_obj$mean_mat),
                   ct_obj$mean_row_marginals,
                   ct_obj$mean_col_marginals,
                   ct_obj$mean_grand)
    all_means <- all_means[!is.na(all_means)]
    mean_w <- if (length(all_means) > 0L) {
      max(vapply(all_means, function(v) nchar(trimws(.fmt_sum(v, digits = 7L, width = 1L, dec = mean_dec))), integer(1L)))
    } else {
      1L
    }

    all_ns <- c(as.integer(ct_obj$n_mat),
                ct_obj$n_row_marginals,
                ct_obj$n_col_marginals,
                ct_obj$n_grand)
    max_n <- max(abs(all_ns), 1L)
    n_w <- nchar(formatC(max_n, format = "d", big.mark = ","))

    cell_w <- max(mean_w, n_w)
  } else {
    # Width driven by frequencies and percentages
    all_counts <- c(as.integer(ct_mat), as.integer(row_totals),
                    as.integer(col_totals), as.integer(grand_total))
    max_count <- max(abs(all_counts), 1L)
    freq_w <- nchar(formatC(max_count, format = "d", big.mark = ","))
    pct_w  <- if (pct_dec > 0L) 4L + pct_dec else 3L   # e.g. "100.00" = 6

    cell_w <- max(freq_w, pct_w)
  }

  # Minimum column width: content + 3 chars padding, at least 10
  cw_data <- max(cell_w + 3L, 10L)

  # Row label area width
  row_display <- if (!is.null(ct_obj$row_label) && nzchar(ct_obj$row_label)) {
    ct_obj$row_label
  } else {
    ct_obj$row_name
  }
  row_hdr_lines <- .wrap_row_header(row_display, 20L, max_lines = 3L)
  row_lbl_w <- max(nchar(row_levels), nchar("Total"),
                   max(nchar(row_hdr_lines)), 5L)

  # How many data columns fit in one panel?
  # Layout: row_lbl_w " |" data_zone " |" total_zone
  # data_zone = n * cw_data + (n-1) spaces between columns
  # total_zone = cw_data (same width, uniform)
  # pipe separators: " |" = 2 chars, twice = 4 chars total

  avail_for_data <- width - row_lbl_w - 2L - 2L - cw_data
  # Each column costs cw_data + 2 spaces (except first which costs cw_data)
  # n columns = n * cw_data + (n-1) * 2
  # avail >= n * cw_data + (n-1)*2  =>  n <= (avail + 2) / (cw_data + 2)
  n_cols_per_panel <- max(1L, floor((avail_for_data + 2L) / (cw_data + 2L)))

  # Re-wrap row header now that we know actual row_lbl_w
  row_hdr_lines <- .wrap_row_header(row_display, row_lbl_w, max_lines = 3L)

  # Column display name
  col_display <- if (!is.null(ct_obj$col_label) && nzchar(ct_obj$col_label)) {
    ct_obj$col_label
  } else {
    ct_obj$col_name
  }

  # --- Render panels -----------------------------------------------------
  col_indices <- seq_len(n_cols)
  n_panels <- ceiling(n_cols / n_cols_per_panel)

  lines <- character(0)
  for (p in seq_len(n_panels)) {
    start_col <- (p - 1L) * n_cols_per_panel + 1L
    end_col   <- min(p * n_cols_per_panel, n_cols)
    panel_cols <- col_indices[start_col:end_col]

    panel_lines <- .render_crosstab_panel(
      ct_mat       = ct_mat,
      pct_mat      = pct_mat,
      row_levels   = row_levels,
      col_levels   = col_levels,
      panel_cols   = panel_cols,
      row_totals   = row_totals,
      col_totals   = col_totals,
      grand_total  = grand_total,
      total_col_pct = total_col_pct,
      total_row_pct = total_row_pct,
      total_row_total_pct = if (!is.null(pct_mode)) total_row_total_pct else NULL,
      row_lbl_w    = row_lbl_w,
      cw_data      = cw_data,
      row_hdr_lines = row_hdr_lines,
      col_display  = col_display,
      pct_mode     = pct_mode,
      show_freq    = show_freq,
      pct_dec      = pct_dec,
      mean_mode    = mean_mode,
      mean_mat     = ct_obj$mean_mat,
      n_mat        = ct_obj$n_mat,
      mean_row_marginals = ct_obj$mean_row_marginals,
      n_row_marginals    = ct_obj$n_row_marginals,
      mean_col_marginals = ct_obj$mean_col_marginals,
      n_col_marginals    = ct_obj$n_col_marginals,
      mean_grand         = ct_obj$mean_grand,
      n_grand            = ct_obj$n_grand,
      mean_dec           = mean_dec
    )

    if (p > 1L) lines <- c(lines, "")
    lines <- c(lines, panel_lines)
  }

  lines
}


# -----------------------------------------------------------------------
# Render a single panel of the crosstab
# -----------------------------------------------------------------------
.render_crosstab_panel <- function(ct_mat, pct_mat, row_levels, col_levels,
                                   panel_cols, row_totals, col_totals,
                                   grand_total, total_col_pct, total_row_pct,
                                   total_row_total_pct,
                                   row_lbl_w, cw_data, row_hdr_lines,
                                   col_display, pct_mode, show_freq,
                                   pct_dec = 2L,
                                   mean_mode = FALSE,
                                   mean_mat = NULL, n_mat = NULL,
                                   mean_row_marginals = NULL,
                                   n_row_marginals = NULL,
                                   mean_col_marginals = NULL,
                                   n_col_marginals = NULL,
                                   mean_grand = NULL,
                                   n_grand = NULL,
                                   mean_dec = NULL) {

  n_panel <- length(panel_cols)
  n_rows  <- nrow(ct_mat)

  # Data zone width: n columns × cw_data + (n-1) × 2-space gaps
  data_zone_w <- n_panel * cw_data + max(0L, (n_panel - 1L) * 2L)
  total_zone_w <- cw_data

  # --- Column variable name header line ---
  col_name_trunc <- .truncate_tab_label(col_display, data_zone_w)
  col_name_centered <- pad_left(col_name_trunc,
                                floor((data_zone_w + nchar(col_name_trunc)) / 2))
  col_name_padded <- pad_right(col_name_centered, data_zone_w)

  col_name_line <- paste0(
    strrep(" ", row_lbl_w), " ", .BOX_V,
    col_name_padded, " ", .BOX_V
  )

  # --- Column labels (up to 2 lines each) ---
  wrapped_cols <- lapply(col_levels[panel_cols], .wrap_col_label, width = cw_data)
  max_col_lines <- max(vapply(wrapped_cols, length, integer(1)), 1L)

  # Pad each wrapped col to max_col_lines
  for (i in seq_along(wrapped_cols)) {
    while (length(wrapped_cols[[i]]) < max_col_lines) {
      wrapped_cols[[i]] <- c("", wrapped_cols[[i]])  # blank on top
    }
  }

  # Row header lines: pad to match max_col_lines + 1 (for col var name line)
  # The row header overlaps with column label lines (not the col var name line)
  n_hdr_lines <- max_col_lines
  while (length(row_hdr_lines) < n_hdr_lines) {
    row_hdr_lines <- c("", row_hdr_lines)  # blank on top, text at bottom
  }
  if (length(row_hdr_lines) > n_hdr_lines) {
    row_hdr_lines <- row_hdr_lines[seq_len(n_hdr_lines)]
  }

  # Build column label lines
  col_label_lines <- character(n_hdr_lines)
  for (ln in seq_len(n_hdr_lines)) {
    # Data zone: column labels for this line
    col_parts <- vapply(seq_along(wrapped_cols), function(i) {
      pad_left(wrapped_cols[[i]][ln], cw_data)
    }, character(1))
    data_str <- paste(col_parts, collapse = "  ")

    # Total header (only on last line)
    total_hdr <- if (ln == n_hdr_lines) pad_left("Total", cw_data) else strrep(" ", cw_data)

    # Row header
    rh <- pad_left(row_hdr_lines[ln], row_lbl_w)

    col_label_lines[ln] <- paste0(rh, " ", .BOX_V, data_str, " ", .BOX_V, total_hdr)
  }

  # --- Separator line ---
  sep_line <- paste0(
    char_rep(.BOX_H, row_lbl_w), .BOX_H, .BOX_CROSS,
    char_rep(.BOX_H, data_zone_w), .BOX_H, .BOX_CROSS,
    char_rep(.BOX_H, total_zone_w)
  )

  # --- Data rows ---
  data_lines <- character(0)
  for (i in seq_len(n_rows)) {
    row_lbl <- pad_left(row_levels[i], row_lbl_w)
    blank_lbl <- strrep(" ", row_lbl_w)

    if (mean_mode) {
      # --- Mean mode: mean line + N line ---
      mean_parts <- vapply(panel_cols, function(j) {
        val <- mean_mat[i, j]
        if (is.na(val)) pad_left("", cw_data) else .fmt_sum(val, digits = 7L, width = cw_data, dec = mean_dec)
      }, character(1))
      mean_data <- paste(mean_parts, collapse = "  ")
      # Row marginal mean in total column
      row_m <- mean_row_marginals[i]
      mean_total <- if (is.na(row_m)) pad_left("", cw_data) else .fmt_sum(row_m, digits = 7L, width = cw_data, dec = mean_dec)
      data_lines <- c(data_lines, paste0(row_lbl, " ", .BOX_V, mean_data, " ", .BOX_V, mean_total))

      # N line
      n_parts <- vapply(panel_cols, function(j) {
        pad_left(formatC(n_mat[i, j], format = "d", big.mark = ","), cw_data)
      }, character(1))
      n_data <- paste(n_parts, collapse = "  ")
      n_total <- pad_left(formatC(n_row_marginals[i], format = "d", big.mark = ","), cw_data)
      data_lines <- c(data_lines, paste0(blank_lbl, " ", .BOX_V, n_data, " ", .BOX_V, n_total))

    } else {
      # --- Freq/pct mode (existing logic) ---

      # Frequencies line
      if (show_freq) {
        freq_parts <- vapply(panel_cols, function(j) {
          pad_left(formatC(ct_mat[i, j], format = "d", big.mark = ","), cw_data)
        }, character(1))
        freq_data <- paste(freq_parts, collapse = "  ")
        freq_total <- pad_left(formatC(row_totals[i], format = "d", big.mark = ","), cw_data)
        data_lines <- c(data_lines, paste0(row_lbl, " ", .BOX_V, freq_data, " ", .BOX_V, freq_total))
      }

      # Percentage line
      if (!is.null(pct_mode)) {
        pct_parts <- vapply(panel_cols, function(j) {
          pad_left(sprintf("%.*f", pct_dec, pct_mat[i, j]), cw_data)
        }, character(1))
        pct_data <- paste(pct_parts, collapse = "  ")
        pct_total_val <- if (!is.null(total_col_pct)) sprintf("%.*f", pct_dec, total_col_pct[i]) else ""
        pct_total <- pad_left(pct_total_val, cw_data)

        lbl <- if (show_freq) blank_lbl else row_lbl
        data_lines <- c(data_lines, paste0(lbl, " ", .BOX_V, pct_data, " ", .BOX_V, pct_total))
      }

      # When neither freq nor pct: one blank line with label
      if (!show_freq && is.null(pct_mode)) {
        blank_data <- strrep(" ", data_zone_w)
        blank_total <- strrep(" ", cw_data)
        data_lines <- c(data_lines, paste0(row_lbl, " ", .BOX_V, blank_data, " ", .BOX_V, blank_total))
      }
    }

    # Blank separator line between categories (not after last row)
    if (i < n_rows) {
      blank_data <- strrep(" ", data_zone_w)
      blank_total <- strrep(" ", cw_data)
      data_lines <- c(data_lines, paste0(blank_lbl, " ", .BOX_V, blank_data, " ", .BOX_V, blank_total))
    }
  }

  # --- Total row ---
  total_lbl <- pad_left("Total", row_lbl_w)
  blank_lbl <- strrep(" ", row_lbl_w)

  total_data_lines <- character(0)

  if (mean_mode) {
    # Mean total row: column marginal means + grand mean
    mean_parts <- vapply(panel_cols, function(j) {
      val <- mean_col_marginals[j]
      if (is.na(val)) pad_left("", cw_data) else .fmt_sum(val, digits = 7L, width = cw_data, dec = mean_dec)
    }, character(1))
    mean_data <- paste(mean_parts, collapse = "  ")
    grand_m <- if (is.na(mean_grand)) pad_left("", cw_data) else .fmt_sum(mean_grand, digits = 7L, width = cw_data, dec = mean_dec)
    total_data_lines <- c(total_data_lines,
                          paste0(total_lbl, " ", .BOX_V, mean_data, " ", .BOX_V, grand_m))

    # N total row: column marginal Ns + grand N
    n_parts <- vapply(panel_cols, function(j) {
      pad_left(formatC(n_col_marginals[j], format = "d", big.mark = ","), cw_data)
    }, character(1))
    n_data <- paste(n_parts, collapse = "  ")
    n_total <- pad_left(formatC(n_grand, format = "d", big.mark = ","), cw_data)
    total_data_lines <- c(total_data_lines,
                          paste0(blank_lbl, " ", .BOX_V, n_data, " ", .BOX_V, n_total))

  } else {
    # --- Freq/pct total row (existing logic) ---

    if (show_freq) {
      freq_parts <- vapply(panel_cols, function(j) {
        pad_left(formatC(col_totals[j], format = "d", big.mark = ","), cw_data)
      }, character(1))
      freq_data <- paste(freq_parts, collapse = "  ")
      freq_total <- pad_left(formatC(grand_total, format = "d", big.mark = ","), cw_data)
      total_data_lines <- c(total_data_lines,
                            paste0(total_lbl, " ", .BOX_V, freq_data, " ", .BOX_V, freq_total))
    }

    if (!is.null(pct_mode)) {
      pct_parts <- vapply(panel_cols, function(j) {
        pad_left(sprintf("%.*f", pct_dec, total_row_pct[j]), cw_data)
      }, character(1))
      pct_data <- paste(pct_parts, collapse = "  ")
      pct_total <- pad_left(sprintf("%.*f", pct_dec, total_row_total_pct), cw_data)

      lbl <- if (show_freq) blank_lbl else total_lbl
      total_data_lines <- c(total_data_lines,
                            paste0(lbl, " ", .BOX_V, pct_data, " ", .BOX_V, pct_total))
    }

    if (!show_freq && is.null(pct_mode)) {
      blank_data <- strrep(" ", data_zone_w)
      blank_total <- strrep(" ", cw_data)
      total_data_lines <- c(total_data_lines,
                            paste0(total_lbl, " ", .BOX_V, blank_data, " ", .BOX_V, blank_total))
    }
  }

  # --- Assemble all lines ---
  c(col_name_line,
    col_label_lines,
    sep_line,
    data_lines,
    sep_line,   # separator before total
    total_data_lines)
}


# -----------------------------------------------------------------------
# by= level extraction for grouped crosstabs
#
# Returns a list:
#   level_keys    chr vector — internal key per by-level
#   level_headers chr vector — display header per by-level
#                              (e.g. "sex = 2 [female]")
#   key_vec       chr vector — same length as by_vec, mapping each obs
#                              to its level key; NA when excluded
# -----------------------------------------------------------------------
.crosstab_by_levels <- function(by_vec, by_type, by_name, by_var_label,
                                missing) {
  hdr_name <- if (!is.null(by_var_label) && nzchar(by_var_label)) {
    by_var_label
  } else {
    by_name
  }

  has_haven <- requireNamespace("haven", quietly = TRUE)

  if (by_type == "haven_labelled") {
    .ct_by_haven(by_vec, hdr_name, missing, has_haven)
  } else if (by_type %in% c("factor", "ordered")) {
    .ct_by_factor(by_vec, hdr_name, missing)
  } else if (by_type == "character") {
    .ct_by_character(by_vec, hdr_name, missing)
  } else {
    # numeric
    .ct_by_numeric(by_vec, hdr_name, missing)
  }
}


# --- Factor / ordered by-variable ----------------------------------------
.ct_by_factor <- function(by_vec, hdr_name, missing) {
  lvls    <- levels(by_vec)
  key_vec <- as.character(by_vec)

  level_keys    <- lvls
  level_headers <- paste0(hdr_name, " = ", lvls)

  if (missing && any(is.na(key_vec))) {
    level_keys    <- c(level_keys, ".")
    level_headers <- c(level_headers, paste0(hdr_name, " = ."))
    key_vec[is.na(key_vec)] <- "."
  }

  list(level_keys = level_keys, level_headers = level_headers,
       key_vec = key_vec)
}


# --- Character by-variable -----------------------------------------------
.ct_by_character <- function(by_vec, hdr_name, missing) {
  is_na  <- is.na(by_vec)
  uvals  <- sort(unique(by_vec[!is_na]))

  key_vec       <- by_vec
  level_keys    <- uvals
  level_headers <- paste0(hdr_name, " = ", uvals)

  if (missing && any(is_na)) {
    level_keys    <- c(level_keys, ".")
    level_headers <- c(level_headers, paste0(hdr_name, " = ."))
    key_vec[is_na] <- "."
  }

  list(level_keys = level_keys, level_headers = level_headers,
       key_vec = key_vec)
}


# --- Numeric by-variable -------------------------------------------------
.ct_by_numeric <- function(by_vec, hdr_name, missing) {
  is_na <- is.na(by_vec)
  uvals <- sort(unique(by_vec[!is_na]))

  code_strs <- vapply(uvals, function(v) {
    if (v == as.integer(v)) as.character(as.integer(v)) else as.character(v)
  }, character(1))

  key_vec <- rep(NA_character_, length(by_vec))
  for (i in seq_along(uvals)) {
    key_vec[!is_na & by_vec == uvals[i]] <- code_strs[i]
  }

  level_keys    <- code_strs
  level_headers <- paste0(hdr_name, " = ", code_strs)

  if (missing && any(is_na)) {
    level_keys    <- c(level_keys, ".")
    level_headers <- c(level_headers, paste0(hdr_name, " = ."))
    key_vec[is_na] <- "."
  }

  list(level_keys = level_keys, level_headers = level_headers,
       key_vec = key_vec)
}


# --- Haven-labelled by-variable ------------------------------------------
.ct_by_haven <- function(by_vec, hdr_name, missing, has_haven) {
  haven_labels <- attr(by_vec, "labels", exact = TRUE)

  # code -> label lookup
  if (!is.null(haven_labels) && length(haven_labels) > 0L) {
    code_to_label <- stats::setNames(names(haven_labels),
                                     as.character(haven_labels))
  } else {
    code_to_label <- character(0)
  }

  # Tagged NA detection
  if (has_haven) {
    is_tagged <- haven::is_tagged_na(by_vec)
    na_tags   <- ifelse(is_tagged, haven::na_tag(by_vec), NA_character_)
  } else {
    is_tagged <- rep(FALSE, length(by_vec))
    na_tags   <- rep(NA_character_, length(by_vec))
  }
  is_regular_na <- is.na(by_vec) & !is_tagged

  # Non-missing, non-tagged values
  non_na_non_tagged <- (!is.na(by_vec)) & (!is_tagged)
  uvals <- sort(unique(as.numeric(by_vec[non_na_non_tagged])))

  level_keys    <- character(0)
  level_headers <- character(0)
  key_vec       <- rep(NA_character_, length(by_vec))

  for (v in uvals) {
    code_str <- if (v == as.integer(v)) {
      as.character(as.integer(v))
    } else {
      as.character(v)
    }

    lbl <- code_to_label[code_str]
    if (is.na(lbl)) lbl <- NULL

    header <- if (!is.null(lbl) && nzchar(lbl)) {
      paste0(hdr_name, " = ", code_str, " [", lbl, "]")
    } else {
      paste0(hdr_name, " = ", code_str)
    }

    level_keys    <- c(level_keys, code_str)
    level_headers <- c(level_headers, header)

    mask <- non_na_non_tagged & as.numeric(by_vec) == v
    key_vec[mask] <- code_str
  }

  # Tagged NAs (only when missing=TRUE)
  if (missing && has_haven && any(is_tagged)) {
    unique_tags <- sort(unique(na_tags[is_tagged]))
    for (tag in unique_tags) {
      tag_code  <- paste0(".", tag)
      tag_label <- NA_character_
      if (!is.null(haven_labels)) {
        for (j in seq_along(haven_labels)) {
          if (haven::is_tagged_na(haven_labels[j]) &&
              haven::na_tag(haven_labels[j]) == tag) {
            tag_label <- names(haven_labels)[j]
            break
          }
        }
      }

      header <- if (!is.na(tag_label) && nzchar(tag_label)) {
        paste0(hdr_name, " = ", tag_code, " [", tag_label, "]")
      } else {
        paste0(hdr_name, " = ", tag_code)
      }

      level_keys    <- c(level_keys, tag_code)
      level_headers <- c(level_headers, header)

      mask <- is_tagged & na_tags == tag
      key_vec[mask] <- tag_code
    }
  }

  # Regular NA (only when missing=TRUE)
  if (missing && any(is_regular_na)) {
    level_keys    <- c(level_keys, ".")
    level_headers <- c(level_headers, paste0(hdr_name, " = ."))
    key_vec[is_regular_na] <- "."
  }

  list(level_keys = level_keys, level_headers = level_headers,
       key_vec = key_vec)
}

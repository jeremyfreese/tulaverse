# ---------------------------------------------------------------------------
# tula() summarize path — Stata-inspired -summarize- output for data frames,
# tibbles, and atomic vectors.
#
# Entry point: .tula_summarize(df, width, sep, mad, median, digits)
# Output object: tula_summary (S3), printed by print.tula_summary()
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Internal entry point called by tula.data.frame() and tula.default()
# ---------------------------------------------------------------------------

.tula_summarize <- function(df, width = NULL, sep = 5L, mad = FALSE,
                             median = FALSE, digits = 7L) {
  stopifnot(is.data.frame(df))

  rows_list <- vector("list", ncol(df))
  for (j in seq_along(df)) {
    rows_list[[j]] <- .build_summary_rows(
      col      = df[[j]],
      varname  = names(df)[j],
      var_idx  = j,
      use_mad  = isTRUE(mad),
      use_med  = isTRUE(median)
    )
  }

  rows <- do.call(rbind, rows_list)

  structure(
    list(
      rows  = rows,
      opts  = list(sep = as.integer(sep), mad = isTRUE(mad),
                   median = isTRUE(median), digits = as.integer(digits)),
      width = width
    ),
    class = "tula_summary"
  )
}


# ---------------------------------------------------------------------------
# Build the canonical rows data frame for one variable
# ---------------------------------------------------------------------------

.build_summary_rows <- function(col, varname, var_idx, use_mad, use_med) {

  # Logical -> integer (treat FALSE=0, TRUE=1)
  if (is.logical(col)) col <- as.integer(col)

  if (is.factor(col)) {
    n_obs   <- sum(!is.na(col))
    lvls    <- levels(col)
    # Header row: Obs = total non-missing N; all numeric columns blank
    hdr <- data.frame(
      label    = varname,
      row_type = "factor_header",
      var_idx  = var_idx,
      n_obs    = n_obs,        # total non-missing N shown here
      mean_val = NA_real_,
      sd_val   = NA_real_,
      min_val  = NA_real_,
      max_val  = NA_real_,
      stringsAsFactors = FALSE
    )
    # One row per level:
    #   Obs    = blank
    #   Mean   = blank
    #   Sd/Min = blank
    #   Max    = level count (rendered as "(count/total)", right-aligned,
    #            overflowing left into SD+Min space as needed)
    #   sd_val = total non-missing N stored as sentinel for the formatter
    lvl_rows <- lapply(lvls, function(lv) {
      lv_count <- sum(col == lv, na.rm = TRUE)
      prop     <- if (n_obs == 0L) NA_real_ else lv_count / n_obs
      data.frame(
        label    = paste0("  ", lv),
        row_type = "factor_level",
        var_idx  = var_idx,
        n_obs    = NA_integer_,            # blank in Obs column
        mean_val = prop,                   # proportion in Mean column
        sd_val   = as.numeric(n_obs),      # total N sentinel for formatter
        min_val  = NA_real_,
        max_val  = as.numeric(lv_count),   # level count for formatter
        stringsAsFactors = FALSE
      )
    })
    return(do.call(rbind, c(list(hdr), lvl_rows)))
  }

  if (is.character(col)) {
    n_obs <- sum(!is.na(col))
    return(data.frame(
      label    = varname,
      row_type = "string",
      var_idx  = var_idx,
      n_obs    = n_obs,
      mean_val = NA_real_,   # rendered as "(string)"
      sd_val   = NA_real_,
      min_val  = NA_real_,
      max_val  = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Numeric / integer
  # as.numeric() strips haven_labelled class so that min/max values from

  # different variables can be safely rbound without vctrs casting errors.
  non_na <- as.numeric(col[!is.na(col)])
  n_obs  <- length(non_na)

  mean_val <- if (n_obs == 0L) NA_real_ else {
    if (use_med) stats::median(non_na) else mean(non_na)
  }

  sd_val <- if (n_obs <= 1L) NA_real_ else {
    if (use_med) stats::IQR(non_na)
    else if (use_mad) stats::mad(non_na)
    else stats::sd(non_na)
  }

  min_val <- if (n_obs == 0L) NA_real_ else min(non_na)
  max_val <- if (n_obs == 0L) NA_real_ else max(non_na)

  data.frame(
    label    = varname,
    row_type = "numeric",
    var_idx  = var_idx,
    n_obs    = n_obs,
    mean_val = mean_val,
    sd_val   = sd_val,
    min_val  = min_val,
    max_val  = max_val,
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' Print method for tula_summary objects
#'
#' Prints a Stata-inspired descriptive statistics table with columns for
#' Obs, Mean (or Median), Std. Dev. (or MAD/IQR), Min, and Max.
#' Factor variables are expanded into per-level rows showing counts
#' and proportions.
#'
#' @param x A `tula_summary` object (from `tula()` with a data frame or vector).
#' @param ... Ignored (for S3 compatibility).
#' @return Invisibly returns `x`.
#' @export
print.tula_summary <- function(x, ...) {
  # Fixed numeric column widths
  cw_obs  <- 10L
  cw_mean <- 10L
  cw_sd   <- 12L
  cw_min  <- 10L
  cw_max  <- 10L
  num_cols_w <- 2L + cw_obs + 1L + cw_mean + 1L + cw_sd + 1L + cw_min + 1L + cw_max
  # = 58

  # Resolve width at print time
  max_w <- if (is.null(x$width)) getOption("width") else x$width

  # Natural width = widest label + numeric columns.
  # Label column must be at least as wide as the column header word "Variable"
  # so the | always lines up between the header and data rows.
  min_lbl_w      <- nchar("Variable")   # = 8
  natural_lbl_w  <- max(nchar(x$rows$label), min_lbl_w)
  natural_width  <- natural_lbl_w + num_cols_w
  total_width    <- min(natural_width, max_w)

  lines <- format_summary_table(x$rows, x$opts, total_width)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}


# ---------------------------------------------------------------------------
# Core table formatter
# ---------------------------------------------------------------------------

#' @keywords internal
format_summary_table <- function(rows, opts, total_width) {
  # Fixed column widths
  cw_obs  <- 10L
  cw_mean <- 10L
  cw_sd   <- 12L
  cw_min  <- 10L
  cw_max  <- 10L
  num_cols_w <- 2L + cw_obs + 1L + cw_mean + 1L + cw_sd + 1L + cw_min + 1L + cw_max

  lbl_w <- max(total_width - num_cols_w, 1L)

  sep_line <- char_rep(.BOX_H, total_width)

  # Column headers vary by options
  mean_hdr <- if (opts$median) "Median" else "Mean"
  sd_hdr   <- if (opts$median) "IQR" else if (opts$mad) "MAD" else "Std. dev."

  hdr_line <- sprintf(
    paste0("%s ", .BOX_V, "%s %s %s %s %s"),
    pad_right("Variable", lbl_w),
    pad_left("Obs",      cw_obs),
    pad_left(mean_hdr,   cw_mean),
    pad_left(sd_hdr,     cw_sd),
    pad_left("Min",      cw_min),
    pad_left("Max",      cw_max)
  )

  lines <- c(sep_line, hdr_line, sep_line)

  # Determine where separators go.
  # sep=0 means no separators; otherwise insert after every opts$sep-th
  # unique var_idx, but never inside a factor block.
  sep_n     <- opts$sep
  use_sep   <- sep_n > 0L
  n_rows    <- nrow(rows)

  # Precompute for each row whether a separator should be inserted AFTER it.
  # A separator fires when:
  #   - We've just completed a variable that is the sep_n-th in a group
  #   - AND the next row belongs to a different var_idx (i.e., the factor
  #     block, if any, is fully rendered).
  insert_sep_after <- logical(n_rows)

  if (use_sep) {
    # Unique var indices in order
    all_var_idx <- unique(rows$var_idx)
    n_vars      <- length(all_var_idx)
    # Which var_idx values should trigger a separator after their last row?
    # Every sep_n-th variable (but not after the very last variable).
    # seq() errors if from > to, so guard explicitly
    trigger_vars <- if (sep_n >= n_vars) integer(0L) else
                    all_var_idx[seq(sep_n, n_vars - 1L, by = sep_n)]

    # For each trigger var, find the last row belonging to it
    for (tv in trigger_vars) {
      last_row_of_var <- max(which(rows$var_idx == tv))
      insert_sep_after[last_row_of_var] <- TRUE
    }
  }

  for (i in seq_len(n_rows)) {
    row <- rows[i, ]

    # Truncate & pad label
    lbl     <- .truncate_label(row$label, lbl_w)
    lbl_fmt <- pad_right(lbl, lbl_w)

    line <- switch(
      row$row_type,

      "numeric" = sprintf(
        paste0("%s ", .BOX_V, "%s %s %s %s %s"),
        lbl_fmt,
        .fmt_obs(row$n_obs,    cw_obs),
        .fmt_sum(row$mean_val, digits = opts$digits, width = cw_mean),
        .fmt_sum(row$sd_val,   digits = opts$digits, width = cw_sd),
        .fmt_sum(row$min_val,  digits = opts$digits, width = cw_min),
        .fmt_sum(row$max_val,  digits = opts$digits, width = cw_max)
      ),

      "string" = {
        # Mean column shows "(string)", rest blank
        str_tag  <- pad_left("(string)", cw_mean)
        blank_sd <- strrep(" ", cw_sd)
        blank_mn <- strrep(" ", cw_min)
        blank_mx <- strrep(" ", cw_max)
        sprintf(
          paste0("%s ", .BOX_V, "%s %s %s %s %s"),
          lbl_fmt,
          .fmt_obs(row$n_obs, cw_obs),
          str_tag,
          blank_sd,
          blank_mn,
          blank_mx
        )
      },

      "factor_header" = {
        # Variable name row: Obs = total non-missing N; all other columns blank
        sprintf(
          paste0("%s ", .BOX_V, "%s %s %s %s %s"),
          lbl_fmt,
          .fmt_obs(row$n_obs, cw_obs),    # total non-missing N
          strrep(" ", cw_mean),
          strrep(" ", cw_sd),
          strrep(" ", cw_min),
          strrep(" ", cw_max)
        )
      },

      "factor_level" = {
        # Obs = blank; Mean = proportion; SD, Min = blank
        # Right side: "(count/total)" right-aligned, allowed to overflow
        # left into the Min and SD space (combined = cw_sd+1+cw_min+1+cw_max)
        lv_count_str <- paste0("(", as.integer(row$max_val), "/",
                                    as.integer(row$sd_val),  ")")
        right_w <- cw_sd + 1L + cw_min + 1L + cw_max   # = 34
        right_field <- pad_left(lv_count_str, right_w)
        sprintf(
          paste0("%s ", .BOX_V, "%s %s %s"),
          lbl_fmt,
          strrep(" ", cw_obs),                                    # Obs blank
          .fmt_sum(row$mean_val, digits = opts$digits, width = cw_mean), # proportion
          right_field                                             # SD+Min+Max zone
        )
      },

      # Fallback (should not occur)
      sprintf(paste0("%s ", .BOX_V), lbl_fmt)
    )

    lines <- c(lines, line)

    if (insert_sep_after[i]) {
      lines <- c(lines, sep_line)
    }
  }

  lines <- c(lines, sep_line)
  lines
}


# ---------------------------------------------------------------------------
# Helper: format a numeric value for summarize columns.
#
# Like fmt_num() but with an extra rule: for |x| > 0.1, cap at 3 decimal
# places. This prevents long decimal trains on values like means and SDs
# while still using significant-digit formatting for small values (e.g.
# proportions near 0).
#
# For |x| > 0.1 the displayed decimal places are:
#   min(3, decimal places implied by `digits` significant figures)
# so large round-ish numbers (e.g. 75000) don't sprout trailing zeros.
# ---------------------------------------------------------------------------

.fmt_sum <- function(x, digits = 7L, width = 10L, dec = NULL) {
  if (is.na(x)) return(strrep(" ", width))

  if (!is.null(dec)) {
    # Explicit decimal cap requested (e.g. tulatab dec= option):
    # always use fixed-format with exactly `dec` decimal places.
    s <- sprintf("%.*f", dec, x)
  } else if (abs(x) > 0.1) {
    # Smart mode (summarize path): cap at 3 decimal places for larger values
    g_str    <- formatC(x, digits = digits, format = "g", flag = " ")
    g_dp     <- if (grepl("\\.", g_str)) {
      nchar(sub(".*\\.", "", trimws(g_str)))
    } else {
      0L
    }
    dp       <- min(3L, g_dp)
    s        <- sprintf("%.*f", dp, x)
  } else {
    # Smart mode: significant-digit formatting for small values
    s <- formatC(x, digits = digits, format = "g", flag = " ")
    s <- trimws(s)
  }

  s <- .strip_lead_zero(s)
  formatC(s, width = width, flag = " ")
}


# ---------------------------------------------------------------------------
# Helper: format an observation count (integer, right-aligned)
# ---------------------------------------------------------------------------

.fmt_obs <- function(n, width) {
  if (is.na(n)) return(strrep(" ", width))
  formatC(as.integer(n), format = "d", width = width)
}

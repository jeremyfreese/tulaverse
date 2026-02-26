#' @keywords internal
#'
#' Build the canonical coefficient data frame from a model.
#'
#' Extracts the coefficient matrix and confidence intervals, then groups
#' factor variable dummy rows under a shared header row (Stata-inspired layout).
#' The intercept is always placed last.
#'
#' Factor grouping uses the `assign` attribute of `model.matrix()` to map
#' each coefficient to its model term, and `dataClasses` to identify which
#' terms are factors. This avoids fragile string-matching on coefficient names.
#'
#' @param model A fitted model object (lm, glm, ...).
#' @param ct Matrix from `coef(summary(model))`: columns are estimate, SE,
#'   statistic, p-value; rows are named by coefficient. Required.
#' @param ci Matrix from `confint()` or NULL. Row names match `ct`.
#' @param wide Logical. If TRUE, ci_lower and ci_upper are populated.
#' @param ref Logical. If TRUE, show the reference level of each factor as the
#'   last row in its group, with estimate = 0 and blanks for SE/stat/p.
#' @param label Logical. If TRUE, use haven value labels for factor levels when
#'   available.
#' @param assign_vec Optional integer vector: the `assign` attribute of the
#'   model matrix (0 = intercept, k = k-th term). When NULL, extracted via
#'   `attr(model.matrix(model), "assign")`. Supply explicitly for models where
#'   `model.matrix()` is unavailable or returns a non-standard result.
#' @param term_labels Optional character vector of term labels (the
#'   `"term.labels"` attribute of `terms(model)`). Supply with `assign_vec`.
#' @param data_classes Optional named character vector of data classes (the
#'   `"dataClasses"` attribute of `terms(model)`), used to identify factor
#'   terms. Supply with `assign_vec` and `term_labels`.
#' @param xlevels Optional named list mapping factor term names to their level
#'   vectors (equivalent to `model$xlevels`). Used to identify the reference
#'   level when `ref = TRUE`. Supply when `model$xlevels` is absent.
#' @param model_frame Optional data frame (equivalent to `model.frame(model)`),
#'   used for haven label lookup. Supply when `model.frame()` is unavailable.
#' @param orig_data Optional data frame: the raw input data, used as a fallback
#'   for haven label lookup when factor-wrapping in the formula strips labels.
#'
#' @return A data frame with columns:
#'   label, is_factor_header, is_intercept, is_ref, is_cutpoint,
#'   estimate, std_err, statistic, p_value, ci_lower, ci_upper.
build_coef_df <- function(model, ct, ci, wide, ref = FALSE, label = TRUE,
                          assign_vec   = NULL,
                          term_labels  = NULL,
                          data_classes = NULL,
                          xlevels      = NULL,
                          model_frame  = NULL,
                          orig_data    = NULL) {
  coef_names <- rownames(ct)

  # Retrieve term-to-column mapping from the model matrix.
  # Callers may supply these directly for models where model.matrix() is
  # unavailable or returns a non-standard structure.
  if (is.null(assign_vec) || is.null(term_labels)) {
    mm          <- tryCatch(model.matrix(model), error = function(e) NULL)
    assign_vec  <- if (!is.null(mm)) {
      av <- attr(mm, "assign")
      if (!is.null(av)) av else rep(seq_along(coef_names) - 1L, 1L)
    } else {
      rep(seq_along(coef_names) - 1L, 1L)  # fallback: 0,1,2,...
    }
    term_labels <- if (!is.null(mm))
      attr(terms(model), "term.labels") else character(0)
  }

  # Final safety guard: if assign_vec is still NULL (model.matrix did not set
  # the assign attribute and no fallback caught it), use sequential non-zero
  # indices so the loop never sees a NULL subscript.
  if (is.null(assign_vec)) {
    assign_vec <- seq_along(coef_names)   # 1, 2, 3, ... — no intercept row
  }

  if (is.null(data_classes)) {
    data_classes <- tryCatch(
      attr(terms(model), "dataClasses"),
      error = function(e) character(0)
    )
  }

  if (is.null(xlevels)) {
    xlevels <- if (!is.null(model$xlevels)) model$xlevels else list()
  }

  # Model frame for label lookup (haven labels live here for bare variables).
  # Use caller-supplied value if provided; otherwise try model.frame().
  mf <- if (!is.null(model_frame)) model_frame else
        tryCatch(model.frame(model), error = function(e) NULL)

  # Original data frame for label lookup when the variable is wrapped in
  # factor() in the formula. Use caller-supplied value if provided.
  orig_data <- if (!is.null(orig_data)) orig_data else
    tryCatch(
      eval(model$call$data, envir = environment(formula(model))),
      error = function(e) NULL
    )

  # Build a mapping from model-matrix column index to term name.
  # assign_vec uses 0 for intercept-type terms, 1-based for other terms.
  # We use the assign_vec position rather than coefficient names so that
  # models with non-standard intercept names (or no intercept at all) work.
  term_for_coef <- function(i) {
    tidx <- assign_vec[i]
    if (tidx == 0L) return("(Intercept)")
    term_labels[tidx]
  }

  # Which positions in ct correspond to intercept-type terms (assign == 0)?
  # Using assign_vec rather than name-matching so models that omit an intercept
  # or name it differently (e.g. no parentheses) are handled correctly.
  intercept_positions <- which(assign_vec == 0L)

  # Determine whether a term is a factor type
  is_factor_term <- function(term_nm) {
    if (length(data_classes) == 0) return(FALSE)
    dc <- data_classes[term_nm]
    if (is.na(dc)) return(FALSE)
    dc %in% c("factor", "ordered")
  }

  # Strip function wrappers like factor(), ordered(), I() from a term name
  # so that "factor(cyl)" displays as "cyl" in the group header row.
  strip_fn_wrapper <- function(term_nm) {
    gsub("^(?:factor|ordered|I)\\((.+)\\)$", "\\1", term_nm, perl = TRUE)
  }

  # Build a haven value-label lookup for a given variable.
  # Tries, in order:
  #   1. mf[[bare_nm]]        — works when variable appears bare in formula
  #   2. mf[[term_nm]]        — works for some wrapped cases
  #   3. orig_data[[bare_nm]] — recovers labels when factor() wrapper stripped them
  # Returns a named character vector (code -> label text) or NULL if no labels.
  get_haven_labels <- function(bare_nm, term_nm) {
    get_labels_from_col <- function(col) {
      if (is.null(col)) return(NULL)
      lbs <- attr(col, "labels", exact = TRUE)
      if (is.null(lbs) || length(lbs) == 0L) return(NULL)
      # lbs: named numeric vector where names=label text, values=codes
      # Invert to map character(code) -> label text
      stats::setNames(names(lbs), as.character(lbs))
    }
    # Try model frame with bare name first
    result <- get_labels_from_col(if (!is.null(mf)) mf[[bare_nm]] else NULL)
    if (!is.null(result)) return(result)
    # Try model frame with full term name (e.g. "factor(cyl_lbl)")
    result <- get_labels_from_col(if (!is.null(mf)) mf[[term_nm]] else NULL)
    if (!is.null(result)) return(result)
    # Fall back to original data frame (recovers labels after factor() wrapping)
    get_labels_from_col(if (!is.null(orig_data)) orig_data[[bare_nm]] else NULL)
  }

  # Resolve the display label for a factor level suffix.
  # suffix   - the raw suffix stripped from the coefficient name (e.g. "6")
  # bare_nm  - bare variable name (e.g. "cyl_lbl") for haven label lookup
  # term_nm  - full term name (e.g. "factor(cyl_lbl)") as fallback key
  resolve_level_label <- function(suffix, bare_nm, term_nm) {
    if (!isTRUE(label)) return(suffix)
    lbs <- get_haven_labels(bare_nm, term_nm)
    if (is.null(lbs)) return(suffix)
    lbs[[suffix]] %||% suffix
  }

  rows <- list()
  last_factor_term <- NULL

  for (i in seq_along(coef_names)) {
    nm <- coef_names[i]

    # Intercept-type terms are handled separately at end (identified by
    # assign_vec == 0, not by name, so models with no/renamed intercept work)
    if (i %in% intercept_positions) next

    term_nm <- term_for_coef(i)
    is_fac  <- is_factor_term(term_nm)

    if (is_fac) {
      bare_nm <- strip_fn_wrapper(term_nm)

      # Level label = coefficient name minus the original term name prefix
      level_suffix  <- substring(nm, nchar(term_nm) + 1L)
      level_display <- resolve_level_label(level_suffix, bare_nm, term_nm)

      # Collapse condition: suppress the separate group-header row and use the
      # bare variable name directly on the coefficient row when ALL of:
      #   1. ref = FALSE  (with ref=TRUE a binary factor produces 2 rows, not 1)
      #   2. the factor has exactly 2 levels (so only 1 non-reference row prints)
      #   3. the non-reference level label matches the bare variable name
      #      (case-insensitive) — the typical "male"/"male" redundancy pattern
      xlvls      <- xlevels[[term_nm]]
      collapsing <- !isTRUE(ref) &&
                    !is.null(xlvls) && length(xlvls) == 2L &&
                    tolower(trimws(level_display)) == tolower(bare_nm)

      # Emit a group header row the first time we see this factor term,
      # unless we are collapsing into a single row.
      if (is.null(last_factor_term) || last_factor_term != term_nm) {
        if (!collapsing) rows[[length(rows) + 1L]] <- .make_header_row(bare_nm)
        last_factor_term <- term_nm
      }

      lbl <- if (collapsing) bare_nm else paste0("  ", level_display)
    } else {
      last_factor_term <- NULL
      lbl              <- gsub(":", "*", nm, fixed = TRUE)
    }

    rows[[length(rows) + 1L]] <- .make_coef_row(
      label        = lbl,
      is_intercept = FALSE,
      nm           = nm,
      ct           = ct,
      ci           = ci,
      i            = i
    )

    # When ref=TRUE and this is the last coefficient for this factor term,
    # append a reference-level row with estimate = 0, blanks for rest.
    if (is_fac && isTRUE(ref)) {
      # Check if the NEXT coefficient belongs to a different term (or is the intercept)
      next_i   <- i + 1L
      next_nm  <- if (next_i <= length(coef_names)) coef_names[next_i] else "(Intercept)"
      next_trm <- if (next_nm == "(Intercept)") "(Intercept)" else term_for_coef(next_i)

      if (next_trm != term_nm) {
        # Emit reference level row (reference = xlevels[[term_nm]][1])
        ref_level_raw     <- xlevels[[term_nm]][1L]
        ref_level_display <- resolve_level_label(ref_level_raw, bare_nm, term_nm)
        ref_lbl           <- paste0("  ", ref_level_display)
        rows[[length(rows) + 1L]] <- .make_ref_row(ref_lbl)
      }
    }
  }

  # Intercept-type terms go last (identified by assign_vec == 0).
  # There may be zero (e.g. Cox models) or more than one (uncommon but possible).
  # Each is labelled with its actual coefficient name from the model.
  for (int_i in intercept_positions) {
    rows[[length(rows) + 1L]] <- .make_coef_row(
      label        = coef_names[int_i],
      is_intercept = TRUE,
      nm           = coef_names[int_i],
      ct           = ct,
      ci           = ci,
      i            = int_i
    )
  }

  do.call(rbind, rows)
}

# Internal: create a factor group header row (no numeric values)
.make_header_row <- function(term_nm) {
  data.frame(
    label            = term_nm,
    is_factor_header = TRUE,
    is_intercept     = FALSE,
    is_ref           = FALSE,
    is_cutpoint      = FALSE,
    estimate         = NA_real_,
    std_err          = NA_real_,
    statistic        = NA_real_,
    p_value          = NA_real_,
    ci_lower         = NA_real_,
    ci_upper         = NA_real_,
    stringsAsFactors = FALSE
  )
}

# Internal: create a regular coefficient row
.make_coef_row <- function(label, is_intercept, nm, ct, ci, i) {
  data.frame(
    label            = label,
    is_factor_header = FALSE,
    is_intercept     = is_intercept,
    is_ref           = FALSE,
    is_cutpoint      = FALSE,
    estimate         = ct[i, 1L],
    std_err          = ct[i, 2L],
    statistic        = ct[i, 3L],
    p_value          = ct[i, 4L],
    ci_lower         = if (!is.null(ci)) ci[nm, 1L] else NA_real_,
    ci_upper         = if (!is.null(ci)) ci[nm, 2L] else NA_real_,
    stringsAsFactors = FALSE
  )
}

# Internal: create a reference-level row (estimate = 0, rest blank)
.make_ref_row <- function(label) {
  data.frame(
    label            = label,
    is_factor_header = FALSE,
    is_intercept     = FALSE,
    is_ref           = TRUE,
    is_cutpoint      = FALSE,
    estimate         = 0,
    std_err          = NA_real_,
    statistic        = NA_real_,
    p_value          = NA_real_,
    ci_lower         = NA_real_,
    ci_upper         = NA_real_,
    stringsAsFactors = FALSE
  )
}

# Internal: create a single cutpoint (threshold) row.
# Cutpoints show estimate and SE but not z or p-value (Stata convention).
.make_cutpoint_row <- function(label, estimate, std_err, ci_lower, ci_upper) {
  data.frame(
    label            = label,
    is_factor_header = FALSE,
    is_intercept     = FALSE,
    is_ref           = FALSE,
    is_cutpoint      = TRUE,
    estimate         = estimate,
    std_err          = std_err,
    statistic        = NA_real_,   # blank in output (Stata convention)
    p_value          = NA_real_,   # blank in output (Stata convention)
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    stringsAsFactors = FALSE
  )
}

# Internal: build a data frame of cutpoint rows from the cutpoint portion of
# the summary coefficient matrix and (optionally) the CI matrix.
#
# ct_zeta: matrix with at least 2 cols [estimate, SE]; rownames = cutpoint names
# ci_zeta: 2-column CI matrix (rownames match ct_zeta), or NULL
.build_cutpoint_rows <- function(ct_zeta, ci_zeta) {
  rows <- lapply(seq_len(nrow(ct_zeta)), function(i) {
    nm <- rownames(ct_zeta)[i]
    .make_cutpoint_row(
      label    = nm,
      estimate = ct_zeta[i, 1L],
      std_err  = ct_zeta[i, 2L],
      ci_lower = if (!is.null(ci_zeta)) ci_zeta[nm, 1L] else NA_real_,
      ci_upper = if (!is.null(ci_zeta)) ci_zeta[nm, 2L] else NA_real_
    )
  })
  do.call(rbind, rows)
}

# Null-coalescing operator (avoid importing rlang)
`%||%` <- function(x, y) if (!is.null(x)) x else y


# Internal: truncate a label to fit within `width` characters.
#
# Two-stage truncation:
#
# Stage 1 — Word-boundary truncation (tried first when label has >= 3 words):
#   Split on [_.] separators. Identify the "anchor": the last word if it is
#   pure digits (a year/number suffix), otherwise the last word regardless.
#   The "middle words" are everything between the first word and the anchor.
#   Generate candidates by dropping progressively more middle words from
#   right-to-left, joining with ~ replacing the dropped segment (no separator
#   around ~). Return the first candidate that fits within `width`.
#
#   Examples:
#     "fuel_efficiency_score_2024" -> "fuel~score_2024" (dropped "efficiency")
#     "vehicle_weight_measurement_2024" -> "vehicle_weight~2024"
#                                          (dropped "measurement")
#     "a_b_c_d_2024" tries: "a_b_c~2024", "a_b~d_2024", "a~d_2024", ...
#
# Stage 2 — Character-level truncation (fallback):
#   Identify the "suffix" to preserve:
#     - Trailing run of digits (e.g. "score2024" -> suffix "2024"), OR
#     - Single last character (e.g. "wallaby" -> suffix "y").
#   Produce: <stem>~<suffix>, exactly `width` characters.
#   Sub-fallback: if suffix alone is too long, use last character with ~ in
#   the second-to-last position.
#
# If the label already fits, it is returned unchanged.
.truncate_label <- function(lbl, width) {
  n <- nchar(lbl)
  if (n <= width) return(lbl)

  # ---- Stage 1: Word-boundary truncation ----------------------------------

  # Split label into (words, separators) preserving separator characters
  # e.g. "fuel_efficiency_score_2024" -> words=c("fuel","efficiency","score","2024")
  #                                      seps=c("_","_","_")
  word_pos <- gregexpr("[^_.]+", lbl, perl = TRUE)[[1L]]
  words    <- regmatches(lbl, gregexpr("[^_.]+", lbl, perl = TRUE))[[1L]]
  sep_pos  <- gregexpr("[_.]", lbl, perl = TRUE)[[1L]]
  seps     <- if (sep_pos[1L] == -1L) character(0L) else
                regmatches(lbl, gregexpr("[_.]", lbl, perl = TRUE))[[1L]]

  nw <- length(words)

  # Need at least 3 words to have any middle words to drop
  if (nw >= 3L) {
    # Anchor: last word if it is pure digits, otherwise last word regardless
    anchor_idx <- nw     # always the last word

    # Middle word indices (between first and anchor, exclusive)
    mid_idx <- seq_len(nw - 2L) + 1L   # indices 2 .. (nw-1)

    # Reconstruct a label from a subset of words.
    # The dropped segment is replaced by a single "~" with no surrounding sep.
    # kept_before: indices of words kept before the gap
    # kept_after:  indices of words kept after the gap (up to end)
    make_candidate <- function(kept_before, kept_after) {
      # Build the "before" part including its trailing separators
      before_str <- paste0(
        sapply(kept_before, function(idx) {
          sep_after <- if (idx <= length(seps)) seps[idx] else ""
          paste0(words[idx], sep_after)
        }),
        collapse = ""
      )
      # The before part ends with a sep character — strip it, then append ~
      # Actually we want: words[before] + <sep_between_last_kept_before_and_first_dropped>
      # Then ~
      # Then <sep_between_last_dropped_and_first_kept_after> + words[after]
      # BUT user wants no separator around ~. So strip trailing sep from before
      # and leading sep from after.
      #
      # Simple approach: join kept_before words with their inter-word seps,
      # then "~", then join kept_after words with their inter-word seps.
      before_parts <- character(length(kept_before))
      for (k in seq_along(kept_before)) {
        idx <- kept_before[k]
        sep_after_w <- if (k < length(kept_before) && idx <= length(seps)) seps[idx] else ""
        before_parts[k] <- paste0(words[idx], sep_after_w)
      }

      after_parts <- character(length(kept_after))
      for (k in seq_along(kept_after)) {
        idx <- kept_after[k]
        sep_after_w <- if (k < length(kept_after) && idx <= length(seps)) seps[idx] else ""
        after_parts[k] <- paste0(words[idx], sep_after_w)
      }

      paste0(paste0(before_parts, collapse = ""), "~",
             paste0(after_parts,  collapse = ""))
    }

    # Generate candidates: drop progressively more middle words from right-to-left.
    # For mid_idx = c(2, 3, 4) (3 middle words), candidates drop:
    #   drop {4}         -> kept_before=c(1,2,3), kept_after=c(5)      -> "a_b_c~e"
    #   drop {3,4}       -> kept_before=c(1,2),   kept_after=c(5)      -> "a_b~e"
    #   drop {2,3,4}     -> kept_before=c(1),     kept_after=c(5)      -> "a~e"
    # Then drop anchor from right side too:
    #   drop {3,4} keep more before... but anchor is always kept.
    # We only drop from mid_idx (never the first word or the anchor).

    n_mid <- length(mid_idx)
    # Generate candidates in the order: for k = 0, 1, 2, ..., n_mid - 1
    #   k = number of middle words kept on the "after" side of the ~
    #   The drop zone is the rightmost (n_mid - k) words shifted left each step.
    #
    # Example: mid = [b, c, d] (idx 2,3,4), anchor = [2024] (idx 5)
    #   k=0: drop all mid, kept_after=[anchor]       -> "a~2024"
    #   k=1: drop mid[1..n_mid-1], kept_after=[d,anchor] -> "a~d_2024"
    #   k=2: drop mid[1..n_mid-2], kept_after=[c,d,anchor] -> "a~c_d_2024"
    #
    # But the user wants "a_b_c~2024" first (drop only last middle word),
    # then "a_b~d_2024" (slide drop zone left), then "a~d_2024" (expand drop).
    #
    # Correct ordering (user spec): slide the tilde leftward, expanding drop zone
    # rightward toward the anchor each time it doesn't fit.  The first candidate
    # drops the rightmost single middle word (keeping everything else in "before").
    #
    #   j = index of the rightmost middle word that stays in "before"
    #   j goes from n_mid down to 0 (0 = no middle words in before)
    #   after  = mid[(j+1)..n_mid that are NOT dropped] + anchor
    #
    # The pattern from the example:
    #   j=n_mid-1: kept_before=[1..mid[n_mid-1]], drop={mid[n_mid]},     after=[anchor]
    #   j=n_mid-2: kept_before=[1..mid[n_mid-2]], drop={mid[n_mid-1]},   after=[mid[n_mid],anchor]
    #   j=n_mid-3: kept_before=[1..mid[n_mid-3]], drop={mid[n_mid-2..n_mid-1]}, after=[mid[n_mid],anchor]
    # ... so the drop zone always ends at mid[n_mid-1] and grows left.
    #
    # Simplified: for step s = 1, 2, ..., n_mid:
    #   kept_before_mid = mid_idx[seq_len(n_mid - s)]   (first n_mid-s middle words)
    #   kept_after_mid  = mid_idx[n_mid]                 (only last middle word in after, for s < n_mid)
    #   drop = mid_idx[(n_mid-s+1):(n_mid-1)]            (the s-1 words just before the last)
    #
    # Actually the user's example for a_b_c_d_2024:
    #   1: a_b_c~2024 = keep[a,b,c] drop[d] after[2024]   -> drop rightmost mid
    #   2: a_b~d_2024 = keep[a,b]   drop[c] after[d,2024] -> drop next-to-last mid, keep last in after
    #   3: a~d_2024   = keep[a]   drop[b,c] after[d,2024] -> expand drop, still keep last in after
    #
    # Pattern: the "kept_after_mid" is always just the very last middle word (unless
    # we've consumed it too).  The drop zone grows leftward.
    for (s in seq_len(n_mid)) {
      if (s == 1L) {
        # Drop only the rightmost middle word; nothing from mid goes to "after"
        kept_before_mid <- mid_idx[seq_len(n_mid - 1L)]
        kept_after_mid  <- integer(0L)
      } else {
        # Keep the last middle word in "after"; grow the drop zone leftward
        drop_count      <- s - 1L   # words dropped from the zone just before last
        kept_before_mid <- mid_idx[seq_len(max(0L, n_mid - 1L - drop_count))]
        kept_after_mid  <- mid_idx[n_mid]  # always the very last middle word
      }

      kept_before <- c(1L, kept_before_mid)
      kept_after  <- c(kept_after_mid, anchor_idx)

      cand <- make_candidate(kept_before, kept_after)
      if (nchar(cand) <= width) return(cand)
    }

    # If even "first_word~last_middle_word_anchor" doesn't fit, try "first_word~anchor"
    cand_last <- make_candidate(c(1L), c(anchor_idx))
    if (nchar(cand_last) <= width) return(cand_last)

    # Fall through to Stage 2.
  }

  # ---- Stage 2: Character-level truncation --------------------------------

  # Identify trailing digit run (suffix to preserve)
  digit_match <- regmatches(lbl, regexpr("[0-9]+$", lbl))
  has_digits  <- length(digit_match) > 0L && nchar(digit_match) > 0L

  suffix <- if (has_digits) digit_match else substr(lbl, n, n)

  # Layout: [stem][~][suffix] = width  =>  stem_len = width - 1 - nchar(suffix)
  stem_len <- width - 1L - nchar(suffix)

  if (stem_len >= 1L) {
    paste0(substr(lbl, 1L, stem_len), "~", suffix)
  } else {
    # Sub-fallback: suffix too long; preserve only the last character
    last_char <- substr(lbl, n, n)
    paste0(substr(lbl, 1L, width - 2L), "~", last_char)
  }
}


#' @keywords internal
#'
#' Format the coefficient table as a character vector of printable lines.
#'
#' Renders the canonical coef_df produced by `build_coef_df()` into Stata-
#' style output: separator lines, a header row, factor group headers, indented
#' factor levels, and the intercept at the bottom after a separator.
#'
#' @param coef_df Data frame from `build_coef_df()`.
#' @param stat_label Character. Column header for the test statistic ("t" or "z").
#' @param wide Logical. If TRUE, include CI columns.
#' @param total_width Integer. Total line width (must equal header total width).
#'   The label column expands to fill: lbl_w = total_width - num_cols_width.
#' @param exp Logical. If TRUE, exponentiate estimates and compute delta-method
#'   SEs. Test statistics and p-values are unchanged. Cutpoint rows are never
#'   exponentiated. Default FALSE.
#' @param exp_label Character or NULL. When non-NULL and `exp = TRUE`, replaces
#'   the default `"exp(b)"` column header (e.g. `"IRR"`, `"Haz. Ratio"`).
#' @param level Numeric. CI width as a percentage (e.g. 95, 90, 99). Controls
#'   the CI column header label (e.g. `"[95% Conf"` vs `"[90% Conf"`).
#'   Default 95.
#' @param se_label Character or NULL. When non-NULL, overrides the SE column
#'   header (e.g. `"Robust SE"`). Takes precedence over `"DMSE"` when both
#'   `se_label` and `exp` are active.
#'
#' @return Character vector, one element per line.
format_coef_table <- function(coef_df, stat_label, wide, total_width,
                              exp = FALSE, exp_label = NULL, level = 95,
                              se_label = NULL) {
  # Fixed numeric column widths.
  # stat is 10 (was 8) so values like "-5.08e+06" don't overflow.
  # pval is 9 (was 8) for a little more breathing room around "P>|z|".
  cw_coef <- 10L
  cw_se   <- 10L
  cw_stat <- 10L
  cw_pval <-  9L
  cw_ci   <- 10L   # each CI bound

  # Numeric portion width (includes the " |" separator after label)
  num_cols_w <- 2L + cw_coef + 1L + cw_se + 1L + cw_stat + 1L + cw_pval
  if (wide) num_cols_w <- num_cols_w + 1L + cw_ci + 1L + cw_ci

  # Label column width derived from total_width
  lbl_w <- total_width - num_cols_w

  sep <- char_rep(.BOX_H, total_width)

  # P-value column header label
  pval_hdr <- if (stat_label == "z") "P>|z|" else "P>|t|"

  # Column header labels: change when exp = TRUE or robust SE is used.
  # se_label (Robust SE) takes precedence over DMSE: if robust SEs were used,
  # that is more important to signal than the delta-method transformation.
  # DMSE appears only when exp = TRUE and no robust SE was requested.
  coef_hdr <- if (exp) (exp_label %||% "exp(b)") else "Coef"
  se_hdr   <- if (!is.null(se_label)) se_label else if (exp) "DMSE" else "Std. Err."

  # CI column header: format level as integer when it has no fractional part
  # (e.g. 95 → "[95% Conf", 99.9 → "[99.9% Conf")
  ci_pct  <- if (level == as.integer(level)) as.integer(level) else level
  ci_hdr1 <- paste0("[", ci_pct, "% Conf")

  # Build column header line
  if (wide) {
    hdr <- sprintf(
      paste0("%s ", .BOX_V, "%s %s %s %s %s %s"),
      pad_right("", lbl_w),
      pad_left(coef_hdr,   cw_coef),
      pad_left(se_hdr,     cw_se),
      pad_left(stat_label, cw_stat),
      pad_left(pval_hdr,   cw_pval),
      pad_left(ci_hdr1,    cw_ci),
      pad_left("Interval]", cw_ci)
    )
  } else {
    hdr <- sprintf(
      paste0("%s ", .BOX_V, "%s %s %s %s"),
      pad_right("", lbl_w),
      pad_left(coef_hdr,    cw_coef),
      pad_left(se_hdr,      cw_se),
      pad_left(stat_label,  cw_stat),
      pad_left(pval_hdr,    cw_pval)
    )
  }

  lines <- c(sep, hdr, sep)

  cutpoint_sep_done <- FALSE

  for (i in seq_len(nrow(coef_df))) {
    row <- coef_df[i, ]

    is_cp <- !is.null(row$is_cutpoint) && isTRUE(row$is_cutpoint)

    # Insert a separator before the first cutpoint row (Stata layout)
    if (is_cp && !cutpoint_sep_done) {
      lines <- c(lines, sep)
      cutpoint_sep_done <- TRUE
    }

    # Truncate label if needed
    lbl     <- row$label
    lbl     <- .truncate_label(lbl, lbl_w)
    lbl_fmt <- pad_right(lbl, lbl_w)

    if (isTRUE(row$is_factor_header)) {
      # Factor group header row: label + pipe + trailing spaces to fill total_width
      trailing <- strrep(" ", total_width - lbl_w - 2L)
      lines <- c(lines, paste0(lbl_fmt, " ", .BOX_V, trailing))

    } else if (isTRUE(row$is_ref)) {
      # Reference-level row: show 0 (or 1 when exp) for Coef, blanks for rest
      ref_val    <- if (exp) "1" else "0"
      blank_se   <- strrep(" ", cw_se)
      blank_stat <- strrep(" ", cw_stat)
      blank_pval <- strrep(" ", cw_pval)
      if (wide) {
        blank_ci <- strrep(" ", cw_ci)
        line <- sprintf(
          paste0("%s ", .BOX_V, "%s %s %s %s %s %s"),
          lbl_fmt,
          pad_left(ref_val, cw_coef),
          blank_se,
          blank_stat,
          blank_pval,
          blank_ci,
          blank_ci
        )
      } else {
        line <- sprintf(
          paste0("%s ", .BOX_V, "%s %s %s %s"),
          lbl_fmt,
          pad_left(ref_val, cw_coef),
          blank_se,
          blank_stat,
          blank_pval
        )
      }
      lines <- c(lines, line)

    } else {
      # When exp = TRUE, exponentiate estimate and compute delta-method SE.
      # Cutpoint rows are never exponentiated (they are threshold parameters,
      # not log-odds coefficients).
      # The test statistic and p-value are unchanged for all rows.
      est_display <- if (exp && !is_cp) exp(row$estimate) else row$estimate
      se_display  <- if (exp && !is_cp) exp(row$estimate) * row$std_err else row$std_err

      # When exp = TRUE and wide, exponentiate CI bounds (cutpoints excluded).
      ci_lo <- row$ci_lower
      ci_hi <- row$ci_upper
      if (exp && !is_cp && wide) {
        ci_lo <- exp(ci_lo)
        ci_hi <- exp(ci_hi)
      }

      if (wide) {
        line <- sprintf(
          paste0("%s ", .BOX_V, "%s %s %s %s %s %s"),
          lbl_fmt,
          fmt_num(est_display,   width = cw_coef),
          fmt_num(se_display,    width = cw_se),
          fmt_num(row$statistic, width = cw_stat),
          fmt_pval(row$p_value,  width = cw_pval),
          fmt_num(ci_lo,         width = cw_ci),
          fmt_num(ci_hi,         width = cw_ci)
        )
      } else {
        line <- sprintf(
          paste0("%s ", .BOX_V, "%s %s %s %s"),
          lbl_fmt,
          fmt_num(est_display,   width = cw_coef),
          fmt_num(se_display,    width = cw_se),
          fmt_num(row$statistic, width = cw_stat),
          fmt_pval(row$p_value,  width = cw_pval)
        )
      }
      lines <- c(lines, line)
    }
  }

  lines <- c(lines, sep)
  lines
}


#' @keywords internal
#'
#' Format ancillary parameter rows for the bottom of the coefficient table.
#'
#' Some model types estimate auxiliary parameters alongside the linear
#' predictor (e.g., negative binomial dispersion). These are rendered below
#' the main coefficient rows, inside the same table frame, each preceded by
#' its own separator line (Stata convention).
#'
#' Ancillary rows show estimate + SE + blank z + blank p \[+ CIs if wide\].
#' They are NOT affected by `exp = TRUE` — always display raw values.
#'
#' @param ancillary_df Data frame with columns: label, estimate, std_err,
#'   ci_lower, ci_upper. CIs may be NA when wide = FALSE.
#' @param wide Logical. Whether CI columns are displayed.
#' @param total_width Integer. Total output line width (must match the
#'   coefficient table width).
#' @param level Numeric. CI width as a percentage (e.g. 95). Currently unused
#'   in this function but accepted for API consistency with
#'   `format_coef_table()`. Default 95.
#' @return Character vector of lines (separator + row pairs).
format_ancillary_rows <- function(ancillary_df, wide, total_width, level = 95) {
  cw_coef <- 10L
  cw_se   <- 10L
  cw_stat <- 10L
  cw_pval <-  9L
  cw_ci   <- 10L

  num_cols_w <- 2L + cw_coef + 1L + cw_se + 1L + cw_stat + 1L + cw_pval
  if (wide) num_cols_w <- num_cols_w + 1L + cw_ci + 1L + cw_ci

  lbl_w <- total_width - num_cols_w
  sep   <- char_rep(.BOX_H, total_width)

  blank_stat <- strrep(" ", cw_stat)
  blank_pval <- strrep(" ", cw_pval)

  lines <- character(0L)

  for (i in seq_len(nrow(ancillary_df))) {
    row     <- ancillary_df[i, ]
    lbl     <- .truncate_label(row$label, lbl_w)
    lbl_fmt <- pad_right(lbl, lbl_w)

    if (wide) {
      line <- sprintf(
        paste0("%s ", .BOX_V, "%s %s %s %s %s %s"),
        lbl_fmt,
        fmt_num(row$estimate, width = cw_coef),
        fmt_num(row$std_err,  width = cw_se),
        blank_stat,
        blank_pval,
        fmt_num(row$ci_lower, width = cw_ci),
        fmt_num(row$ci_upper, width = cw_ci)
      )
    } else {
      line <- sprintf(
        paste0("%s ", .BOX_V, "%s %s %s %s"),
        lbl_fmt,
        fmt_num(row$estimate, width = cw_coef),
        fmt_num(row$std_err,  width = cw_se),
        blank_stat,
        blank_pval
      )
    }
    lines <- c(lines, sep, line)
  }

  lines
}

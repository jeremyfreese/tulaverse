#' @rdname tula
#' @export
tula.multinom <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                          width = NULL, exp = FALSE, level = 95,
                          parallel = FALSE, ...) {
  level <- .resolve_level(level)
  wide <- .resolve_wide(wide, width)
  s    <- summary(model)
  mf   <- model.frame(model)
  n   <- nrow(mf)

  # Outcome levels: all levels, first is the base
  all_lev  <- model$lev          # e.g. c("4", "6", "8")
  base_lev <- all_lev[1L]        # e.g. "4"
  out_lev  <- all_lev[-1L]       # e.g. c("6", "8")  — one block each

  # Coefficient and SE matrices: rows = non-base outcomes, cols = predictors.
  # Ensure matrix form even when there is only one non-base outcome.
  coef_mat <- s$coefficients
  se_mat   <- s$standard.errors
  if (!is.matrix(coef_mat)) {
    coef_mat <- matrix(coef_mat, nrow = 1L,
                       dimnames = list(out_lev, names(coef_mat)))
    se_mat   <- matrix(se_mat,   nrow = 1L,
                       dimnames = list(out_lev, names(se_mat)))
  }

  # z-statistics and two-tailed p-values (multinom summary doesn't compute them)
  z_mat <- coef_mat / se_mat
  p_mat <- 2 * stats::pnorm(-abs(z_mat))

  # Confidence intervals: 3-D array [predictors x bounds x outcomes] or NULL.
  # For binary outcomes (K=2), confint() returns a plain 2-D matrix instead
  # of a 3-D array; promote it to a 3-D array with a single named slice so
  # the per-block extraction code below works uniformly.
  ci_arr <- if (wide) {
    tryCatch({
      ci_raw <- confint(model, level = level / 100)
      if (is.matrix(ci_raw)) {
        # Binary case: reshape [P x 2] -> [P x 2 x 1] named by the one non-base level
        array(ci_raw, dim = c(nrow(ci_raw), 2L, 1L),
              dimnames = list(rownames(ci_raw), colnames(ci_raw), out_lev))
      } else {
        ci_raw
      }
    }, error = function(e) NULL)
  } else NULL

  # Header metrics (same structure as glm)
  ll <- as.numeric(stats::logLik(model))

  # McFadden R² = 1 - LL_fitted / LL_null.
  # LL_null for a K-category outcome = N * log(1/K) (equal-probability baseline)
  k        <- length(all_lev)
  ll_null  <- n * log(1 / k)
  mcfadden <- 1 - ll / ll_null

  header_left <- c(
    AIC              = stats::AIC(model),
    BIC              = stats::BIC(model),
    "Log likelihood" = ll
  )
  header_right <- c(
    "Number of obs" = n,
    "McFadden R-sq" = mcfadden
  )

  # assign_vec, term_labels, data_classes, xlevels — extracted once and reused
  # across all outcome blocks (same design matrix for every outcome).
  mm          <- model.matrix(model)
  assign_vec  <- attr(mm, "assign")
  term_labels <- attr(terms(model), "term.labels")
  data_classes <- tryCatch(
    attr(terms(model), "dataClasses"),
    error = function(e) character(0)
  )
  xlevels <- if (!is.null(model$xlevels)) model$xlevels else list()

  opts <- .parse_tula_opts(ref, label)

  # Build one coef_df per non-base outcome level
  blocks <- lapply(out_lev, function(lv) {
    # ct: 4-column matrix with conventional column names, one row per predictor
    ct_lv <- cbind(
      "Estimate"   = coef_mat[lv, ],
      "Std. Error" = se_mat[lv, ],
      "z value"    = z_mat[lv, ],
      "Pr(>|z|)"   = p_mat[lv, ]
    )

    # ci: 2-column matrix for this outcome level, or NULL
    ci_lv <- if (!is.null(ci_arr)) ci_arr[ , , lv, drop = FALSE] |>
                matrix(ncol = 2L,
                       dimnames = list(rownames(ct_lv), c("2.5 %", "97.5 %")))
              else NULL

    coef_df <- build_coef_df(
      model        = model,
      ct           = ct_lv,
      ci           = ci_lv,
      wide         = wide,
      ref          = opts$ref,
      label        = opts$label,
      assign_vec   = assign_vec,
      term_labels  = term_labels,
      data_classes = data_classes,
      xlevels      = xlevels,
      model_frame  = mf
    )

    list(outcome = lv, coef_df = coef_df)
  })

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  new_tula_multinom_output(
    header_left  = header_left,
    header_right = header_right,
    blocks       = blocks,
    base_outcome = base_lev,
    stat_label   = "z",
    wide         = wide,
    width        = width,
    value_fmts   = c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3"),
    exp          = exp,
    parallel     = isTRUE(parallel),
    dep_var      = dep_var,
    level        = level
  )
}


# ---------------------------------------------------------------------------
# Internal: compute the number-zone width (nz_w) for the parallel table.
#
# Scans all formatted coefficient and SE values across all outcome blocks and
# returns the minimum number-zone width that can hold every value without
# overflow.  The result is at least 7 (wide enough for typical "-1.234" values).
#
# Used by both print.tula_multinom_output (to set total_width correctly) and
# .format_parallel_multinom_table (to format cells).
# ---------------------------------------------------------------------------
.parallel_nz_w <- function(blocks, exp = FALSE) {
  max_w   <- 7L
  base_df <- blocks[[1L]]$coef_df
  for (i in seq_len(nrow(base_df))) {
    if (isTRUE(base_df[i, "is_factor_header"])) next
    for (b in blocks) {
      r <- b$coef_df[i, ]
      if (isTRUE(r$is_ref)) next
      est    <- if (isTRUE(exp)) base::exp(r$estimate) else r$estimate
      se_val <- if (isTRUE(exp)) base::exp(r$estimate) * r$std_err else r$std_err
      coef_w <- nchar(trimws(fmt_num(est,    width = 1L)))
      # SE string "(x.xxx)" occupies nz_w+1 chars; need nz_w >= nchar(se_str) - 1
      se_w   <- nchar(trimws(fmt_num(se_val, width = 1L))) + 2L  # +2 for parens
      max_w  <- max(max_w, coef_w, se_w - 1L)
    }
  }
  max_w
}


# ---------------------------------------------------------------------------
# Internal: render a parallel (side-by-side) table for multinom output.
#
# Each non-base outcome gets one column. Within each row:
#   - Coefficient row: label | est1*  est2** est3   (stars appended, right-aligned)
#   - SE row:          blank | (se1)  (se2)  (se3)  (right-aligned)
# Factor group-header rows take one line with no numeric content.
# Intercept rows go last (as ordered by coef_df).
#
# exp = TRUE: estimates shown as exp(b), SEs as delta-method exp(b)*SE(b).
#             Reference rows show "1" instead of "0".
#
# Returns a character vector (one element per output line), NOT including
# the "Base outcome:" footer or the star legend — those are printed by the
# caller (print.tula_multinom_output).
# ---------------------------------------------------------------------------
.format_parallel_multinom_table <- function(blocks, dep_var, total_width,
                                            exp = FALSE) {
  outcomes <- vapply(blocks, `[[`, character(1L), "outcome")
  n_out    <- length(outcomes)

  # Column layout (per outcome column):
  #
  #   [  nz_w chars: number right-aligned  ][  star_w chars: stars left-aligned  ]
  #
  # nz_w is dynamic: it equals the widest formatted number across all cells
  # (minimum 7).  This prevents scientific-notation values (e.g. 9.036e-39)
  # from overflowing the number zone.  star_w is always 3.
  #
  # Alignment: the rightmost digit of the coefficient (at position nz_w) lines
  # up with the rightmost digit of the SE "(x.xxx)" on the row below.  The ")"
  # lands at position nz_w+1, leaving star_w-1 trailing spaces.

  star_w  <- 3L
  col_gap <- 2L
  pipe_w  <- 2L

  # nz_w: dynamic number-zone width (via .parallel_nz_w helper).
  nz_w    <- .parallel_nz_w(blocks, exp)
  cw_col  <- nz_w + star_w
  base_df <- blocks[[1L]]$coef_df

  # num_w: total width of the numeric section (pipe + all columns + gaps)
  num_w <- pipe_w + n_out * cw_col + (n_out - 1L) * col_gap

  # available_lbl_w: maximum label column width that fits within total_width.
  # If even this is below the minimum, the table can't be rendered.
  available_lbl_w <- total_width - num_w
  min_lbl_w <- 8L
  if (available_lbl_w < min_lbl_w) {
    stop(
      "tula(): parallel = TRUE requires more width than width = ", total_width,
      " provides for ", n_out, " outcome categories. ",
      "Try a larger width argument or use parallel = FALSE.",
      call. = FALSE
    )
  }

  # lbl_w: set from actual label content rather than total_width, so that
  # excess width (driven by the header block) appears as trailing whitespace
  # to the RIGHT of the coefficient columns, not as empty space between labels
  # and the "|" separator.  Cap at available_lbl_w (truncates labels if needed).
  all_labels      <- base_df$label
  dep_lbl_len     <- if (!is.null(dep_var)) nchar(dep_var) else 0L
  natural_lbl_w   <- max(nchar(all_labels), dep_lbl_len, 1L, na.rm = TRUE)
  lbl_w           <- min(natural_lbl_w, available_lbl_w)

  sep <- char_rep("-", total_width)

  # --- helpers ---------------------------------------------------------------

  stars_for <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    ""
  }

  # Format one cell on the coefficient line.
  # Layout: [  nz_w chars: number right-aligned  ][  star_w chars: stars left-aligned  ]
  # Rightmost digit of the number lands at position nz_w, matching the SE alignment.
  fmt_coef_cell <- function(r) {
    if (isTRUE(r$is_ref))
      return(paste0(pad_left(if (exp) "1" else "0", nz_w), strrep(" ", star_w)))
    est <- if (isTRUE(exp)) base::exp(r$estimate) else r$estimate
    st  <- stars_for(r$p_value)
    # trimws(fmt_num(..., width=1)) gives the number string without padding
    num <- trimws(fmt_num(est, width = 1L))
    paste0(pad_left(num, nz_w), pad_right(st, star_w))
  }

  # Format one cell on the SE line.
  # Layout: [  nz_w+1 chars: "(se)" right-aligned  ][  star_w-1 spaces  ]
  # The closing ")" lands at position nz_w+1, one past the last digit — aligning
  # the last digit of the SE with the last digit of the coefficient above it.
  fmt_se_cell <- function(r) {
    if (isTRUE(r$is_ref)) return(strrep(" ", cw_col))
    se_val <- if (isTRUE(exp)) base::exp(r$estimate) * r$std_err else r$std_err
    se_str <- paste0("(", trimws(fmt_num(se_val, width = 1L)), ")")
    paste0(pad_left(se_str, nz_w + 1L), strrep(" ", star_w - 1L))
  }

  # --- column header line ----------------------------------------------------
  # Outcome names are right-aligned in the number zone (nz_w), followed by
  # star_w spaces — so the last character of the name lines up with the last
  # digit of the coefficients below it.
  dep_lbl  <- if (!is.null(dep_var)) .truncate_label(dep_var, lbl_w) else ""
  col_hdrs <- vapply(outcomes,
                     function(o) paste0(pad_left(.truncate_label(o, nz_w), nz_w),
                                        strrep(" ", star_w)),
                     character(1L))
  hdr_line <- paste0(pad_right(dep_lbl, lbl_w), " |",
                     paste(col_hdrs, collapse = strrep(" ", col_gap)))

  lines <- c(sep, hdr_line, sep)

  # --- data rows -------------------------------------------------------------
  # All blocks share identical row structure; iterate using the first block.
  base_df <- blocks[[1L]]$coef_df

  for (i in seq_len(nrow(base_df))) {
    base_row <- base_df[i, ]
    lbl_fmt  <- pad_right(.truncate_label(base_row$label, lbl_w), lbl_w)

    if (isTRUE(base_row$is_factor_header)) {
      # One line only: label + pipe + blanks across all outcome columns
      lines <- c(lines, paste0(lbl_fmt, " |", strrep(" ", num_w - pipe_w)))
      next
    }

    # Coefficient line
    coef_cells <- vapply(blocks,
                         function(b) fmt_coef_cell(b$coef_df[i, ]),
                         character(1L))
    lines <- c(lines,
               paste0(lbl_fmt, " |",
                      paste(coef_cells, collapse = strrep(" ", col_gap))))

    # SE line (blank label area)
    se_cells <- vapply(blocks,
                       function(b) fmt_se_cell(b$coef_df[i, ]),
                       character(1L))
    lines <- c(lines,
               paste0(pad_right("", lbl_w), " |",
                      paste(se_cells, collapse = strrep(" ", col_gap))))
  }

  lines <- c(lines, sep)
  lines
}

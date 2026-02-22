# tula — Developer Notes for Claude

This file is read automatically by Claude Code at the start of every session.
It records architectural decisions and how-to patterns for this package.

---

## What tula does

`tula()` is an S3 generic that produces Stata-style console output for two
distinct input types:

- **Regression models** (`lm`, `glm`, and future model types): coefficient
  table with header block (fit statistics).
- **Data frames / vectors** (the "summarize path"): Stata `-summarize`-style
  descriptive statistics table.

---

## File map

| File | Purpose |
|------|---------|
| `R/tula.R` | Generic, `tula.default`, `tula.data.frame`, `new_tula_output()`, `print.tula_output()` |
| `R/tula_lm.R` | `tula.lm()` method |
| `R/tula_glm.R` | `tula.glm()` method |
| `R/coef_table.R` | `build_coef_df()`, `format_coef_table()`, `.truncate_label()`, row constructors |
| `R/header.R` | `format_header()`, `compute_total_width()` |
| `R/format_helpers.R` | `fmt_num()`, `fmt_pval()`, `fmt_header_val()`, `pad_left/right()`, `char_rep()` |
| `R/tula_summarize.R` | `.tula_summarize()`, `print.tula_summary()`, `format_summary_table()`, `.fmt_sum()`, `.fmt_obs()` |

---

## Regression output architecture

### The canonical pipeline

```
tula.XYZ(model, ...)
  │
  ├─ extract: ct (coef matrix), ci (CI matrix or NULL)
  ├─ compute: header_left, header_right (named numeric vectors)
  ├─ determine: stat_label ("t", "z", etc.), family_label (or NULL)
  │
  ├─► build_coef_df(model, ct, ci, wide, ref, label, ...)  [coef_table.R]
  │     └─ returns canonical coef_df (10-column data frame)
  │
  └─► new_tula_output(...)  [tula.R]
        └─ returns tula_output S3 object → auto-printed by print.tula_output()
```

`build_coef_df()` and `format_coef_table()` are **fully model-agnostic** —
they work on the canonical `coef_df` structure and know nothing about model
class. The rendering pipeline (`print.tula_output`, `format_header`,
`format_coef_table`) never needs to change for new model types.

### The coef_df structure

Ten columns produced by `build_coef_df()`:

```
label            chr   Display label; factor levels indented with "  "
is_factor_header lgl   TRUE for factor group header rows (no numeric values)
is_intercept     lgl   TRUE for intercept-type rows (placed last)
is_ref           lgl   TRUE for reference-level rows (shown when ref=TRUE)
estimate         dbl   Coefficient
std_err          dbl   Standard error
statistic        dbl   t or z statistic
p_value          dbl   p-value
ci_lower         dbl   Lower CI bound (NA if wide=FALSE)
ci_upper         dbl   Upper CI bound (NA if wide=FALSE)
```

### The tula_output object

```r
list(
  model_type   = "lm",          # character; used for nothing except record-keeping
  header_left  = c(...),         # named numeric vector — left header block
  header_right = c(...),         # named numeric vector — right header block
  coef_df      = <data.frame>,   # from build_coef_df()
  stat_label   = "t",            # "t", "z", or whatever label suits the model
  wide         = FALSE,          # logical; whether CI columns are shown
  family_label = NULL,           # optional string, e.g. "Family: binomial / Link: logit"
  width        = NULL            # integer, Inf, or NULL (→ getOption("width"))
)
```

---

## How to add a new model type

### Step 1 — Create `R/tula_MODELTYPE.R`

The method needs to:
1. Run `summary(model)` (or equivalent).
2. Extract `ct`: a matrix with columns (Estimate, Std. Error, statistic, p-value).
   Column names don't matter — only column positions [1:4] are used.
3. Compute `ci`: a two-column matrix of CI bounds (rownames matching `ct`), or NULL.
4. Assemble `header_left` and `header_right` as named numeric vectors.
5. Call `build_coef_df()` and `new_tula_output()`.

Minimal template:

```r
#' @rdname tula
#' @export
tula.MODELTYPE <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                           width = NULL, ...) {
  s     <- summary(model)

  # --- ct: coefficient matrix (Estimate, SE, statistic, p-value) -----------
  ct <- s$coefficients[ , 1:4, drop = FALSE]

  # --- ci: confidence intervals or NULL ------------------------------------
  ci <- if (wide) confint(model) else NULL

  # --- Header metrics -------------------------------------------------------
  header_left  <- c(AIC = AIC(model), BIC = BIC(model))
  header_right <- c("Number of obs" = nobs(model))

  # --- Stat label -----------------------------------------------------------
  # Inspect the column name of ct to decide "t" vs "z" vs something else.
  stat_label <- if (grepl("^z", colnames(ct)[3L], ignore.case = TRUE)) "z" else "t"

  # --- Coef data frame ------------------------------------------------------
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                            ref = opts$ref, label = opts$label)

  # --- Output ---------------------------------------------------------------
  new_tula_output(
    model_type   = "MODELTYPE",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = stat_label,
    wide         = wide,
    family_label = NULL,   # or a descriptive string
    width        = width
  )
}
```

### Step 2 — Register in `NAMESPACE`

Add:
```
S3method(tula,MODELTYPE)
```

Or re-run `devtools::document()` if using roxygen2.

### Step 3 — Add a test section to an appropriate `.qmd` file

---

## build_coef_df() — optional parameters for non-standard models

For models where `model.matrix()`, `terms()`, or `model.frame()` are
unavailable or return non-standard results, `build_coef_df()` accepts
pre-parsed values as explicit arguments:

| Argument | What it is | Default behaviour |
|----------|-----------|-------------------|
| `assign_vec` | Integer vector: `attr(model.matrix(model), "assign")`. 0 = intercept, k = k-th term. | Extracted via `model.matrix()` |
| `term_labels` | Character vector: `attr(terms(model), "term.labels")` | Extracted via `terms()` |
| `data_classes` | Named character vector: `attr(terms(model), "dataClasses")` | Extracted via `terms()` |
| `xlevels` | Named list of factor level vectors: `model$xlevels` | Taken from `model$xlevels` |
| `model_frame` | Data frame: `model.frame(model)` | Extracted via `model.frame()` |
| `orig_data` | Raw input data frame, for haven label recovery | Evaluated from `model$call$data` |

**Key behaviour**: intercept detection uses `assign_vec == 0`, not name
matching. Models with no intercept (e.g. Cox) simply produce no intercept
rows. Models with a non-standard intercept name work correctly.

### Example: Cox proportional hazards

```r
tula.coxph <- function(model, wide = FALSE, ref = FALSE, label = TRUE,
                       width = NULL, ...) {
  s  <- summary(model)

  # coxph summary matrix columns: coef, exp(coef), se(coef), z, Pr(>|z|)
  # We need columns: Estimate, SE, statistic, p-value
  ct_raw <- s$coefficients
  ct <- ct_raw[ , c("coef", "se(coef)", "z", "Pr(>|z|)"), drop = FALSE]
  colnames(ct) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

  ci <- if (wide) s$conf.int[ , c("lower .95", "upper .95"), drop = FALSE] else NULL

  # Cox has no intercept — assign_vec will have no zeros, so no intercept
  # row is emitted. Supply assign_vec explicitly so model.matrix() isn't
  # called with an implicit intercept column added.
  mm         <- model.matrix(model)
  assign_vec <- attr(mm, "assign")   # all entries will be >= 1

  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                            ref = opts$ref, label = opts$label,
                            assign_vec = assign_vec)

  header_left  <- c(AIC = AIC(model), BIC = BIC(model))
  header_right <- c(
    "Number of obs"   = s$n,
    "Number of events" = s$nevent,
    "Concordance"     = s$concordance[["C"]]
  )

  new_tula_output(
    model_type   = "coxph",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = "z",
    wide         = wide,
    family_label = "Cox proportional hazards",
    width        = width
  )
}
```

---

## Summarize path architecture

Called when `tula()` receives a data frame, tibble, or atomic vector.

```
tula.data.frame / tula.default
  └─► .tula_summarize(df, width, sep, mad, median, digits)
        ├─ .build_summary_rows() per column → stacked data frame (rows)
        └─ returns tula_summary S3 object → print.tula_summary()
              └─► format_summary_table(rows, opts, total_width)
```

### factor_level rows use sentinel columns

Since `sd_val` and `max_val` are meaningless for factor levels, they store:
- `sd_val` = total non-missing N for the variable (used to build `"(count/total)"`)
- `max_val` = level count
- `mean_val` = proportion
- `n_obs` = NA (Obs column is blank for level rows; N shown on header row)

---

## Key options and their defaults

| Option | Default | Path | Effect |
|--------|---------|------|--------|
| `wide` | FALSE | regression | Show 95% CI columns |
| `ref` | FALSE | regression | Show reference level rows |
| `label` | TRUE | regression | Use haven value labels |
| `width` | NULL (→ `getOption("width")`) | both | Total output width |
| `sep` | 5L | summarize | Variables between separator lines |
| `mad` | FALSE | summarize | Show MAD instead of SD |
| `median` | FALSE | summarize | Show median/IQR instead of mean/SD |
| `digits` | 7L | summarize | Significant digits (avoids sci notation to ~10M) |

The summarize path also caps decimal places at 3 for `|x| > 0.1` via `.fmt_sum()`.

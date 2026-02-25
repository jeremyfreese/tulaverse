# tula — Developer Notes for Claude

This file is read automatically by Claude Code at the start of every session.
It records architectural decisions and how-to patterns for this package.

---

## Session rules

- **Never commit to git unless the user explicitly instructs you to.** Make
  changes, render if needed, and stop — let the user decide when and whether
  to commit.
- **Rendering `visual_test.qmd` to verify changes work is fine** and
  encouraged; committing the result is not.
- **Stop after the requested task.** Don't chain to unrequested improvements
  — finish what was asked, summarize what changed, and wait.

---

## What tula does

`tula()` is an S3 generic that produces Stata-style console output for two
distinct input types:

- **Regression models** (`lm`, `glm`, `negbin`, `multinom`, `polr`, `clm`,
  `rq`, `rqs`, `coxph`, and future model types): coefficient table with header
  block (fit statistics).
- **Data frames / vectors** (the "summarize path"): Stata `-summarize`-style
  descriptive statistics table.

`tulatab()` is a standalone exported function (not dispatched through `tula()`)
that produces Stata-style one-way frequency tables (the "tabulate path").

---

## File map

| File | Purpose |
|------|---------|
| `R/tula.R` | Generic, `tula.default`, `tula.data.frame`, `new_tula_output()`, `print.tula_output()`, `new_tula_multinom_output()`, `print.tula_multinom_output()` |
| `R/tula_lm.R` | `tula.lm()` method |
| `R/tula_glm.R` | `tula.glm()` method |
| `R/tula_multinom.R` | `tula.multinom()` method |
| `R/tula_negbin.R` | `tula.negbin()` method |
| `R/tula_polr.R` | `tula.polr()` method (ordered regression via `MASS::polr`) |
| `R/tula_coxph.R` | `tula.coxph()` method (Cox proportional hazards via `survival::coxph`) |
| `R/tula_clm.R` | `tula.clm()` method (ordered regression via `ordinal::clm`) |
| `R/tula_rq.R` | `tula.rq()`, `tula.rqs()`, `new_tula_rqs_output()`, `print.tula_rqs_output()` |
| `R/tulaplot.R` | `tulaplot()`, `theme_tula()`, `scale_color_tula()`, `scale_fill_tula()`, `.tula_palette` |
| `R/coef_table.R` | `build_coef_df()`, `format_coef_table()`, `format_ancillary_rows()`, `.truncate_label()`, `.build_cutpoint_rows()`, row constructors |
| `R/header.R` | `format_header()`, `compute_total_width()` |
| `R/format_helpers.R` | `fmt_num()`, `fmt_pval()`, `fmt_header_val()`, `pad_left/right()`, `char_rep()` |
| `R/tula_summarize.R` | `.tula_summarize()`, `print.tula_summary()`, `format_summary_table()`, `.fmt_sum()`, `.fmt_obs()` |
| `R/robust.R` | `.resolve_robust_vcov()`, `.recompute_ct_robust()`, `.robust_ci()` |
| `R/tulatab.R` | `tulatab()`, `new_tula_tab()`, `print.tula_tab()`, `.extract_var_name()` |
| `R/tab_helpers.R` | `.build_tab_df()`, `.detect_var_type()`, `.tab_factor()`, `.tab_character()`, `.tab_numeric()`, `.tab_haven()`, `.haven_display_value()`, `.fmt_freq()`, `.fmt_pct()`, `.format_tab_table()`, `.truncate_tab_label()` |

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
  │     └─ returns canonical coef_df (11-column data frame)
  │
  └─► new_tula_output(...)  [tula.R]
        └─ returns tula_output S3 object → auto-printed by print.tula_output()
```

`build_coef_df()` and `format_coef_table()` are **fully model-agnostic** —
they work on the canonical `coef_df` structure and know nothing about model
class. The rendering pipeline (`print.tula_output`, `format_header`,
`format_coef_table`) never needs to change for new model types.

### The coef_df structure

Eleven columns produced by `build_coef_df()` (or appended via
`.build_cutpoint_rows()` for ordered models):

```
label            chr   Display label; factor levels indented with "  "
is_factor_header lgl   TRUE for factor group header rows (no numeric values)
is_intercept     lgl   TRUE for intercept-type rows (placed last)
is_ref           lgl   TRUE for reference-level rows (shown when ref=TRUE)
is_cutpoint      lgl   TRUE for cutpoint/threshold rows (ordered regression)
estimate         dbl   Coefficient
std_err          dbl   Standard error
statistic        dbl   t or z statistic
p_value          dbl   p-value
ci_lower         dbl   Lower CI bound (NA if wide=FALSE)
ci_upper         dbl   Upper CI bound (NA if wide=FALSE)
```

Cutpoint rows (from ordered regression) have `is_cutpoint = TRUE` and show
estimate + SE + blank statistic + blank p-value. They are rendered after the
main coefficient block, preceded by a separator line (Stata convention).

### The tula_output object

```r
list(
  model_type     = "lm",          # character; used for nothing except record-keeping
  header_left    = c(...),         # named numeric vector — left header block
  header_right   = c(...),         # named numeric vector — right header block
  coef_df        = <data.frame>,   # from build_coef_df()
  stat_label     = "t",            # "t", "z", or whatever label suits the model
  wide           = FALSE,          # logical; whether CI columns are shown
  family_label   = NULL,           # optional string, e.g. "Family: binomial / Link: logit"
  width          = NULL,           # integer, Inf, or NULL (→ getOption("width"))
  value_fmts     = c(AIC = "f3"), # named character: header value format overrides
  exp            = FALSE,          # logical; whether exponentiated coefficients are displayed
  dep_var        = NULL,           # character or NULL; dependent variable name shown in column header
  exp_label      = NULL,           # character or NULL; replaces "exp(b)" header (e.g. "IRR")
  ancillary_df   = NULL,           # data.frame or NULL; ancillary parameter rows (e.g. negbin alpha)
  level          = 95,             # numeric; CI width as percentage (e.g. 95, 90, 99)
  outcome_levels = NULL,           # character vector or NULL; ordered regression level names for footer
  se_label       = NULL            # character or NULL; overrides "Std. Err." header (e.g. "Robust SE")
)
```

### Header value formatting (`value_fmts`)

`value_fmts` is a named character vector that controls how header values
are formatted. Names correspond to header label names (e.g., `"AIC"`,
`"Log likelihood"`). Two formats are supported:

- `"g4"` (default): 4 significant digits, general format.
- `"f3"`: fixed notation with up to 3 decimal places — used for AIC, BIC,
  and log-likelihood, which can have many digits before the decimal point.

Each `tula.*()` method passes its own `value_fmts` to the constructor.
For example, `tula.lm()` uses `c(AIC = "f3", BIC = "f3")` while `tula.glm()`
uses `c(AIC = "f3", BIC = "f3", "Log likelihood" = "f3")`.

---

## Exponentiated coefficients (`exp = TRUE`)

When `exp = TRUE` is passed to `tula()`, the formatting layer transforms
the display without mutating `coef_df`:

- **Estimate column**: shows `exp(β)` instead of `β`; header becomes `"exp(b)"`.
- **SE column**: shows the delta-method SE = `exp(β) × SE(β)`; header becomes `"DMSE"` unless robust SEs are in use, in which case `"Robust SE"` takes precedence.
- **Test statistic and p-value**: unchanged (invariant under exponentiation).
- **Reference rows**: show `1` instead of `0` (since `exp(0) = 1`).
- **CIs**: when `exp = TRUE` and `wide = TRUE`, CI bounds are exponentiated
  (`exp(ci_lower)`, `exp(ci_upper)`). The resulting CIs are asymmetric, which
  is standard for hazard ratios, odds ratios, IRRs, etc. Cutpoint rows
  (ordered regression) are never exponentiated.

The transformation happens in `format_coef_table()` (in `coef_table.R`),
not in `build_coef_df()`. This keeps the data pipeline clean — `coef_df`
always stores raw (unexponentiated) values.

### `exp_label`

Optional character scalar on `tula_output`. When `exp = TRUE` and `exp_label`
is non-NULL, it replaces the default `"exp(b)"` column header. Current usage:

- Negative binomial: `"IRR"` (Incidence Rate Ratio)
- Cox PH: `"Haz. Ratio"` (Hazard Ratio)
- Future: `"Odds Ratio"` for logistic, `"HR"` for other survival models

---

## Ancillary parameters (`ancillary_df`)

Some model types estimate auxiliary parameters alongside the linear predictor
(e.g., negative binomial dispersion). These are rendered at the bottom of the
coefficient table, above the closing separator, each preceded by its own
separator line (Stata convention).

### The ancillary_df structure

Five columns:

```
label      chr   Display label (e.g. "/lnalpha", "alpha")
estimate   dbl   Point estimate
std_err    dbl   Standard error
ci_lower   dbl   Lower CI bound (NA if wide=FALSE)
ci_upper   dbl   Upper CI bound (NA if wide=FALSE)
```

Ancillary rows show estimate + SE + blank z + blank p [+ CIs if wide].
They are NOT affected by `exp = TRUE` — always display raw values.

### Rendering

`format_ancillary_rows()` in `coef_table.R` is called by `print.tula_output()`
when `ancillary_df` is non-NULL. Lines are spliced before the final separator
of the coefficient table. Each ancillary row is preceded by its own separator.

### Adding ancillary parameters to a new model type

Pass `ancillary_df = <data.frame>` to `new_tula_output()`. The rendering
pipeline handles everything automatically. No changes to `format_coef_table()`
or `print.tula_output()` are needed.

---

## Robust standard errors (`robust`, `vcov`, `cluster`)

### Overview

All regression methods accept three new parameters for sandwich-based robust
standard errors. These require the `sandwich` package (in Suggests, not Imports).

| Parameter | Type | Effect |
|-----------|------|--------|
| `robust = TRUE` | logical | HC3 heteroskedasticity-robust SEs (default type) |
| `vcov = "HC4"` | character | HC type override (any type accepted by `sandwich::vcovHC()`) |
| `vcov = matrix` | matrix | Pre-computed vcov matrix used directly |
| `cluster = "var"` | character | Cluster-robust SEs; variable name in model frame or data |

`cluster` implies `robust = TRUE` regardless of the `robust` argument.

### Three internal helpers (`R/robust.R`)

```r
.resolve_robust_vcov(model, robust, vcov_arg, cluster_arg)
  → NULL if no robust adjustment needed
  → list(vcov_mat, se_label, cluster_n) otherwise

.recompute_ct_robust(ct, vcov_mat, stat_label, df = Inf)
  → updated ct matrix (columns 2, 3, 4 replaced)

.robust_ci(ct, level, stat_label, df = Inf)
  → 2-column CI matrix with same rownames as ct
```

### Pipeline in each method

After extracting `ct` (and determining `stat_label`), before calling
`build_coef_df()`:

```r
robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
if (!is.null(robust_info)) {
  df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
  ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, stat_label, df = df_resid)
  ci <- if (wide) .robust_ci(ct, level, stat_label, df = df_resid) else NULL
} else {
  ci <- if (wide) confint(model, level = level / 100) else NULL
}
if (!is.null(robust_info$cluster_n)) {
  header_right <- c(header_right, "Num. clusters" = robust_info$cluster_n)
}
```

Then pass `se_label = if (!is.null(robust_info)) robust_info$se_label else NULL`
to `new_tula_output()`.

### SE column header

`se_label` flows through the object to `format_coef_table()`. When non-NULL
and `exp = FALSE`, it replaces `"Std. Err."` in the column header:

```
se_hdr <- if (!is.null(se_label)) se_label else if (exp) "DMSE" else "Std. Err."
```

### Ordered models (polr, clm) — special handling

These models have both predictor `ct` and cutpoint `ct_zeta`. The robust vcov
covers all parameters. The method:
1. Applies `.recompute_ct_robust()` to predictor `ct`.
2. Updates `ct_zeta[, "Std. Error"]` directly from `sqrt(diag(v_use)[zeta_names])`.
3. Uses `v_use` (the robust or model vcov) for both predictor and cutpoint CIs.

### Multinom — per-outcome submatrix extraction

The `sandwich` vcov for multinom covers all outcome-level coefficients. Names
follow the convention `"outcome:predictor"` (e.g., `"6:(Intercept)"`). The
`tula.multinom()` method extracts per-outcome submatrices using
`paste0(lv, ":", pred_names)`. If the naming doesn't match (unexpected vcov
structure), a warning is emitted and model SEs are used for that outcome.

### Quantile regression (rqs) — per-block application

For `tula.rqs()`, robust SEs are applied per block using a lightweight
single-tau `rq` object constructed per quantile. A representative
`robust_info` is also computed before the loop for `se_label` and `cluster_n`.

### Graceful error for unsupported models

If `sandwich::vcovHC()` or `vcovCL()` does not support the model class,
`.resolve_robust_vcov()` stops with an informative message:
```
sandwich::vcovHC() does not support models of class 'clm'.
Robust SEs are not available for this model type.
```

### Ancillary parameters and `exp = TRUE`

Robust SEs do NOT affect ancillary parameter rows (e.g., negbin `/lnalpha`
and `alpha`). These are always computed from the model's own dispersion
parameter SEs. The `exp = TRUE` transformation is independent of robust SEs —
both can be used simultaneously (`exp = TRUE, robust = TRUE`).

---

## Negative binomial (`MASS::glm.nb`)

### Key structural features

- Class: `c("negbin", "glm", "lm")`. S3 dispatch finds `tula.negbin()` before
  `tula.glm()`.
- `summary()` gives standard glm-style coefficient table with z-values.
- `model$theta`: R's NB dispersion parameter. Stata's `alpha = 1/theta`.
- `model$SE.theta`: standard error of theta.
- Ancillary parameters:
  - `/lnalpha = -log(theta)`, SE via delta method: `SE(theta) / theta`
  - `alpha = 1/theta = exp(lnalpha)`, SE via delta method: `alpha * SE(lnalpha)`
  - CI for alpha = `exp(CI for lnalpha)` (asymmetric, correct)
- McFadden R-sq: requires fitting a null intercept-only NB model via
  `update(model, . ~ 1, trace = FALSE)` because theta differs between null
  and fitted models.
- `exp = TRUE` shows column header `"IRR"` via `exp_label`.
- `exp` does NOT affect the ancillary section.

---

## How to add a new model type

### Step 1 — Create `R/tula_MODELTYPE.R`

The method needs to:
1. Accept `level = 95` and resolve it via `.resolve_level(level)`.
2. Resolve `wide` via `.resolve_wide(wide, width)`.
4. Run `summary(model)` (or equivalent).
5. Extract `ct`: a matrix with columns (Estimate, Std. Error, statistic, p-value).
   Column names don't matter — only column positions [1:4] are used.
6. Compute `ci`: a two-column matrix of CI bounds (rownames matching `ct`), or NULL.
   Pass `level / 100` to `confint()` if it accepts a `level` argument.
7. Assemble `header_left` and `header_right` as named numeric vectors.
8. Extract `dep_var` via `deparse(formula(model)[[2L]])`.
9. Call `build_coef_df()` and `new_tula_output()`, passing `exp`, `dep_var`, `level`.

Minimal template:

```r
#' @rdname tula
#' @export
tula.MODELTYPE <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                           width = NULL, exp = FALSE, level = 95,
                           robust = FALSE, vcov = NULL, cluster = NULL, ...) {
  level <- .resolve_level(level)
  wide  <- .resolve_wide(wide, width)

  s     <- summary(model)

  # --- ct: coefficient matrix (Estimate, SE, statistic, p-value) -----------
  ct <- s$coefficients[ , 1:4, drop = FALSE]

  # --- stat label (determine before robust adjustment) ----------------------
  stat_label <- if (grepl("^z", colnames(ct)[3L], ignore.case = TRUE)) "z" else "t"

  # --- robust SE (optional; requires sandwich in Suggests) ------------------
  # sandwich::vcovHC() and vcovCL() support this model type: YES/NO
  # (Update this comment when implementing; if NO, robust args are silently
  # ignored because .resolve_robust_vcov() will error with a clear message.)
  robust_info <- .resolve_robust_vcov(model, robust, vcov, cluster)
  if (!is.null(robust_info)) {
    df_resid <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    ct <- .recompute_ct_robust(ct, robust_info$vcov_mat, stat_label, df = df_resid)
    ci <- if (wide) .robust_ci(ct, level, stat_label, df = df_resid) else NULL
  } else {
    # --- ci: confidence intervals or NULL ------------------------------------
    ci <- if (wide) confint(model, level = level / 100) else NULL
  }

  # --- Header metrics -------------------------------------------------------
  header_left  <- c(AIC = AIC(model), BIC = BIC(model))
  header_right <- c("Number of obs" = nobs(model))
  if (!is.null(robust_info$cluster_n)) {
    header_right <- c(header_right, "Num. clusters" = robust_info$cluster_n)
  }

  # --- Coef data frame ------------------------------------------------------
  opts    <- .parse_tula_opts(ref, label)
  coef_df <- build_coef_df(model, ct, ci, wide,
                            ref = opts$ref, label = opts$label)

  dep_var <- tryCatch(deparse(formula(model)[[2L]]), error = function(e) NULL)

  # --- Output ---------------------------------------------------------------
  new_tula_output(
    model_type   = "MODELTYPE",
    header_left  = header_left,
    header_right = header_right,
    coef_df      = coef_df,
    stat_label   = stat_label,
    wide         = wide,
    family_label = NULL,   # or a descriptive string
    width        = width,
    exp          = exp,
    dep_var      = dep_var,
    level        = level,
    se_label     = if (!is.null(robust_info)) robust_info$se_label else NULL
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

### Example: Cox proportional hazards (now implemented)

See `R/tula_coxph.R` for the full implementation and the
"Cox proportional hazards" section above for details.

---

## Multinomial logit (`nnet::multinom`)

### Key structural differences from lm/glm

- `coef(m)` is a `(K−1) × P` matrix — rows = non-base outcomes, cols = predictors.
- `summary(m)` provides `$coefficients` and `$standard.errors` separately; z-stats
  and p-values must be computed manually: `z = coef/SE`, `p = 2*pnorm(-|z|)`.
- `confint(m)` returns a **3-D array** `[predictors × bounds × outcome-levels]`;
  slice per outcome with `ci_arr[ , , lv, drop=FALSE]`.
- `nobs()` has no registered method; use `nrow(model.frame(m))`.
- `model.matrix()`, `terms()`, and `m$xlevels` all work normally.

### Output object: `tula_multinom_output`

Distinct from `tula_output`. Fields:

```r
list(
  header_left  = c(...),    # AIC, BIC, Log likelihood
  header_right = c(...),    # Number of obs, McFadden R-sq
  blocks       = list(      # one list per non-base outcome
    list(outcome = "6", coef_df = <data.frame>),
    list(outcome = "8", coef_df = <data.frame>)
  ),
  base_outcome = "4",       # label of reference outcome
  stat_label   = "z",
  wide         = FALSE,
  width        = NULL,
  value_fmts   = c(AIC = "f3"),  # named character: header value format overrides
  exp          = FALSE            # logical; exponentiated coefficients
)
```

### Print layout

```
<shared header block>
--------------------------------------------------
<outcome label>
<coefficient table>   # one table per non-base outcome
--------------------------------------------------
<outcome label>
<coefficient table>
--------------------------------------------------
Base outcome: <base_outcome>
```

### McFadden R²

Null log-likelihood = `N * log(1/K)` where K = number of outcome levels.
`McFadden = 1 - LL_fitted / LL_null`.

### `build_coef_df()` is called once per outcome block

The same `assign_vec`, `term_labels`, `data_classes`, `xlevels`, and
`model_frame` are extracted once and passed explicitly to each call,
avoiding redundant computation.

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
| `wide` | NULL | regression | Show CI columns (NULL = auto based on width ≥ 80) |
| `ref` | FALSE | regression | Show reference level rows |
| `label` | TRUE | regression | Use haven value labels |
| `exp` | FALSE | regression | Exponentiate coefficients; show exp(b) and DMSE; CIs also exponentiated |
| `level` | 95 | regression | CI width as percentage (e.g. 90, 95, 99); also accepts 0–1 scale |
| `parallel` | FALSE | multinom/rqs | Side-by-side outcome columns instead of stacked blocks |
| `robust` | FALSE | regression | HC3 heteroskedasticity-robust SEs; requires `sandwich` |
| `vcov` | NULL | regression | HC type override (character, e.g. `"HC4"`) or pre-computed vcov matrix |
| `cluster` | NULL | regression | Cluster-robust SEs; character variable name; implies `robust = TRUE` |
| `width` | NULL (→ `getOption("width")`) | both | Total output width |
| `sep` | 5L | summarize | Variables between separator lines |
| `mad` | FALSE | summarize | Show MAD instead of SD |
| `median` | FALSE | summarize | Show median/IQR instead of mean/SD |
| `digits` | 7L | summarize | Significant digits (avoids sci notation to ~10M) |

The summarize path also caps decimal places at 3 for `|x| > 0.1` via `.fmt_sum()`.

### `level` parameter

The `level` parameter controls CI width. Accepted on the `tula()` generic and
all regression methods. Resolved via `.resolve_level()`:
- Values < 1 are multiplied by 100 (so `0.95` → `95`).
- Must be between 50 and 99.9 after normalisation.
- The CI column header adjusts automatically (e.g. `"[90% CI]"` instead of
  `"[95% CI]"`). This is handled in `format_coef_table()`.
- Passed to `confint()` as `level / 100` where applicable.

### `dep_var` — dependent variable name

All regression methods extract the dependent variable name via
`deparse(formula(model)[[2L]])` and pass it to `new_tula_output()`. The
`print.tula_output()` method displays it in the label-column area of the
column-header line (same pattern as multinomial outcome labels).

---

## Ordered regression (`MASS::polr` and `ordinal::clm`)

### Key structural features

- **Two methods, same architecture**: `tula.polr()` and `tula.clm()` share
  nearly identical structure. Both separate predictor coefficients from
  cutpoints (thresholds between ordered categories).
- **Cutpoints**: Ordered models estimate threshold parameters (e.g. `"1|2"`,
  `"2|3"`) in addition to predictor coefficients. These are rendered as
  cutpoint rows (via `.build_cutpoint_rows()`) appended to `coef_df`.
  Cutpoint rows have `is_cutpoint = TRUE`, show estimate + SE, and leave
  statistic + p-value blank (Stata convention).
- **polr quirk**: `summary(polr)$coefficients` has only 3 columns (Value,
  Std. Error, t value) — no p-value. The method computes p-values manually:
  `2 * pnorm(-abs(t))`. The "t value" is really asymptotically z-distributed,
  so `stat_label = "z"`.
- **clm**: `summary(clm)$coefficients` has the standard 4 columns (Estimate,
  Std. Error, z value, Pr(>|z|)) for all parameters (thresholds + predictors).
- **`model.matrix()` workarounds**: `polr`'s model matrix includes an
  intercept column (assign = 0) that polr strips internally. Fix: filter
  `assign_vec` to remove zeros so positions match predictor names.
  `clm`'s `model.matrix.clm()` does NOT set the `"assign"` attribute;
  bypass by calling `model.matrix(terms(model), data = mf)` instead.
- **McFadden R²**: computed from marginal proportions (`ll_null = sum(n_k * log(n_k/N))`)
  rather than fitting a separate null model.
- **CIs**: Wald-type CIs via `vcov()`, manually computed with z-critical value.
  Covers both predictors and cutpoints.
- **Footer**: `outcome_levels` is passed to `new_tula_output()`. The
  `print.tula_output()` method renders a "Lowest level: X, Highest: Y" footer
  when `outcome_levels` has ≥ 2 elements.
- **family_label**: `"Ordered regression / Link: <method>"` (polr uses
  `model$method`, clm uses `model$link`).

---

## Cox proportional hazards (`survival::coxph`)

### Key structural features

- **S3 class**: `"coxph"`. `survival` is in Suggests, not Imports.
- **Default `exp = TRUE`**: unlike all other methods, Cox defaults to showing
  hazard ratios. Users pass `exp = FALSE` for raw log-hazard coefficients.
- **`exp_label`**: `"Haz. Ratio"` when `exp = TRUE`.
- **No intercept**: Cox models have no intercept term. `model.matrix(coxph)`
  returns a matrix where all `assign` entries are >= 1, so `build_coef_df()`
  produces no intercept row automatically.
- **summary**: `summary(coxph)$coefficients` has 5 columns: `coef`,
  `exp(coef)`, `se(coef)`, `z`, `Pr(>|z|)`. Extract columns 1, 3, 4, 5
  for the canonical 4-column `ct` matrix.
- **CIs**: Wald-type via `confint.default()` on the log-hazard scale.
  `format_coef_table()` exponentiates these when `exp = TRUE`.
- **Header left**: No. of subjects, No. of failures, Time at risk,
  Log likelihood.
- **Header right**: Number of obs, AIC, Concordance.
- **Time at risk**: `sum(model$y[, 1L])` — total survival time across all
  subjects. Falls back to `model.frame()` if `model$y` is NULL.
- **Concordance**: `summary(model)$concordance[["C"]]` (Harrell's C).
- **Log likelihood**: `model$loglik[2L]` (fitted model; `[1L]` = null).
- **family_label**: `"Cox regression / Ties: <method>"` where method is
  `model$method` (efron, breslow, or exact).
- **stat_label**: `"z"`.
- **value_fmts**: `c("Log likelihood" = "f3", AIC = "f3")`.

---

## Quantile regression (`quantreg::rq` and `rqs`)

### Key structural features

- **S3 classes**: `quantreg::rq()` returns class `"rq"` for a single quantile
  and class `"rqs"` for multiple quantiles (when `tau` is a vector).
- **`quantreg` is in Suggests**, not Imports. Users must install it themselves.
- **summary**: `summary(rq_model, se = "nid")` gives the standard 4-column
  coefficient table (Value, Std. Error, t value, Pr(>|t|)) with `stat_label = "t"`.
- **CIs**: Wald-type CIs computed manually from SE and z-critical value
  (not via `confint.rq()`, which uses rank-based inversion).
- **`model.matrix()` workaround**: `model.matrix(rq_object)` fails because
  it can't find the data. Bypassed with
  `model.matrix(terms(model), data = model.frame(model))`.

### Single quantile: `tula.rq()`

Standard `tula_output` object. Header shows:
- Left: `Raw sum of dev`, `Min sum of dev`, `K-B Pseudo R2`
  (Koenker-Bassett Pseudo R² = `1 - min_sum_dev / raw_sum_dev`)
- Right: `Number of obs`
- `family_label`: `.rq_family_label(tau)` — e.g. `"Median regression"` or
  `"90th Quantile regression"`.

### Multiple quantiles: `tula.rqs()`

Uses a **distinct output object**: `tula_rqs_output` (analogous to
`tula_multinom_output`). Supports two display modes:

- **Stacked** (default): one coefficient block per quantile, separated by
  dashed lines. Short labels via `.rq_short_label()` appear in the
  label-column area of each block's header row.
- **Parallel** (`parallel = TRUE`): all quantiles as side-by-side columns
  with significance stars. Uses the same `.format_parallel_multinom_table()`
  renderer as multinomial output.

### `tula_rqs_output` object

```r
list(
  header_left  = numeric(0),     # empty (no shared fit stats)
  header_right = c("Number of obs" = N),
  blocks       = list(           # one per quantile
    list(outcome = "25th Q", coef_df = <data.frame>, tau = 0.25),
    list(outcome = "Median", coef_df = <data.frame>, tau = 0.5),
    ...
  ),
  stat_label   = "t",
  wide         = FALSE,
  width        = NULL,
  value_fmts   = character(0L),
  exp          = FALSE,
  parallel     = FALSE,
  dep_var      = "y",
  level        = 95,
  family_label = "Quantile regression (3 quantiles)"
)
```

### Two-tier labeling system

- **`.rq_family_label(tau)`**: Full label for single-quantile `family_label`
  line. E.g. `"Median regression"`, `"90th Quantile regression"`.
- **`.rq_short_label(tau)`**: Compact label for stacked/parallel block headers.
  E.g. `"Median"`, `"90th Q"`, `"25th Q"`. Avoids truncation of long labels
  in narrow output.

### Per-quantile summary in `tula.rqs()`

Each quantile block is summarized by constructing a lightweight single-tau
`rq` object from the multi-tau model's coefficient and residual columns,
then calling `summary(..., se = "nid")` on it. This avoids re-fitting
but gives proper standard errors.

---

## Plotting (`tulaplot.R`)

### Overview

Four exported functions for ggplot2 integration with Stata 18 `stcolor`
scheme defaults. All use explicit `ggplot2::` prefixes (no `importFrom`).

### `.tula_palette`

Internal 15-color vector from Stata 18's default `stcolor` scheme:
stblue, stred, stgreen, styellow, stc5–stc15.

### `tulaplot(data, mapping, ..., base_size = 14)`

Thin wrapper: `ggplot2::ggplot(data, mapping, ...) + theme_tula(base_size)`.
Returns a standard composable ggplot object.

### `theme_tula(base_size = 14, base_family = "sans")`

Complete ggplot2 theme (usable standalone). Key choices:
- White backgrounds (panel + plot)
- Light gray dashed major grid lines; no minor grid lines
- Black axis lines (no panel border)
- `base_size = 14` (vs ggplot2 default 11) for readability
- Default geom fill = stc1 blue (#1A85FF), colour = black
- Bar geoms get thin black outlines via `update_geom_defaults()`
- `complete = TRUE` — fully self-contained theme

### `scale_color_tula(...)` / `scale_fill_tula(...)`

Discrete scales using the 15-color palette. Recycles with warning if > 15 levels.

### Dependencies

`ggplot2 (>= 3.5.0)` is in `Imports`. The `>= 3.5.0` requirement is for
`ggplot2::element_geom()` used in `theme_tula()`.

---

## Parallel output layout (multinomial and quantile regression)

Both `tula_multinom_output` and `tula_rqs_output` support `parallel = TRUE`
for side-by-side outcome/quantile columns. The shared renderer is
`.format_parallel_multinom_table()` in `R/tula_multinom.R` (despite the name,
it works for any block-based output). Each column shows the coefficient with
significance stars (`*` p<0.05, `**` p<0.01, `***` p<0.001`). SE, statistic,
and p-value are not shown in parallel mode.

---

## Tabulate path architecture (`tulatab()`)

`tulatab()` produces Stata-style one-way frequency tables. It is a standalone
exported function (not dispatched through the `tula()` generic).

### Entry point

```r
tulatab(x, data = NULL, missing = FALSE, sort = FALSE,
        value = TRUE, label = TRUE, width = NULL, ...)
```

Two calling conventions:
- `tulatab(df$varname)` — direct vector
- `tulatab(varname, data = df)` — NSE lookup

### Variable types and Cum column

| Type | Cum? | Default order |
|------|------|---------------|
| Ordered factor | Yes | Level order |
| Unordered factor | No | Level order |
| Character | No | Alphabetical |
| Numeric | Yes | Ascending |
| Haven labelled | Yes | Ascending numeric value |

### Haven display parameters

| `value` | `label` | Display |
|---------|---------|---------|
| TRUE | TRUE | `1 Strong Democrat` (code + label, whitespace separated) |
| TRUE | FALSE | `1` (code only) |
| FALSE | TRUE | `Strong Democrat` (label only) |
| FALSE | FALSE | `1` (falls back to code) |

### The tab_df structure (7 columns)

```
value      chr   display string
num_value  dbl   raw numeric (haven/numeric; NA for factor/character)
label      chr   haven value label (NA if none)
freq       int   count
percent    dbl   percentage (0-100)
cum        dbl   cumulative % (NA if show_cum is FALSE)
is_missing lgl   TRUE for NA / tagged NA rows
```

Total row computed at print time, NOT stored in tab_df.

### Formatting details

- Percentages: exactly 2 decimal places (`sprintf("%.2f", x)`)
- Freq: comma-formatted integers, right-aligned, 10 chars wide
- Separator uses `+` at pipe position (Stata tab convention)
- Label truncation: simple right-truncation with `~` as last character
  (via `.truncate_tab_label()`, distinct from `.truncate_label()` which
  uses word-boundary logic for regression output)
- Haven variable label (`attr(x, "label")`) used in header when available

### S3 object: tula_tab

```r
list(tab_df, var_name, var_type, show_cum,
     has_haven_labels, has_haven_values,
     var_label, width, missing, value, label)
```

### Dependencies

`haven` is in Suggests (not Imports). Haven-labelled detection uses
`inherits(vec, "haven_labelled")` which works without loading haven.
Tagged NA handling requires `haven::is_tagged_na()` and `haven::na_tag()`.

---

## tula_compare() — Design notes (NOT YET IMPLEMENTED)

This section records design decisions made in conversation. Do not begin
implementation until the user gives the go-ahead. All open questions must be
resolved first.

### What it does

`tula_compare(m1, m2, ...)` prints a side-by-side comparison table of two or
more regression models. Each predictor occupies two lines: the coefficient
(with significance stars) on the first line, the standard error in parentheses
on the second. The table is self-contained — it is a new rendering pipeline,
not a variant of `format_coef_table()`.

### Decided

| # | Decision |
|---|----------|
| 1 | **Entry point — Option A: pre-dispatch interception inside `tula()`** — see detailed explanation below. `tula(m1)` continues to work exactly as before; `tula(m1, m2, m3)` triggers compare mode. No separate function is exported. |
| 2 | **Layout** — stacked: coefficient (with stars) on row 1, SE in parentheses on row 2. One pair of rows per predictor. Factor group header rows take one line (no numeric content). Intercept goes last, as in single-model output. |
| 3 | **Significance stars** — conventional cutoffs: `*` p<0.05, `**` p<0.01, `***` p<0.001. Star legend prints below the table. |
| 4 | **Column headers** — model object names by default. A `labels` argument (character vector, same length as models) accepted from day one for override, defaulting to object names. |
| 5 | **Binary-factor collapse** — the single-model collapse rule carries over, but only fires for a given variable if the collapse condition is met across **all** models being compared. |

### Option A — How pre-dispatch interception works

This is the central architectural decision. Read carefully before implementing.

#### The problem with naïve multi-argument dispatch

`tula()` uses `UseMethod("tula")`, which dispatches on the class of the
**first argument only**. So `tula(m1, m2, m3)` would call `tula.lm(m1, m2, m3)`
if `m1` is an `lm`. The extra models `m2`, `m3` arrive in `...` and are
silently ignored or cause errors. S3 dispatch cannot natively handle
"dispatch on the combination of arguments."

#### The solution: intercept before UseMethod

The body of `tula()` (the generic) runs **before** `UseMethod()` is called.
This means we can inspect `...` and divert to a different code path before
dispatch ever happens. The existing single-model path is completely untouched
because `UseMethod()` is only reached when no extra models are found.

#### Detailed implementation plan for `tula()`

```r
tula <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                 width = NULL, sep = 5L, mad = FALSE, median = FALSE,
                 digits = 7L, exp = FALSE, labels = NULL, ...) {

  .tula_call_nm <- deparse(substitute(model))

  # --- Multi-model detection (compare mode) ---------------------------------
  # Inspect unnamed elements of ... to see if any are regression model objects.
  # Named elements (e.g. width = 80) are options, not models — skip them.
  # A "regression model" is defined as: has a registered tula S3 method AND
  # is not a data.frame/vector (those belong to the summarize path).
  #
  # .is_regression_model() returns TRUE for lm, glm, polr, clm, multinom, etc.
  # It works by checking whether getS3method("tula", class(x)) exists.
  # This is self-maintaining: any future model type registered via tula.XYZ()
  # is automatically recognised here without changing this function.

  dots      <- list(...)
  dot_names <- names(dots)
  unnamed   <- dots[is.null(dot_names) | !nzchar(dot_names)]

  extra_models <- Filter(.is_regression_model, unnamed)
  extra_other  <- Filter(Negate(.is_regression_model), unnamed)

  # Error: mixing regression models with data frames / vectors is not allowed.
  if (length(extra_models) > 0L && length(extra_other) > 0L) {
    stop("tula(): cannot mix regression models with data frames or vectors. ",
         "Call tula() separately for each.", call. = FALSE)
  }

  # If extra regression models found, divert to compare path.
  if (length(extra_models) > 0L) {
    all_models   <- c(list(model), extra_models)
    model_labels <- if (!is.null(labels)) {
      labels
    } else {
      # Capture the original expressions the user typed for ALL model arguments.
      # substitute(list(...)) captures the unevaluated ... expressions;
      # [-1] drops the "list" symbol; as.character() converts to strings.
      mc   <- match.call(expand.dots = FALSE)
      nms  <- c(deparse(mc$model),
                as.character(as.list(mc$`...`)[is.null(dot_names) | !nzchar(dot_names)]))
      nms[seq_along(all_models)]
    }
    return(.tula_compare_impl(all_models, model_labels,
                              ref = ref, label = label,
                              width = width, exp = exp))
  }

  # --- Single-model path (unchanged) ----------------------------------------
  UseMethod("tula")
}
```

#### The `.is_regression_model()` helper

This internal function (defined in `tula.R`) is what distinguishes regression
model objects from data frames and vectors:

```r
.is_regression_model <- function(x) {
  # Must have a tula S3 method registered for its class...
  has_method <- any(vapply(class(x), function(cl)
    !is.null(getS3method("tula", cl, optional = TRUE)), logical(1L)))
  # ...but must NOT be a data.frame or atomic vector (those use summarize path)
  is_summarize_type <- is.data.frame(x) || (is.atomic(x) && is.null(dim(x)))
  has_method && !is_summarize_type
}
```

#### What the user sees

```r
tula(m1)              # single model — dispatches to tula.lm() as always
tula(m1, m2, m3)      # compare mode — three models side by side
tula(m1, m2, ref = TRUE)  # compare mode with named option — works fine
tula(m1, width = 80)  # single model with option — "width" is named, no detection
tula(m1, mydf)        # ERROR: mixing model with data frame not allowed
```

#### Where compare logic lives

`tula()` intercepts and calls `.tula_compare_impl()`, an internal function
defined in `R/tula_compare.R`. This function owns all compare-mode logic:
extracting coef_dfs from each model, building the master row set, rendering
the stacked table, printing footer stats. It is never called directly by users.

### Open questions (must resolve before writing code)

| # | Question | Notes |
|---|----------|-------|
| A | **Footer fit statistics** — fixed set for all models regardless of type, or each model contributes its natural stats with blanks where inapplicable? Fixed set is simpler; flexible is more informative but sparse with mixed model types. | Not yet decided. |
| B | **`ref = TRUE` support** — include in initial implementation, or defer to a future version? | Not yet decided. |
| C | ~~**Function name**~~ — resolved: no separate exported function. Compare mode is triggered by passing multiple model objects to `tula()`. Internal implementation function is `.tula_compare_impl()`. | Resolved. |
| D | **Row ordering** — when models have different predictors, what determines the order of rows in the master list? First model's order? Order of first appearance across all models? | Not yet discussed. |
| E | **Mixed model types** — if lm and glm (or polr) are passed together, is that supported? Any restrictions? | Not yet discussed. |
| F | **Factor grouping across models** — if a variable is a factor in one model and numeric in another, how is it shown? Group header with blank numeric-model column? | Not yet discussed. |

### Proposed file layout (tentative)

```
R/tula.R            modified: add .is_regression_model() helper and
                    multi-model detection block at top of tula() generic;
                    add `labels` parameter to tula() signature
R/tula_compare.R    new file: .tula_compare_impl(), new_tula_compare_output(),
                    print.tula_compare_output()
R/compare_table.R   new file: format_compare_table() — renders the stacked
                    coef/SE grid
```

The only change to an existing file is `R/tula.R` (the generic).
All model-specific method files (`tula_lm.R`, `tula_glm.R`, etc.) are
untouched. `tula_compare` does NOT appear in NAMESPACE as an export.

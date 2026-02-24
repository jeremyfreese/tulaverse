# tula — Developer Notes for Claude

This file is read automatically by Claude Code at the start of every session.
It records architectural decisions and how-to patterns for this package.

---

## What tula does

`tula()` is an S3 generic that produces Stata-style console output for two
distinct input types:

- **Regression models** (`lm`, `glm`, `multinom`, and future model types):
  coefficient table with header block (fit statistics).
- **Data frames / vectors** (the "summarize path"): Stata `-summarize`-style
  descriptive statistics table.

---

## File map

| File | Purpose |
|------|---------|
| `R/tula.R` | Generic, `tula.default`, `tula.data.frame`, `new_tula_output()`, `print.tula_output()` |
| `R/tula_lm.R` | `tula.lm()` method |
| `R/tula_glm.R` | `tula.glm()` method |
| `R/tula_multinom.R` | `tula.multinom()` method |
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
  width        = NULL,           # integer, Inf, or NULL (→ getOption("width"))
  value_fmts   = c(AIC = "f3"), # named character: header value format overrides (see format_header)
  exp          = FALSE           # logical; whether exponentiated coefficients are displayed
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
- **SE column**: shows the delta-method SE = `exp(β) × SE(β)`; header becomes `"DMSE"`.
- **Test statistic and p-value**: unchanged (invariant under exponentiation).
- **Reference rows**: show `1` instead of `0` (since `exp(0) = 1`).
- **CIs suppressed**: when `exp = TRUE`, `wide` is forced to `FALSE` with an
  informative `message()`. Exponentiated CIs (which would be asymmetric) are
  documented as a future consideration.

The transformation happens in `format_coef_table()` (in `coef_table.R`),
not in `build_coef_df()`. This keeps the data pipeline clean — `coef_df`
always stores raw (unexponentiated) values.

### Future extensibility: `exp_label`

The column header is currently always `"exp(b)"`. The architecture is designed
so that a model-specific label (e.g., `"Odds Ratio"` for logistic, `"IRR"`
for Poisson) could be added later via an `exp_label` field on the output
object, without changing `format_coef_table()` substantially.

### `exp`/`wide` interaction

Each `tula.*()` method checks `exp` before resolving `wide`:

```r
if (isTRUE(exp) && (isTRUE(wide) || is.null(wide))) {
  message("Note: wide output is not yet supported with exp = TRUE; CIs suppressed.")
  wide <- FALSE
}
wide <- .resolve_wide(wide, width)
```

The message fires only when the user would have gotten CIs (explicit
`wide = TRUE`, or `wide = NULL` which auto-resolves). When `wide = FALSE`
is already explicit, no message is printed.

---

## How to add a new model type

### Step 1 — Create `R/tula_MODELTYPE.R`

The method needs to:
1. Handle the `exp`/`wide` interaction (suppress CIs when `exp = TRUE`).
2. Resolve `wide` via `.resolve_wide(wide, width)`.
3. Run `summary(model)` (or equivalent).
4. Extract `ct`: a matrix with columns (Estimate, Std. Error, statistic, p-value).
   Column names don't matter — only column positions [1:4] are used.
5. Compute `ci`: a two-column matrix of CI bounds (rownames matching `ct`), or NULL.
6. Assemble `header_left` and `header_right` as named numeric vectors.
7. Call `build_coef_df()` and `new_tula_output()`, passing `exp = exp`.

Minimal template:

```r
#' @rdname tula
#' @export
tula.MODELTYPE <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                           width = NULL, exp = FALSE, ...) {
  # --- exp / wide interaction -----------------------------------------------
  if (isTRUE(exp) && (isTRUE(wide) || is.null(wide))) {
    message("Note: wide output is not yet supported with exp = TRUE; CIs suppressed.")
    wide <- FALSE
  }
  wide <- .resolve_wide(wide, width)

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
    width        = width,
    exp          = exp
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
tula.coxph <- function(model, wide = NULL, ref = FALSE, label = TRUE,
                       width = NULL, exp = FALSE, ...) {
  # --- exp / wide interaction -----------------------------------------------
  if (isTRUE(exp) && (isTRUE(wide) || is.null(wide))) {
    message("Note: wide output is not yet supported with exp = TRUE; CIs suppressed.")
    wide <- FALSE
  }
  wide <- .resolve_wide(wide, width)

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
    width        = width,
    exp          = exp
  )
}
```

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
| `wide` | NULL | regression | Show 95% CI columns (NULL = auto based on width ≥ 80) |
| `ref` | FALSE | regression | Show reference level rows |
| `label` | TRUE | regression | Use haven value labels |
| `exp` | FALSE | regression | Exponentiate coefficients; show exp(b) and DMSE; suppress CIs |
| `width` | NULL (→ `getOption("width")`) | both | Total output width |
| `sep` | 5L | summarize | Variables between separator lines |
| `mad` | FALSE | summarize | Show MAD instead of SD |
| `median` | FALSE | summarize | Show median/IQR instead of mean/SD |
| `digits` | 7L | summarize | Significant digits (avoids sci notation to ~10M) |

The summarize path also caps decimal places at 3 for `|x| > 0.1` via `.fmt_sum()`.

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

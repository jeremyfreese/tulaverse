#' tulaverse: Stata-Inspired Console Output and Visualization for R
#'
#' The **tulaverse** package brings Stata-style formatted output to base-R
#' modelling. Three entry points cover the common workflows; the rest of
#' the package is internal scaffolding.
#'
#' @section Main entry points:
#' \describe{
#'   \item{[tula()]}{S3 generic for regression and descriptive output.
#'     Dispatches on the input class: fitted models (`lm`, `glm`, `negbin`,
#'     `polr`, `clm`, `multinom`, `coxph`, `clogit`, `rq`/`rqs`, `fixest`,
#'     `survreg`, `svyglm`, `restriktor`) produce coefficient tables; data
#'     frames and atomic vectors produce summary or codebook output.}
#'   \item{[tulatab()]}{One-way frequency tables and two-way crosstabs,
#'     with optional means (or any subset of `mean`/`median`/`sd`/`n`/
#'     percentiles via `stat=`) and panel-by grouping.}
#'   \item{[tulaplot()] / [theme_tula()] / [scale_color_tula()] /
#'     [scale_fill_tula()]}{ggplot2 wrappers using Stata 18's `stcolor`
#'     palette and larger default text.}
#' }
#'
#' @section Output pipeline (for contributors):
#' Every regression method in this package follows the same shape:
#'
#' 1. The `tula.<class>()` method extracts a coefficient matrix from the
#'    fitted model, optionally applies a robust variance estimator via
#'    `.resolve_robust_vcov()`, and builds two named numeric vectors for
#'    the left / right header blocks.
#' 2. It calls `build_coef_df()` to produce the canonical
#'    row-per-coefficient data frame (factor grouping, label resolution,
#'    intercept placement, optional reference rows).
#' 3. It assembles a `tula_output` S3 object via `new_tula_output()` (or
#'    one of the variant constructors for multinomial / multi-quantile
#'    models) and attaches `select=` state via `.attach_select()`.
#' 4. The S3 print method ([print.tula_output()]) formats the header with
#'    `format_header()` and the coefficient table with
#'    `format_coef_table()`, sharing a total width computed by
#'    `compute_total_width()`.
#'
#' To add a new model class, see the "Anatomy of a tula.<class>() method"
#' comment block in `R/tula.R`. The shortest reference implementation is
#' `R/tula_lm.R`.
#'
#' @section S3 output classes:
#' \describe{
#'   \item{`tula_output`}{Single-block regression output (most models).}
#'   \item{`tula_multinom_output`}{Multinomial logit — one block per
#'     non-base outcome.}
#'   \item{`tula_rqs_output`}{Multi-quantile regression — one block per
#'     quantile.}
#'   \item{`tula_summary`}{Descriptive statistics table (data frames / vectors).}
#'   \item{`tula_codebook`}{Per-variable codebook output.}
#'   \item{`tula_tab` / `tula_crosstab` / `tula_crosstab_by`}{One-way,
#'     two-way, and panel-by frequency tables from `tulatab()`.}
#' }
#'
#' @keywords internal
"_PACKAGE"

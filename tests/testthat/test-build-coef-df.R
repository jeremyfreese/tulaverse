# Assertions on build_coef_df() in R/coef_table.R — the model-agnostic core
# that every tula.XYZ() method feeds into. We test its canonical coef_df
# structure directly from an lm(), no printing involved.

test_that("build_coef_df returns the canonical coef_df columns", {
  m  <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  df <- build_coef_df(m, summary(m)$coefficients, ci = NULL, wide = FALSE)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("label", "is_factor_header", "is_intercept", "is_ref",
                    "estimate", "std_err", "statistic", "p_value",
                    "ci_lower", "ci_upper") %in% names(df)))
})

test_that("build_coef_df groups a factor into a header plus level rows", {
  m  <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  df <- build_coef_df(m, summary(m)$coefficients, ci = NULL, wide = FALSE)

  # The factor contributes one header row (no estimate) and two level rows.
  expect_true(any(df$is_factor_header))
  hdr <- df[df$is_factor_header, ]
  expect_true(is.na(hdr$estimate[1]))

  # Exactly one intercept row, carrying the fitted intercept.
  expect_equal(sum(df$is_intercept), 1L)
  expect_equal(df$estimate[df$is_intercept], unname(coef(m)["(Intercept)"]),
               tolerance = 1e-8)

  # The continuous term keeps its coefficient.
  wt_row <- df[trimws(df$label) == "wt" & !df$is_factor_header, ]
  expect_equal(wt_row$estimate, unname(coef(m)["wt"]), tolerance = 1e-8)
})

test_that("build_coef_df handles an intercept-less model without inventing one", {
  m  <- lm(mpg ~ 0 + wt + hp, data = mtcars)
  df <- build_coef_df(m, summary(m)$coefficients, ci = NULL, wide = FALSE)

  expect_equal(sum(df$is_intercept), 0L)
  expect_setequal(trimws(df$label), c("wt", "hp"))
})

test_that("build_coef_df populates CI columns when wide = TRUE", {
  m  <- lm(mpg ~ wt, data = mtcars)
  ci <- confint(m)
  df <- build_coef_df(m, summary(m)$coefficients, ci = ci, wide = TRUE)

  wt_row <- df[trimws(df$label) == "wt", ]
  expect_equal(wt_row$ci_lower, ci["wt", 1], tolerance = 1e-8)
  expect_equal(wt_row$ci_upper, ci["wt", 2], tolerance = 1e-8)
})

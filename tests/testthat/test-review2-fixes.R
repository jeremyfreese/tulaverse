# Value-assertion regression guards for the bugs found in the second code
# review. These are deterministic (no snapshots), so they run on CI too.

test_that("multinom McFadden R2 uses the intercept-only null", {
  skip_if_not_installed("nnet")
  d <- data.frame(
    y = factor(rep(c("a", "b", "c"), times = c(60, 30, 10))),
    x = rep(c(0, 1), length.out = 100)
  )
  m  <- nnet::multinom(y ~ x, data = d, trace = FALSE)
  m0 <- nnet::multinom(y ~ 1, data = d, trace = FALSE)
  truth <- 1 - (-m$deviance / 2) / (-m0$deviance / 2)
  hdr <- tula(m)$header_right
  expect_equal(unname(hdr[["McFadden R-sq"]]), truth, tolerance = 1e-6)
  # and it must NOT be the (inflated) equal-probability baseline
  equal_prob <- 1 - (-m$deviance / 2) / (nrow(d) * log(1 / 3))
  expect_false(isTRUE(all.equal(unname(hdr[["McFadden R-sq"]]), equal_prob)))
})

test_that("crosstab mean+sort keeps each mean with its own row/column", {
  y <- rep(c("a", "b", "c"), times = c(5, 20, 10))
  x <- rep(c("L", "R"), length.out = length(y))
  z <- ifelse(y == "a", 100, ifelse(y == "b", 200, 300))
  unsorted <- capture.output(print(tulatab(y, x, mean = z)))
  sorted   <- capture.output(print(tulatab(y, x, mean = z, sort = TRUE)))
  # In both, the "b" row's cell mean is 200 and the "c" row's is 300.
  b_row_uns <- grep("^ *b ", unsorted, value = TRUE)[1]
  b_row_srt <- grep("^ *b ", sorted,   value = TRUE)[1]
  expect_true(grepl("200", b_row_uns))
  expect_true(grepl("200", b_row_srt))   # not scrambled to 300 by the sort
})

test_that("one-way mean Total excludes missing-y when not displayed", {
  y <- c("a", "a", "b", "b", NA, NA)
  z <- c(1, 1, 2, 2, 100, 100)
  # missing = FALSE: Total = mean of shown obs = 1.5 over N = 4
  info <- tulaverse:::.compute_oneway_means(
    vec = y, mean_vec = z,
    tab_df = tulaverse:::.build_tab_df(y, "character", FALSE, FALSE, FALSE),
    var_type = "character"
  )
  expect_equal(info$mean_total, 1.5)
  expect_equal(info$n_total, 4L)
})

test_that("coxph uses the robust SE column when the model was fit robustly", {
  skip_if_not_installed("survival")
  m  <- survival::coxph(survival::Surv(time, status) ~ age + sex,
                        data = survival::lung, robust = TRUE)
  cd <- tula(m)$coef_df
  sc <- summary(m)$coefficients
  age_se <- cd$std_err[trimws(cd$label) == "age"]
  expect_equal(age_se, unname(sc["age", "robust se"]), tolerance = 1e-9)
  expect_false(isTRUE(all.equal(age_se, unname(sc["age", "se(coef)"]))))
})

test_that("counting-process Cox reports total time at risk (stop - start)", {
  skip_if_not_installed("survival")
  m <- survival::coxph(survival::Surv(start, stop, event) ~ rx,
                       data = survival::bladder2)
  y <- m$y
  expect_equal(unname(tula(m)$header_left[["Time at risk"]]),
               sum(y[, 2L] - y[, 1L]))
})

test_that("rqs errors (not silently ignores) when robust SEs are unavailable", {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("sandwich")
  m <- quantreg::rq(mpg ~ wt + hp, data = mtcars, tau = c(0.25, 0.5, 0.75))
  expect_error(suppressWarnings(tula(m, robust = TRUE)), "not available")
})

test_that("build_coef_df groups coefficients correctly with an aliased term", {
  d <- mtcars
  d$wt2  <- d$wt * 2            # perfectly collinear -> aliased (dropped)
  d$cylf <- factor(d$cyl)
  m  <- lm(mpg ~ wt + wt2 + cylf, data = d)
  expect_true(any(is.na(coef(m))))          # confirm aliasing
  df <- tula(m)$coef_df
  # The cylf factor must form a header with both non-reference levels under it.
  expect_true(any(df$is_factor_header & trimws(df$label) == "cylf"))
  hdr_i <- which(df$is_factor_header & trimws(df$label) == "cylf")
  levs  <- trimws(df$label[(hdr_i + 1L):(hdr_i + 2L)])
  expect_setequal(levs, c("6", "8"))
})

test_that("character predictors are grouped like factors", {
  d <- mtcars
  d$grp <- c("lo", "mid", "hi")[d$gear - 2L]
  df <- tula(lm(mpg ~ grp + wt, data = d))$coef_df
  expect_true(any(df$is_factor_header & trimws(df$label) == "grp"))
})

test_that("glm sets a family-appropriate exp_label", {
  logit <- tula(glm(am ~ wt, data = mtcars, family = binomial), exp = TRUE)
  expect_equal(logit$exp_label, "Odds Ratio")
  pois <- tula(glm(carb ~ wt, data = mtcars, family = poisson), exp = TRUE)
  expect_equal(pois$exp_label, "IRR")
})

test_that("tula() on an empty data frame errors clearly", {
  expect_error(tula(data.frame()), "nothing to summarize")
})

test_that("tulatab() does not crash on out-of-integer-range values", {
  expect_no_error(invisible(capture.output(
    print(tulatab(c(3e9, 3e9, 4e9), c("a", "b", "a")))
  )))
})

test_that("tulatab() does not crash on colliding display levels", {
  expect_no_error(invisible(capture.output(
    print(tulatab(factor(c("a", ".", NA, "a")), c("x", "y", "x", "y"),
                  missing = TRUE))
  )))
})

test_that("character-backed haven codebook shows codes, not blank cells", {
  skip_if_not_installed("haven")
  v <- haven::labelled(c("a", "b", "zz", "a"),
                       labels = c(Apple = "a", Banana = "b"))
  out <- capture.output(print(tula(data.frame(x = v), codebook = TRUE)))
  # the unlabeled code "zz" must appear in the tabulation
  expect_true(any(grepl("zz", out)))
  # and no empty "Mean:" line (it must not route to continuous)
  expect_false(any(grepl("^\\s*Mean:", out)))
})

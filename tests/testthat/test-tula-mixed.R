# Snapshot + value tests for tula.lmerMod() / tula.glmerMod() — mixed-effects
# models via lme4. Snapshots are pinned to a width and skipped on CI (numeric
# formatting of iterative fits is platform-sensitive); the value assertions run
# everywhere.

test_that("linear mixed model (lmer) output is stable", {
  skip_if_not_installed("lme4")
  local_reproducible_output(width = 90)
  skip_on_ci()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_snapshot(tula(m))
})

test_that("generalized linear mixed model (glmer, exp) output is stable", {
  skip_if_not_installed("lme4")
  local_reproducible_output(width = 90)
  skip_on_ci()
  m <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                   data = lme4::cbpp, family = binomial)
  expect_snapshot(tula(m, exp = TRUE))
})

test_that("crossed random effects render both grouping factors", {
  skip_if_not_installed("lme4")
  local_reproducible_output(width = 72)
  skip_on_ci()
  m <- lme4::lmer(diameter ~ 1 + (1 | plate) + (1 | sample),
                  data = lme4::Penicillin)
  expect_snapshot(tula(m))
})

# --- Value assertions (run on CI) -----------------------------------------

test_that("lmer fixed-effects estimates and normal-approx p-values are correct", {
  skip_if_not_installed("lme4")
  m  <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  cd <- tula(m)$coef_df
  fe <- lme4::fixef(m)
  days <- cd[trimws(cd$label) == "Days", ]
  expect_equal(days$estimate, unname(fe[["Days"]]), tolerance = 1e-6)
  # p-value is the normal-approximation two-sided p from the z = est/se
  se <- sqrt(diag(as.matrix(stats::vcov(m))))[["Days"]]
  expect_equal(days$p_value, 2 * stats::pnorm(-abs(fe[["Days"]] / se)),
               tolerance = 1e-8)
  expect_equal(tula(m)$stat_label, "z")
})

test_that("random-effects section reports sd and correlation from VarCorr", {
  skip_if_not_installed("lme4")
  m  <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  vc <- as.data.frame(lme4::VarCorr(m))
  rd <- tula(m)$ranef_df
  # sd((Intercept)) shown as sd(_cons); value = the sd from VarCorr
  sd_cons <- rd$estimate[rd$label == "sd(_cons)"]
  expect_equal(sd_cons,
               vc$sdcor[vc$grp == "Subject" & vc$var1 == "(Intercept)" & is.na(vc$var2)],
               tolerance = 1e-8)
  # residual sd present for a linear model
  expect_true("sd(Residual)" %in% rd$label)
  expect_equal(rd$estimate[rd$label == "sd(Residual)"],
               vc$sdcor[vc$grp == "Residual"], tolerance = 1e-8)
  # correlation row present and equals VarCorr's sdcor for the covariance term
  expect_equal(rd$estimate[rd$label == "corr(_cons,Days)"],
               vc$sdcor[vc$grp == "Subject" & !is.na(vc$var2)], tolerance = 1e-8)
})

test_that("glmer has no residual sd and reports per-factor group counts", {
  skip_if_not_installed("lme4")
  m  <- lme4::glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                    data = lme4::cbpp, family = binomial)
  out <- tula(m)
  expect_false("sd(Residual)" %in% out$ranef_df$label)   # binomial: no residual scale
  expect_equal(unname(out$header_right[["N groups: herd"]]),
               unname(lme4::ngrps(m)[["herd"]]))
  expect_equal(tula(m, exp = TRUE)$exp_label, "Odds Ratio")
})

test_that("mixed models warn and ignore robust/cluster arguments", {
  skip_if_not_installed("lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  expect_warning(tula(m, robust = TRUE), "random-effects structure")
  expect_warning(tula(m, cluster = "Subject"), "ignored")
})

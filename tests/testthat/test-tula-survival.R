# Snapshot tests for the survival-based methods:
# tula.coxph()   — Cox proportional hazards (survival::coxph)
# tula.clogit()  — conditional logistic (survival::clogit)
# tula.survreg() — tobit / parametric survival (survival::survreg)
# All live in the survival package, so they share one skip guard.

test_that("Cox proportional hazards output is stable", {
  skip_if_not_installed("survival")
  local_reproducible_output(width = 80)
  m <- survival::coxph(survival::Surv(time, status) ~ age + sex,
                       data = survival::lung)
  expect_snapshot(tula(m))
})

test_that("Cox model with exp = TRUE (hazard ratios) is stable", {
  skip_if_not_installed("survival")
  local_reproducible_output(width = 90)
  m <- survival::coxph(survival::Surv(time, status) ~ age + sex,
                       data = survival::lung)
  expect_snapshot(tula(m, exp = TRUE))
})

test_that("conditional logistic (clogit) output is stable", {
  skip_if_not_installed("survival")
  skip_if_not_installed("withr")
  # clogit builds a coxph() call internally and its formula uses the bare
  # strata() special, so survival must be attached (not just ::-qualified).
  # withr::local_package() attaches it for this test only and detaches after.
  withr::local_package("survival")
  local_reproducible_output(width = 80)
  m <- clogit(case ~ spontaneous + induced + strata(stratum), data = infert)
  expect_snapshot(tula(m))
})

test_that("survival regression (survreg / tobit) output is stable", {
  skip_if_not_installed("survival")
  local_reproducible_output(width = 80)
  m <- survival::survreg(survival::Surv(futime, fustat) ~ age + rx,
                         data = survival::ovarian)
  expect_snapshot(tula(m))
})

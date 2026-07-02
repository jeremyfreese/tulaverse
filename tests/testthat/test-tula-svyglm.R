# Snapshot test for tula.svyglm() — survey-weighted GLM via survey::svyglm().
# Skips wherever survey is unavailable (Suggests only).

test_that("survey-weighted logistic (svyglm) output is stable", {
  skip_if_not_installed("survey")
  local_reproducible_output(width = 80)
  skip_on_ci()
  des <- survey::svydesign(ids = ~1, weights = ~1, data = mtcars)
  m   <- survey::svyglm(am ~ cyl + wt, design = des,
                        family = quasibinomial())
  expect_snapshot(tula(m))
})

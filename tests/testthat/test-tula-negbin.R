# Snapshot tests for tula.negbin() — negative binomial via MASS::glm.nb().
# The model is fit under suppressWarnings() (glm.nb's theta estimation can hit
# its iteration limit on small data) so only the rendered table is snapshotted.

test_that("negative binomial output is stable", {
  skip_if_not_installed("MASS")
  local_reproducible_output(width = 80)
  m <- suppressWarnings(MASS::glm.nb(carb ~ wt + hp, data = mtcars))
  expect_snapshot(tula(m))
})

test_that("negative binomial with a grouped factor is stable", {
  skip_if_not_installed("MASS")
  local_reproducible_output(width = 80)
  m <- suppressWarnings(MASS::glm.nb(carb ~ wt + factor(cyl), data = mtcars))
  expect_snapshot(tula(m))
})

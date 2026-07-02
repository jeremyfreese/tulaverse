# theme_tula() applies its geom defaults through a theme-scoped element_geom()
# rather than a global update_geom_defaults() call. These tests lock in both
# the intended bar styling and the absence of any session-wide side effect.

# Build a one-bar plot and return the drawn geom's resolved aesthetics.
bar_aes <- function(theme = NULL) {
  p <- ggplot2::ggplot(data.frame(x = c("a", "b", "c")), ggplot2::aes(x)) +
    ggplot2::geom_bar()
  if (!is.null(theme)) p <- p + theme
  ld <- ggplot2::ggplot_build(p)$data[[1]]
  list(colour = as.character(ld$colour[1]),
       fill   = as.character(ld$fill[1]),
       linewidth = ld$linewidth[1])
}

test_that("theme_tula() returns a complete ggplot2 theme", {
  th <- theme_tula()
  expect_s3_class(th, "theme")
  expect_true(attr(th, "complete"))
})

test_that("theme_tula() gives filled geoms a thin black outline over stc1 fill", {
  a <- bar_aes(theme_tula())
  expect_equal(a$colour, "black")
  expect_equal(a$fill, "#1A85FF")
  expect_equal(a$linewidth, 0.3)
})

test_that("theme_tula() does not mutate global geom defaults", {
  # A default bar has no outline (colour NA). Applying theme_tula() to one plot
  # must not change that for a subsequent, un-themed plot.
  before <- bar_aes(NULL)
  invisible(bar_aes(theme_tula()))
  after <- bar_aes(NULL)
  expect_equal(before$colour, NA_character_)
  expect_equal(after$colour, NA_character_)   # no session-wide leakage
  expect_equal(after$linewidth, before$linewidth)
})

test_that("theme_tula() light-mode colours are the expected values", {
  th <- theme_tula()
  expect_equal(th$panel.background$fill, "#FFFFFF")
  expect_equal(th$text$colour, "#000000")
  expect_equal(th$panel.grid.major$colour, "#E6E6E6")
  expect_equal(th$strip.background$fill, "#F2F2F2")
})

test_that("theme_tula_darkmode() returns a complete dark-palette theme", {
  th <- theme_tula_darkmode()
  expect_s3_class(th, "theme")
  expect_true(attr(th, "complete"))
  expect_equal(th$panel.background$fill, "#1E1E1E")
  expect_equal(th$plot.background$fill, "#1E1E1E")
  expect_equal(th$text$colour, "#E6E6E6")
  expect_equal(th$axis.line$colour, "#E6E6E6")
  expect_equal(th$panel.grid.major$colour, "#3D3D3D")
  expect_equal(th$strip.background$fill, "#2E2E2E")
})

test_that("theme_tula_darkmode() gives filled geoms a light outline over stc1 fill", {
  a <- bar_aes(theme_tula_darkmode())
  expect_equal(a$colour, "#E6E6E6")   # light outline, visible on dark bg
  expect_equal(a$fill, "#1A85FF")     # same stc1 blue as light mode
  expect_equal(a$linewidth, 0.3)
})

test_that("theme_tula_darkmode() does not mutate global geom defaults", {
  before <- bar_aes(NULL)
  invisible(bar_aes(theme_tula_darkmode()))
  after <- bar_aes(NULL)
  expect_equal(after$colour, NA_character_)   # no session-wide leakage
  expect_equal(after$linewidth, before$linewidth)
})

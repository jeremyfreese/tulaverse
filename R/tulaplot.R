# Stata 18 stcolor scheme: 15-color discrete palette.
# See: https://www.stata.com/stata18/new-graph-style/
.tula_palette <- c(
  "#1A85FF",   # stc1  (stblue)
  "#D41159",   # stc2  (stred)
  "#00BF7F",   # stc3  (stgreen)
  "#FFD400",   # stc4  (styellow)
  "#4F2C99",   # stc5
  "#FF6333",   # stc6
  "#4DB7FF",   # stc7
  "#7C0015",   # stc8
  "#0FEFAF",   # stc9
  "#FAA307",   # stc10
  "#758BFD",   # stc11
  "#FED9B7",   # stc12
  "#08234C",   # stc13
  "#F88DAD",   # stc14
  "#0F5156"    # stc15
)


#' Stata-inspired ggplot2 theme with larger text
#'
#' A complete ggplot2 theme inspired by Stata 18's `stcolor` graph scheme:
#' white backgrounds, faint gray grid lines, black axis lines, and enlarged
#' text for readability in teaching and presentation contexts.
#' `theme_tula_darkmode()` is the dark-mode counterpart — the same layout with a
#' dark background, light foreground, and light geom outlines.
#'
#' Add it to any plot with `ggplot(...) + theme_tula()`. Any theme elements the
#' user adds afterward override the defaults set here, following normal ggplot2
#' layering rules. Pair with [scale_color_tula()] / [scale_fill_tula()] for the
#' matching Stata `stcolor` palette.
#'
#' @param base_size Base font size in points (default 14). All other text
#'   sizes are computed relative to this value. The ggplot2 default is 11;
#'   14 produces noticeably larger labels suitable for slides and documents.
#' @param base_family Base font family (default `"sans"`).
#'
#' @return A [ggplot2::theme()] object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_tula()
#'
#' # Dark mode:
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_tula_darkmode()
#'
#' @export
theme_tula <- function(base_size = 14, base_family = "sans") {
  .tula_build_theme(base_size, base_family, dark = FALSE)
}

#' @rdname theme_tula
#' @export
theme_tula_darkmode <- function(base_size = 14, base_family = "sans") {
  .tula_build_theme(base_size, base_family, dark = TRUE)
}

# Shared theme builder. `dark` selects the color palette; the layout, sizing,
# and geom defaults are identical between the light and dark variants so the
# two never drift apart.
.tula_build_theme <- function(base_size, base_family, dark) {
  if (dark) {
    fg           <- "#E6E6E6"   # foreground: text, lines, axes, ticks
    bg           <- "#1E1E1E"   # panel / plot / legend background
    grid         <- "#3D3D3D"   # faint grid, a bit lighter than the bg
    strip        <- "#2E2E2E"   # facet strip, a bit lighter than the bg
    geom_outline <- "#E6E6E6"   # light outlines for points/lines/bar borders
  } else {
    fg           <- "#000000"
    bg           <- "#FFFFFF"
    grid         <- "#E6E6E6"   # faint grid, a bit darker than the bg
    strip        <- "#F2F2F2"
    geom_outline <- "black"
  }

  half_line <- base_size / 2

  ggplot2::theme(
    # --- Base text element (all text inherits from this) ---
    text = ggplot2::element_text(
      family     = base_family,
      face       = "plain",
      colour     = fg,
      size       = base_size,
      hjust      = 0.5,
      vjust      = 0.5,
      angle      = 0,
      lineheight = 0.9,
      margin     = ggplot2::margin(),
      debug      = FALSE
    ),

    # --- Line and rect defaults ---
    line = ggplot2::element_line(
      colour    = fg,
      linewidth = 0.5,
      linetype  = 1,
      lineend   = "butt"
    ),
    rect = ggplot2::element_rect(
      fill      = bg,
      colour    = NA,
      linewidth = 0.5,
      linetype  = 1
    ),

    # --- Plot (outer canvas) ---
    plot.background = ggplot2::element_rect(fill = bg, colour = NA),
    plot.title = ggplot2::element_text(
      size   = ggplot2::rel(1.286),
      face   = "bold",
      hjust  = 0,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.subtitle = ggplot2::element_text(
      size   = ggplot2::rel(1.0),
      hjust  = 0,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size   = ggplot2::rel(0.786),
      hjust  = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.margin = ggplot2::margin(half_line, half_line, half_line, half_line),

    # --- Panel (plot area) ---
    panel.background = ggplot2::element_rect(fill = bg, colour = NA),
    panel.border     = ggplot2::element_blank(),
    panel.spacing    = ggplot2::unit(half_line, "pt"),

    # --- Grid lines (faint, solid, major only) ---
    panel.grid.major = ggplot2::element_line(
      colour    = grid,
      linewidth = 0.4,
      linetype  = "solid"
    ),
    panel.grid.minor = ggplot2::element_blank(),

    # --- Axes ---
    axis.line = ggplot2::element_line(
      colour    = fg,
      linewidth = 0.4
    ),
    axis.ticks = ggplot2::element_line(
      colour    = fg,
      linewidth = 0.4
    ),
    axis.ticks.length = ggplot2::unit(half_line / 2, "pt"),
    axis.text = ggplot2::element_text(
      size   = ggplot2::rel(0.857),
      colour = fg
    ),
    axis.title = ggplot2::element_text(
      size   = ggplot2::rel(1.0),
      colour = fg
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line)
    ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line)
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = half_line),
      angle  = 90
    ),
    axis.title.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = half_line),
      angle  = -90
    ),

    # --- Legend ---
    legend.background = ggplot2::element_rect(
      fill   = bg,
      colour = NA
    ),
    legend.key = ggplot2::element_rect(
      fill   = bg,
      colour = NA
    ),
    legend.key.size  = ggplot2::unit(1.2, "lines"),
    legend.text      = ggplot2::element_text(size = ggplot2::rel(0.857)),
    legend.title     = ggplot2::element_text(
      size = ggplot2::rel(1.0),
      face = "bold"
    ),
    legend.position  = "right",

    # --- Facet strips ---
    strip.background = ggplot2::element_rect(
      fill   = strip,
      colour = NA
    ),
    strip.text = ggplot2::element_text(
      size   = ggplot2::rel(0.929),
      colour = fg,
      margin = ggplot2::margin(
        t = half_line / 2, b = half_line / 2,
        l = half_line,     r = half_line
      )
    ),

    # --- Default geom aesthetics ---
    # colour = foreground for points, lines, and borders; fill = stc1 blue for
    # bars/histograms/areas; borderwidth = 0.3 gives filled geoms thin outlines.
    # This is theme-scoped (applies only to plots using the theme, reverts
    # automatically) — no session-wide update_geom_defaults() side effect.
    geom = ggplot2::element_geom(
      colour      = geom_outline,
      fill        = "#1A85FF",
      borderwidth = 0.3
    ),

    complete = TRUE
  )
}


#' Stata stcolor discrete color/fill scales
#'
#' Discrete color and fill scales using the 15-color palette from Stata 18's
#' default `stcolor` graph scheme. Colors cycle in order: stblue, stred,
#' stgreen, styellow, then stc5 through stc15. If more than 15 levels are
#' present, colors are recycled with a warning.
#'
#' @param ... Additional arguments passed to [ggplot2::discrete_scale()].
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'   geom_point() +
#'   scale_color_tula()
#'
#' @export
scale_color_tula <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette    = function(n) {
      if (n > length(.tula_palette)) {
        warning("tula palette has ", length(.tula_palette),
                " colors; recycling for ", n, " levels.",
                call. = FALSE)
      }
      unname(rep_len(.tula_palette, n))
    },
    ...
  )
}

#' @rdname scale_color_tula
#' @export
scale_fill_tula <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette    = function(n) {
      if (n > length(.tula_palette)) {
        warning("tula palette has ", length(.tula_palette),
                " colors; recycling for ", n, " levels.",
                call. = FALSE)
      }
      unname(rep_len(.tula_palette, n))
    },
    ...
  )
}

# theme_tula_gallery.R
# -----------------------------------------------------------------------------
# Renders a single image showcasing several ggplot2 geom types styled with
# theme_tula() and the matching scale_color_tula() / scale_fill_tula() scales.
# Output: theme_tula_darkmode_gallery.png
#
# Run with:  Rscript theme_tula_gallery.R
# -----------------------------------------------------------------------------

suppressMessages(devtools::load_all("."))
suppressMessages({
  library(ggplot2)
  library(patchwork)
})

mt <- mtcars
mt$cyl  <- factor(mt$cyl)
mt$gear <- factor(mt$gear)
bs <- 12  # base font size tuned for a six-panel grid

# 1. Scatter with a fitted line, coloured by group
p_scatter <- ggplot(mt, aes(wt, mpg, colour = cyl)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8) +
  scale_color_tula() +
  labs(title = "Scatter + linear fit", x = "Weight (1000 lbs)", y = "MPG",
       colour = "Cylinders") +
  theme_tula_darkmode(base_size = bs)

# 2. Bar counts (shows the theme's thin black outlines on filled geoms)
p_bar <- ggplot(mt, aes(cyl, fill = cyl)) +
  geom_bar() +
  scale_fill_tula() +
  labs(title = "Bar counts", x = "Cylinders", y = "Count") +
  theme_tula_darkmode(base_size = bs) +
  theme(legend.position = "none")

# 3. Boxplot by group
p_box <- ggplot(mt, aes(cyl, mpg, fill = cyl)) +
  geom_boxplot() +
  scale_fill_tula() +
  labs(title = "Boxplot", x = "Cylinders", y = "MPG") +
  theme_tula_darkmode(base_size = bs) +
  theme(legend.position = "none")

# 4. Histogram
p_hist <- ggplot(mt, aes(mpg)) +
  geom_histogram(bins = 10) +
  labs(title = "Histogram", x = "MPG", y = "Count") +
  theme_tula_darkmode(base_size = bs)

# 5. Time series line
ec <- economics[economics$date >= as.Date("2000-01-01"), ]
p_line <- ggplot(ec, aes(date, unemploy / 1000)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Time series", x = NULL, y = "Unemployed (millions)") +
  theme_tula_darkmode(base_size = bs)

# 6. Stacked proportions
p_stack <- ggplot(mt, aes(gear, fill = cyl)) +
  geom_bar(position = "fill") +
  scale_fill_tula() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Stacked proportions", x = "Gears", y = "Share",
       fill = "Cylinders") +
  theme_tula_darkmode(base_size = bs)

gallery <- (p_scatter | p_bar | p_box) /
           (p_hist    | p_line | p_stack) +
  plot_annotation(
    title    = "theme_tula_darkmode(): dark-mode ggplot2 defaults",
    subtitle = "Six geom types with theme_tula_darkmode() + scale_color_tula() / scale_fill_tula()",
    theme    = theme_tula_darkmode(base_size = bs)
  )

ggsave("theme_tula_darkmode_gallery.png", gallery,
       width = 13, height = 8, dpi = 150, bg = "#1E1E1E")

message("Wrote theme_tula_darkmode_gallery.png")

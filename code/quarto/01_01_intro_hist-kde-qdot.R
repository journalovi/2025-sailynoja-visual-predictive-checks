# This script is used for producing the three density visualizations in hte introduction section.

library(ggplot2)
library(ggdist)
library(scales)

source("code/R/helpers.R")

theme_set(ppc_paper_theme(28))

set.seed(3245)

# The sample to be visualized.
x <- rnorm(300)

# Histogram
p_h <-
  ggplot() +
  geom_histogram(
    aes(x = x, y = after_stat(density)),
    fill = paper_colors["mid"],
    colour = paper_colors["dark_highlight"],
    linewidth = 1
  ) +
  theme(axis.title = element_blank()) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, .5)) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(-3, 3)) +
  coord_cartesian(ylim = c(0, NA), clip = "off", expand = FALSE)

# Kernel density plot
p_d <-
  ggplot() +
  geom_density(
    aes(x = x),
    bw = "sj",
    fill = paper_colors["mid"],
    colour = paper_colors["dark_highlight"],
    linewidth = 1
  ) +
  theme(axis.title = element_blank()) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, .5)) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(-3, 3)) +
  coord_cartesian(ylim = c(0, NA), clip = "off", expand = FALSE)

# Quantile dot plot
p_q <-
  ggplot() +
  stat_dots(
    aes(x = x),
    quantiles = 100,
    scale = 1,
    height = max(density(x)$y),
    fill = paper_colors["mid"],
    colour = paper_colors["mid"]
  ) +
  theme(axis.title = element_blank()) +
  scale_y_continuous(breaks = pretty_breaks(), limits = c(0, .5)) +
  scale_x_continuous(breaks = pretty_breaks(), limits = c(-3, 3)) +
  coord_cartesian(ylim = c(0, NA), clip = "off", expand = FALSE)

p_h
p_d
p_q


ggsave(
  "images/intro_histogram.png",
  plot = p_h,
  device = "png",
  width = 7,
  height = 5,
  dpi = 72
)

ggsave(
  "images/intro_density.png",
  plot = p_d,
  device = "png",
  width = 7,
  height = 5,
  dpi = 72
)

ggsave(
  "images/intro_qdot.png",
  plot = p_q,
  device = "png",
  width = 7,
  height = 5,
  dpi = 72
)

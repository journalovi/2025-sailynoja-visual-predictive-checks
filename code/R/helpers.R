ppc_paper_theme <- function(base_size = 16) {
  bayesplot::theme_default(base_family = "Sans", base_size = base_size) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(colour = "#666666", size = base_size * 7 / 8),
    axis.ticks = ggplot2::element_line(colour = "#666666"),
    axis.line = ggplot2::element_line(colour = "#666666"),
    title = ggplot2::element_text(colour = "#666666", size = base_size),
    plot.subtitle = ggplot2::element_text(colour = "#666666", size = base_size * 7 / 8),
    legend.text = ggplot2::element_text(colour = "#666666", size = base_size * 7 / 8),
    legend.title = ggplot2::element_text(colour = "#666666", size = base_size * 7 / 8)
  )
}

paper_colors <- unlist(c(bayesplot::color_scheme_get("blue"),khroma::color("sunset")(9)[8:9]))
names(paper_colors)[7:8] <- c("orange", "red")

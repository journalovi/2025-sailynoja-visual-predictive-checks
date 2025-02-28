rooto_discrete <- function(y,
                           yrep,
                           max_count = NULL,
                           sum_over_max = TRUE,
                           prob = .5,
                           prob_outer = .95,
                           alpha = 0.7,
                           size = 1,
                           fatten = 2.5,
                           linewidth = 1,
                           highlight_color = "red",
                           ...) {
  require("ggplot2")
  require("bayesplot")
  if (!is.null(max_count) && sum_over_max) {
    # Compress values above max_count to max_count
    y[y > max_count] <- max_count
    yrep[yrep > max_count] <- max_count
  }
  # Get data range. Truncate if max_count given
  x_range <- range(y, yrep)[1]:min(range(y, yrep)[2], max_count)
  n <- length(x_range)
  tyrep <- t(apply(yrep, 1, \(r) sapply(x_range, function(n) sum(r == n))))
  data <-
    ppc_intervals_data(
      y = sapply(x_range, function(n) sum(y == n)),
      yrep = tyrep,
      x = x_range,
      prob = prob,
      prob_outer = prob_outer
    )
  data$m <- colMeans(tyrep)

  p <- ggplot(data) +
    aes(x = x) +
    geom_linerange(
      aes(ymin = ll, ymax = hh, color = "yrep"),
      alpha = 0.5 * alpha,
      linewidth = linewidth
    ) +
    geom_pointrange(
      aes(y = m, ymin = l, ymax = h, color = "yrep", fill = "yrep"),
      shape = 21,
      stroke = 0,
      size = size,
      fatten = fatten,
      linewidth = linewidth,
      alpha = alpha
    ) +
    geom_point(
      mapping = aes(
        y = y_obs,
        color = ifelse((y_obs < ll) | (y_obs > hh), "y_out", "y"),
        fill = ifelse((y_obs < ll) | (y_obs > hh), "y_out", "y")
      ),
      shape = 21,
      stroke = 0.5,
      size = size
    ) +
    scale_y_sqrt() +
    ylab("Count") +
    xlab("") +
    guides(y = guide_axis(minor.ticks = TRUE)) +
    scale_color_manual(
      aesthetics = c("fill", "colour"),
      values = c(
        "y" = color_scheme_get()$dark,
        "yrep" = color_scheme_get()$mid,
        "y_out" = highlight_color
      ),
      breaks = c("y", "yrep"),
      labels = c("Observed", "Expected")
    ) +
    labs(colour = "", fill = "")

  if (!is.null(max_count) && sum_over_max) {
    p <- p +
      geom_vline(
        xintercept = max(x_range) - .5,
        linetype = 3,
        colour = "gray",
      ) +
      scale_x_continuous(labels = \(breaks) {
        breaks[length(breaks)] <- paste(breaks[length(breaks)], "+", sep = "")
        breaks
      })
  }
  p
}

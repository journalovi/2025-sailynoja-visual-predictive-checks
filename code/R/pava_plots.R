ppc_calibration_pava <- function(y,
                                 p = NULL,
                                 yrep = NULL,
                                 quantiles = 100,
                                 region.method = "resampling",
                                 n.boot = 200,
                                 dot_scale = .25,
                                 fill_alpha = .8,
                                 cep_line_color = "red") {
  require("reliabilitydiag")
  require("ggdist")
  require("dplyr")

  if (is.matrix(yrep) && (nrow(yrep) > 1)) {
    bayesplot:::validate_predictions(yrep, length(y))
    if (nrow(yrep) < 100) {
      cli::cli_inform(paste(
        "Computing consistency intervals using",
        nrow(yrep),
        "predictive samples. We recommend using more predictions for more accurate bounds."
      ))
    }
    ep <- colMeans(yrep)
    ep_o <- order(ep)
    ep_s <- sort(ep)
    ceps <- Iso::pava(y[ep_o])
    consistency_intervals <- seq_len(nrow(yrep)) |>
      lapply(\(k) data.frame(
        y = Iso::pava(yrep[k, ep_o]),
        x = ep_s,
        id_ = ep_o
      )) |>
      bind_rows() |>
      group_by(id_) |>
      summarise(
        upper = quantile(y, .95),
        lower = quantile(y, .05),
        x = mean(x)
      ) |>
      arrange(x)
  } else {
    ceps <- Iso::pava(y[order(p)])
    consistency_intervals <- reliabilitydiag::reliabilitydiag(
      y = y,
      x = p,
      region.method = region.method,
      region.position = "diagonal",
      n.boot = n.boot
    )$x$regions |>
      reframe(
        lower = rep(lower, n),
        upper = rep(upper, n),
        x = rep(x, n)
      ) |>
      select(lower, upper, x)
  }
  ggplot(consistency_intervals) +
    aes(
      x = x,
      y = ceps,
      ymin = lower,
      ymax = upper
    ) +
    stat_dots(
      aes(x = x),
      quantiles = 100,
      height = dot_scale,
      scale = 1,
      shape = 19,
      colour = color_scheme_get()$mid,
      inherit.aes = FALSE
    ) +
    geom_ribbon(
      alpha = fill_alpha,
      fill = color_scheme_get()$mid
    ) +
    geom_abline(slope = 1, intercept = 0, col = "black", lty = 2, alpha = .3) +
    geom_line(colour = cep_line_color, linewidth = 1) +
    coord_equal(xlim = c(0, 1.01), ylim = c(0, 1.01), expand = FALSE) +
    xlab("Predicted probability") +
    ylab("CEP") +
    theme(panel.grid = element_line(colour = "gray", linewidth = .2))
}

ppc_residual_pava <- function(y,
                              x,
                              p = NULL,
                              yrep = NULL,
                              quantiles = 100,
                              region.method = "resampling",
                              n.boot = 500,
                              fill_alpha = .8,
                              cep_color = "red") {
  require("reliabilitydiag")
  require("ggdist")
  require("dplyr")

  if (is.matrix(yrep) && (nrow(yrep) > 1)) {
    bayesplot:::validate_predictions(yrep, length(y))
    if (nrow(yrep) < 100) {
      cli::cli_inform(paste(
        "Computing consistency intervals using",
        nrow(yrep),
        "predictive samples. We recommend using more predictions for more accurate bounds."
      ))
    }
    ep <- colMeans(yrep)
    ep_o <- order(ep)
    ep_s <- sort(ep)
    ceps <- Iso::pava(y[ep_o])
    x <- x[ep_o]
    consistency_intervals <- seq_len(nrow(yrep)) |>
      lapply(\(k) data.frame(
        y = Iso::pava(yrep[k, ep_o]),
        x_ = ep_s,
        id_ = ep_o
      )) |>
      bind_rows() |>
      group_by(id_) |>
      summarise(
        upper = quantile(y, .95),
        lower = quantile(y, .05),
        x_ = mean(x_)
      ) |>
      arrange(x_)
  } else {
    ceps <- Iso::pava(y[order(p)])
    x <- x[order(p)]
    consistency_intervals <- reliabilitydiag::reliabilitydiag(
      y = y,
      x = p,
      region.method = region.method,
      region.position = "diagonal",
      n.boot = n.boot
    )$x$regions |>
      select(lower, upper, x) |>
      rename(x_ = x)
  }
  ggplot(consistency_intervals[order(x), ]) +
    aes(
      x = sort(x),
      y = ceps[order(x)] - x_,
      ymin = lower - x_,
      ymax = upper - x_
    ) +
    geom_ribbon(
      alpha = fill_alpha,
      fill = color_scheme_get()$mid
    ) +
    geom_abline(slope = 0, intercept = 0, col = "black", lty = 2, alpha = .3) +
    geom_point(aes(colour = I(ifelse(ceps[order(x)] < lower | ceps[order(x)] > upper, cep_color, color_scheme_get()$mid))))
}

# Modified from bayesplot v1.11.1
ppc_error_binned <-
  function(y,
           yrep,
           ...,
           x = NULL,
           facet_args = list(),
           bins = NULL,
           size = NULL,
           alpha = 0.25,
           interval_type = "ridge") {
    require("ggplot2", quietly = TRUE)
    require("bayesplot", quietly = TRUE)
    bayesplot:::check_ignored_arguments(...)

    data <- ppc_error_binned_data(y, yrep, x, bins = bins)
    facet_layer <- if (nrow(yrep) == 1) {
      bayesplot:::geom_ignore()
    } else {
      facet_args[["facets"]] <- "rep_id"
      do.call("facet_wrap", facet_args)
    }

    mixed_scheme <- bayesplot:::is_mixed_scheme(color_scheme_get())
    point_fill <- bayesplot:::get_color(ifelse(mixed_scheme, "m", "d"))
    point_color <- bayesplot:::get_color(ifelse(mixed_scheme, "mh", "dh"))


    ggplot(data, aes(x = .data$ey_bar)) +
    geom_abline(slope = 0, intercept = 0, col = "black", lty = 2, alpha = .3) +
      switch(interval_type,
        "ribbon" = {
          geom_ribbon(
            mapping = aes(ymax = .data$se2, ymin = -.data$se2),
            alpha = alpha,
            fill = bayesplot:::get_color("l"),
            color = bayesplot:::get_color("l"),
            linewidth = size
          )
        },
        "errorbar" = {
          geom_errorbar(
            mapping = aes(ymax = .data$se2, ymin = -.data$se2),
            alpha = alpha,
            color = bayesplot:::get_color(ifelse(mixed_scheme, "m", "d")),
            linewidth = size,
            width = .02 * diff(range(data$ey_bar))
          )
        }
      ) +
      geom_point(
        mapping = aes(y = .data$err_bar),
        shape = 21,
        fill = point_fill,
        color = point_color
      ) +
      labs(
        x = if (is.null(x)) "Predicted proportion" else expression(italic(x)),
        y = "Average Error"
      ) +
      bayesplot_theme_get() +
      facet_layer +
      bayesplot:::force_axes_in_facets() +
      facet_text(FALSE)
  }

# Data for binned errors plots
# Modified from bayesplot v1.11.1
ppc_error_binned_data <- function(y, yrep, x = NULL, bins = NULL) {
  y <- bayesplot:::validate_y(y)
  x <- bayesplot:::validate_x(x, y)
  yrep <- bayesplot:::validate_predictions(yrep, length(y))

  if (is.null(bins)) {
    bins <- bayesplot:::n_bins(length(y))
  }

  errors <- bayesplot:::compute_errors(y, yrep)
  binned_errs <- list()
  for (s in seq_len(nrow(errors))) {
    binned_errs[[s]] <-
      bayesplot:::bin_errors(
        ey = if (is.null(x)) yrep[s, ] else x,
        r = errors[s, ],
        bins = bins,
        rep_id = s
      )
  }

  binned_errs <- dplyr::bind_rows(binned_errs)
  tibble::as_tibble(binned_errs)
}

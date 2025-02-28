#' PIT computation for histograms.
#'
#' @param hist_plot ggplot object containing a histogram
#' @param x Vector to transform. Typically the data used in the histogram.
#' @param bw bin width of the histogram.
#' @param layer_idx index of the layer in `hist_plot` containing the histogram geom.
#'
#' @return vector of PIT values
#'
#' @examples
#' library("ggplot2")
#' library("bayesplot")
#' x <- rnorm(1000)
#' p <- ggplot() + geom_histogram(aes(x = x, y = after_stat(density)),
#'                                binwidth = .1,
#'                                center = mean(range(x)))
#' pit <- pit_from_hist(p, x, bw = .1)
#'
#' # Show histogram and the PIT ECDF evaluating the goodness-of-fit of the histogram.
#' p
#' ppc_pit_ecdf(pit = pit)

pit_from_hist <- function(hist_plot, x, bw, layer_idx = 1) {
  ld <- ggplot2::layer_data(hist_plot, layer_idx)
  cdf <- c(0, bw * cumsum(ld$density))
  pit <- sapply(x, \(xi) {
    if (ld$xmin[1] >= xi) {
      pit_i <- 0
    } else if (max(ld$xmax) <= xi) {
      pit_i <- 1
    } else {
      # How many bars have been covered
      idx <- match(FALSE, ld$xmin <= xi) - 1
      pit_i <- cdf[idx] + (xi - ld$xmin[idx]) * ld$density[idx]
    }
    pit_i
  })
  pit
}

#' PIT computation for quantile dot plot.
#'
#' @param x Observations to plot in the quantile dot plot.
#' @param nq Number of quantile dot to evaluate.
#' @param bw Optional. Desired bin width of the quantile dot plot.
#' Defaults to bin width that makes total area of dots 1.
#'
#' @return vector of PIT values
#'
#' @examples
#' library("ggplot2")
#' library("ggdist")
#' library("bayesplot")
#'
#' x <- rnorm(500)
#' pit <- pit_qdotplot(x, 100)
#'
#' ggplot() +
#'   stat_dots(aes(x = x), quantiles = 100)
#' ppc_pit_ecdf(pit = pit)
#'
#' ## Compute PIT by the density estimator implied by the quantile dot plot.
pit_qdotplot <- function(x, nq, bw = NULL) {
  # Area of dots adds to one.
  if (is.null(bw)) {
    bw <- sqrt(1 / nq)
  }
  # Ggdist default dot placement for quantile dotplot with the given bw.
  dots <- ggdist::bin_dots(
    x = quantile(x, stats::ppoints(nq, a = 1 / 2), type = 5),
    y = 0,
    binwidth = bw
  )

  # PIT scores
  sapply(x, \(xi) {
    # Find first quantile (i.e. dot) with left edge larger than xi,
    # pit is then the index of the previous normalized by nq.
    # If larger than all left edges (no match), pit = 1.

    q_max <- match(
      FALSE, # Find first FALSE for below
      dots$x - 0.5 * bw <= xi, # left lim smaller than xi
      nq + 1 # If no match
    ) - 1 # go one index back

    q_min <- ifelse(
      q_max, # If q_max is non-zero.
      match(
        TRUE, # Find first TRUE
        (dots$x == dots$x[q_max]) & (dots$x + 0.5 * bw >= xi), q_max
      ), 0
    )
    # draw a pit between the smallest and largest quantile stacked on top of xi
    runif(1, q_min, q_max) / nq
  })
}

pit_from_densityplot <- function(p, i, x, ggdist_layer = FALSE, maxEval = 100, ...) {
  require("cubature", quietly = TRUE)
  ld <- ggplot2::layer_data(p, i = i)
  if (ggdist_layer == TRUE) {
    dens_fun <- approxfun(ld$x, y = ld$pdf, yleft = 0, yright = 0)
  } else {
    dens_fun <- approxfun(ld$x, y = ld$y, yleft = 0, yright = 0)
  }
    
  unlist(lapply(x, function(x_i) if (x_i > min(x)) cubature::cubintegrate(dens_fun , lower = -max(abs(x)), upper = x_i, maxEval = 200, ...)$integral else 0))
}

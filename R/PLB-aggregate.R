##' Aggregated bounded power-law distributions across multiple samples
##'
##' For data that are collected using different sampling techniques but are
##' desired to be aggregated, the measured sizes can be fitted separately
##' for each sample, using likelihood (see vignette - TODO make simpler one and
##' refer to here). For each sample, $s = 1, 2, 3, ..., S$, we then have a
##' fitted fitted exponent $b_s$, and minima and maxima of that sample (based on
##' the data).
##'
##' Here we construct the resulting probability density function, `dPLB_agg()`,
##' and cumulative distribution function, `pPLB_agg()`, based on math worked out
##' in `aggregatging-size-spectra.Rmd` (TODO for now, obviously update if
##' becomes part of a manuscript, or vignette). `pPLB_agg()` then feeds into
##' TODO `MLE.plot_agg()` to generate the plot -- the resulting fitted aggregate
##' distribution is different to the simple approach of fitting one PLB to the
##' full data set (which is not appropriate when the samples have different
##' sampling protocols, or are not comparable for other reasons).
##'
##' @param x vector of values to compute the density and distribution functions.
##' @param n_vec vector of known (or assumed) sample size for each sample,
##' element `s` corresponds to sample `s`. If generating random numbers using
##' `rPLB_agg()` TODO then probably should sample `sum(n_vec)` from the full
##' distribution; need to think about. Note for rPLB we had this, which we don't
##' want here: number of random numbers to be generated (if `length(n) > 1` then
##' generate `length(n)` values)
##' @param b_vec vector of exponents of the PLB distribution, one element for each sample
##' @param xmin_vec vector of minimum bounds of the distributions, each element
##'   is the assumed value for each sample, which will probably be defined (by
##'   the user) as the minimum body size of the data for each sample
##' @param xmax_vec as for `xmin_vec` but for maximum bounds
##' @return `dPLB_agg` returns a vector of probability density values
##' corresponding to `x`. `pPLB_agg` returns a vector of cumulative
##' distribution values P(X <= x) corresponding to `x`. `rPLB_agg` (TODO when
##'   written) will return a vector (TODO of length `sum(n_vec)`) of independent
##'   random draws from the full aggregated distribution.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' x <- 1:10000
##' y <- dPLB_agg(x,
##'   b_vec = c(-1, -2, -3, -4),
##'   n_vec = c(6000, 6000, 1600, 2000),
##'   xmin_vec = c(0.3, 10, 100, 500),
##'   xmax_vec = c(80, 800, 1000, 1500)
##' # Can do same example arguments for pPLB_agg()
##' }
##'
dPLB_agg <- function(x,
                     b_vec,
                     n_vec,
                     xmin_vec,
                     xmax_vec){
  expect_equal(rep(length(b_vec),
                   3),
               c(length(n_vec),
                 length(xmin_vec),
                 length(xmax_vec)))

  y <- 0 * x     # so have zeros where x < xmin or x > xmax

  for(s in 1:length(b_vec)){
    y <- y + n_vec[s] * dPLB(x,
                             b = b_vec[s],
                             xmin = xmin_vec[s],
                             xmax = xmax_vec[s])
  }
  # dPLB returns 0's for outside of the range, so don't need explicit indicator functions
  y <- y / sum(n_vec)
  return(y)
}

##' @rdname dPLB_agg
##' @export
pPLB_agg <- function(x,
                     b_vec,
                     n_vec,
                     xmin_vec,
                     xmax_vec){
  expect_equal(rep(length(b_vec),
                   3),
               c(length(n_vec),
                 length(xmin_vec),
                 length(xmax_vec)))

  y <- 0 * x     # so have zeros where x < xmin; TODO check get 1's further up

  for(s in 1:length(b_vec)){
    y <- y + n_vec[s] * pPLB(x,
                             b = b_vec[s],
                             xmin = xmin_vec[s],
                             xmax = xmax_vec[s])
  }
  # pPLB returns 0's and 1's for outside of the range, so don't need explicit indicator functions
  y <- y / sum(n_vec)
  return(y)
}

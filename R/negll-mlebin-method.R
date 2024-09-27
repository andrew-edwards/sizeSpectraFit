##' Calculate negative log-likelihood for the bounded power-law
##'   distribution given binned data, using the MLEbin method
##'
##' Calculates the negative log-likelihood of the parameters `b`, `x_min` and
##' `x_max` given binned data for the PLB model. Returns the negative
##' log-likelihood. Will be called by `calc_mle_conf()` via `fit_size_spectrum.data.frame()`, or similar, but `x_min` and `x_max`
##' will just be estimated as the minimum of lowest bin and maximum of the
##' largest bin, respectively, (since they are the MLEs), no need to do
##' numerically. Specifically this is the negative of the log-likelihood
##' function given in (A.70) of the MEE paper and (S.27) of the MEPS paper,
##' where the latter fixed a minor error in (A.75) of the MEE paper.
##'
##' @param b value of b for which to calculate the negative log-likelihood
##' @param w vector of length `J+1` giving the bin breaks `w_1, w_2, ..., w_{J+1}`
##' @param d vector of length `J` giving the count in each bin; must have `d_1,
##'   d_J > 0`
##' @param J number of bins (length of `d`)
##' @param x_min minimum value of bins, as an input to avoid repeatedly calculating
##' @param x_max maximum value of bins, as an input to avoid repeatedly calculating
##' @return negative log-likelihood of the parameters given the data
##' @export
##' @author Andrew Edwards
neg_ll_mlebin_method <- function(b,
                                x_min,
                                x_max,
                                w,
                                d,
                                J,
                                n){
  if(b != -1){
    neg_ll <- n * log( abs( w[J+1]^(b+1) - w[1]^(b+1) ) ) -
      sum( d * log( abs( w[-1]^(b+1) - w[-(J+1)]^(b+1) ) ) )
  } else {
    neg_ll <- n * log( log(w[J+1]) - log(w[1]) ) -
      sum( d * log( ( log(w[-1]) - log(w[-(J+1)]) ) ) )
  }
  return(neg_ll)
}

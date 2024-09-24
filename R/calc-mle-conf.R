##' Calculate maximum likelihood estimate and 95\% confidence
##'  interval
##'
##' Calculate maximum likelihood estimate of a parameter and its 95\% confidence
##'  interval using the profile log-likelihood method, for a given
##'  negative log-likelihood function and its arguments (other parameters and data).
##' TODO Will likely give warnings that can safely be ignored (see
##'  `suppress.warnings` description below). Decide what to do, maybe like for hdiAnalysis.
##'
##' @param neg_ll_fn negative log-likelihood function that take arguments
##'  (parameters and data) in ... and returns a negative
##'  log-likelihood value.
##' @param p starting point to calculate the maximum likelihood estimate
##' @param vec_diff the range over which to test the negative log-likelihood
##'  to construct the confidence interval. Default is 0.5 and a symmetric
##'  range is tested for fitting size spectra, since for movement data
##'  sets in Table 2 of Edwards (2011; 92(6):1247-1257) the intervals were
##'  symmetric, so symmetric seems a good start. TODO may need updating
##' @param vec_inc TODO increments to try, the accuracy of the resulting bounds
##'  will depend on this. Note that a resulting interval of, say,
##'  (-2.123, -1.987) means that that interval is contained within the
##'  true 95\% interval, which is itself contained within (-2.124, -1.986).
##'  The true bounds lie between the stated lower bounds and between
##'  the stated upper bounds. So reduce `vecInc` if further accuracy is needed.
##' @param suppress_warnings TODO decide. If TRUE then suppress warnings from the `nlm()`
##'   calculations; for the `MEPS_IBTS_MLEbins` vignette these occur a lot, and
##'   are always:
##'   `Warning in nlm(f = negll.fn, p = p, ...) :
##'    NA/Inf replaced by maximum positive value`. The same warning often happens in other
##'   situations also. It is due to the likelihood function blowing up,
##'   presumably when searching some very very very unlikely region of paramater space.
##' @param ... further arguments (including parameters and data) to `negll_fn()`
##' @return       list containing:
##'   * MLE: the maximum likelihood estimate
##'   * conf: the 95\% confidence interval of the MLE
##' @export
##' @author Andrew Edwards
calc_mle_conf <- function(this_neg_ll_fn,   # needed to avoid partial matching
                                        # with n
                          p,
                          vec_diff = 0.5,
                          vec_inc = 0.001,
                          suppress_warnings = FALSE,
                          b_vec,
                          b_vec_inc,
                          ...){

  # calcLike was:  TODO decide about the warnings
  if(suppress_warnings){
    min_neg_ll <- suppressWarnings(nlm(f = this_neg_ll_fn,
                                  p=p,
                                  ...))
  } else {
    min_neg_ll <- nlm(f = this_neg_ll_fn,
                      p = p,
                      ...)
  }

  mle = min_neg_ll$estimate

  if(is.null(b_vec)){
    b_vec <- seq(mle - vec_diff,
                 mle + vec_diff,
                 by = vec_inc)
  }

  b_conf <- calc_confidence_interval(this_neg_ll_fn = this_neg_ll_fn,
                                     min_neg_ll_value = min_neg_ll$minimum,
                                     # x = x,
                                     # n = n,
                                     # x_min = x_min,
                                     # x_max = x_max,
                                     # sum_log_x = sum_log_x,
                                     b_vec = b_vec,
                                     ...)

  # If confidence interval hits a bound then redo it over a larger range
  while(b_conf[1] == min(b_vec) | b_conf[2] == max(b_vec)){
    b_vec <- seq(min(b_vec) - 0.5,
                 max(b_vec) + 0.5,
                 b_vec_inc)

    b_conf <- calc_confidence_interval(this_neg_ll_fn = this_neg_ll_fn,  # TODO same as above
                                       min_neg_ll_value = min_neg_ll$minimum,
#                                       x = x,
#                                       n = n,
#                                       x_min = x_min,
#                                       x_max = x_max,
#                                       sum_log_x = sum_log_x,
                                       b_vec = b_vec,
                                       ...)
  }


#  conf = prof_like(negll_fn = negll_fn,
#                   mle = mle,
#                   min_negll = min_neg_ll$minimum,
#                   vec_diff = vec_diff,
#                   ...)

  res = list(mle = mle,
             conf = b_conf)   # TODO standardise, just have conf not b_conf
  return(res)
}

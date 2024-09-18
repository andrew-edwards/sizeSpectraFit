##' Calculate confidence interval
##'
##' Gets called from [fit_size_spectrum.numeric()]. TODO may need to be explicit
##' about the help. Might be simpler as fitting function gets more complex.
##'
##' @param min_ll list output from [nlm()].
##' @inheritParams fit_size_spectrum
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
calc_confidence_interval <- function(min_ll,
                                     x,
                                     n,
                                     x_min,
                                     x_max,
                                     sum_log_x,
                                     b_vec){
  # 95% confidence intervals
  min_neg_ll <- min_ll$minimum

  # Values of b to test to obtain confidence interval. For the real movement data
  #  sets in Table 2 of Edwards (2011) the intervals were symmetric, so make a
  #  symmetric interval here.

  ll_vals <- vector(length = length(b_vec))  # negative log-likelihood for bvec
  for(i in 1:length(b_vec)){
    ll_vals[i] <- negll_mle(b_vec[i],
                            x = x,
                            n = n,
                            x_min = x_min,
                            x_max = x_max,
                            sum_log_x = sum_log_x)
  }

  crit_val <- min_neg_ll  + qchisq(0.95,1)/2
                      # 1 degree of freedom, Hilborn and Mangel (1997) p162.
  b_in_95 <- b_vec[ll_vals < crit_val] # b values in 95% confidence interval
  b_conf = c(min(b_in_95), max(b_in_95))
  return(b_conf)
}

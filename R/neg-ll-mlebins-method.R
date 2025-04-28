##' Calculate negative log-likelihood of `b` for the bounded power-law
##'   distribution given binned data for multiple species, using the MLEbins method
##'
##' Calculates the negative log-likelihood of the parameters `b`, `x_min` and
##' `x_max` given binned data for the PLB model, where bins are
##' species-specific. This is
##'  the MLEbins method derived as equations (S.18) and (S.26) in MEPS paper.
##'  Returns the negative log-likelihood.
##' Will be called by `calc_mle_conf()` via
##' `fit_size_spectrum_mlebins()` which gets called from
##' `fit_size_spectrum.data.frame()`, but `x_min` and `x_max`
##' will just be estimated as the minimum of lowest bin and maximum of the
##' largest bin, respectively, (since they are the MLEs), no need to do
##' numerically. See Supplementary Material of MEPS paper for derivation, and
##'  the vignettes for example use.
##'
##' @param b value of b for which to calculate the negative log-likelihood
##' @param data_for_mlebins tibble where each row is the count (can be
##'   non-integer) in a bin
##'  of a species, and columns (and corresponding mathematical notation in MEPS
##'   Supplementary Material) are:
##'  * `species`: code for each species, `s`
##'  * `bin_min`: lower bound of the bin, `w_\{sj\}` where `j` is the bin number
##'  * `bin_max`: upper bound of the bin, `w_\{s, j+1\}`
##'  * `bin_count`: count in that bin for that species, `d_\{sj\}`
##'  For each species the first and last bins must be non-empty, i.e.
##'   `w_\{s1\}, w_\{s,J_s +1\} > 0`.   TODO add a check for that
##' @param n total number of counts `n = \sum_\{sj\} d_\{sj\}` over all `s` and `j`
##' @param xmin maximum likelihood estimate for `xmin`, `xmin = min_\{sj\}
##'   w_\{s, 1\}`, as an input to avoid repeatedly calculating.
##' @param xmax maximum likelihood estimate for `xmax`, `xmax = max_\{sj\}
##'   w_\{s, J_s+1\}`, as an input to avoid repeatedly calculating
##' @return  negative log-likelihood of the parameters given the data
##' @author Andrew Edwards
##' @export
##'
neg_ll_mlebins_method <- function(b,
                                  x_min,
                                  x_max,
                                  data_for_mlebins,
                                  n){
  if(b != -1){
    # From MEPS equation (S.18), first calculate each component in the
    # summations and then sum them:
    temp = dplyr::mutate(data_for_mlebins,
                         component = bin_count * log( abs( bin_max^(b+1) - bin_min^(b+1) ) ) )
    comp_sum = sum(temp$component)

    log_ll = - n * log( abs( x_max^(b+1) - x_min^(b+1) ) ) + comp_sum
    neg_ll = - log_ll      # Negative log-likelihood
  } else {
    # Not fully tested, but should work, equation (S.26)
    temp = dplyr::mutate(data_for_mlebins,
                         component = bin_count * log( log(bin_max) - log(bin_min) ) )
    comp_sum = sum(temp$component)

    log_ll = - n * log( log(x_max) - log(x_min) ) + comp_sum
    neg_ll = - log_ll      # Negative log-likelihood
  }

  return(neg_ll)
}

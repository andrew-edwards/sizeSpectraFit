##' Given data for MLEbins calculation, with species-specific weight bins,
##' determine a common binning and use that to calculate `x_min` and then fit
##' using MLEbins. TODO might also work for just MLEbin, need to test.
##'
##' @param dat_for_mlebins tibble of data in the format required for fitting
##'   using MLEbins method. TODO
##' @return list of class `determine_xmin_and_fit` for plotting, containing TODO
##'   see [determine_xmin_and_fit()] also
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'  TODO
##' }
determine_xmin_for_mlebins_and_fit <- function(dat_for_mlebins,
                                               bin_width = 1,
                                               bin_start = 0,
                                               ...){
  # Need to be pragmatic, since have overlapping bins. Since assuming a power
  # law, kind of expect counts to be shifted to the low end of the bin. Use this
  # thought to just create simple histograms of counts based on bin_min values.

  # Can just treat the bin_min values as components of a single vector
  hh <- make_hist(dat_for_mlebins$bin_min,
                  bin_width = bin_width,
                  bin_start = bin_start)

  x_min <- determine_xmin(hh)

  mlebins_fit <- fit_size_spectrum(dat_for_mlebins,
                                   x_min = x_min,
                                   ...)

  res <- list(mlebins_fit = mlebins_fit,
              h = hh)

  class(res) <- c("determine_xmin_for_mlebins_and_fit",  # TODO prob simplify
                  class(res))
  return(res)
}

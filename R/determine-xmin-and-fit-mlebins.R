##' Given data for MLEbins calculation, with species-specific weight bins,
##' determine a common binning and use that to calculate `x_min` and then fit
##' using MLEbins. TODO might also work for just MLEbin, need to test.
##'
##' @param dat_for_mlebins tibble of data in the format required for fitting
##'   using MLEbins method. TODO
##' @return list of class `determine_xmin_and_fit_mlebins` for plotting, containing TODO
##'   see [determine_xmin_and_fit()] also
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'  TODO
##' }
determine_xmin_and_fit_mlebins <- function(dat_for_mlebins,
                                           bin_width = 1,
                                           bin_start = 0,
                                           x_min = NULL,
                                           ...){
  # Need to be pragmatic, since have overlapping bins. Since assuming a power
  # law, kind of expect counts to be shifted to the low end of the bin. Use this
  # though to just create simple histograms of counts based on bin_min values.

  # Can't just treat the bin_min values as components of a single vector
  hh <- make_hist_for_binned_counts(dat_for_mlebins,
                                    bin_width = bin_width,
                                    bin_start = bin_start)

  if(is.null(x_min)){
    x_min <- determine_xmin(hh)
  }

  # Just set x_min to be the minimum value if it comes out as 0. TODO add to
  #  MLEbin function probably.
  if(x_min == 0){
    x_min = min(dat_for_mlebins$bin_min)
  }

  # Not determining x_max separately here so no need to mention it, it gets
  #  passed on in ...

  mlebins_fit <- fit_size_spectrum(dat_for_mlebins,
                                   x_min = x_min,
                                   ...)

  res <- list(mlebins_fit = mlebins_fit,
              h = hh)

  class(res) <- c("determine_xmin_and_fit_mlebins",
                  class(res))
  return(res)
}

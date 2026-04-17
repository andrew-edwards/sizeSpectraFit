##' Given data for MLEbin calculation, determine a common binning and use that
##' to calculate `x_min` and then fit using MLEbin.
##'
##' @param dat tibble of data in the format required for fitting
##'   using MLEbin method; see [fit_size_spectrum()].
##' @return list of class `determine_xmin_and_fit_mlebin` for plotting, containing TODO
##'   see [determine_xmin_and_fit()] also
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'  TODO
##' }
determine_xmin_and_fit_mlebin <- function(dat,
                                           bin_width = 1,
                                           bin_start = 0,
                                           x_min = NULL,
                                           ...){

  # Can't just treat the bin_min values as components of a single vector
  hh <- make_hist_for_binned_counts(dat,
                                    bin_width = bin_width,
                                    bin_start = bin_start)

  if(is.null(x_min)){
    x_min_based_on_hist <- determine_xmin_based_on_hist(hh)

    # Now set x_min to be the minimum value that is above x_min_based_on_hist,
    #  because the latter is based on histograms bin breaks (which are somewhat
    #  arbitrary, though likely integers). This will also work for the case
    #  where x_min_based_on_hist comes out as 0.
    x_min = min(dplyr::filter(dat,
                              bin_min >= x_min_based_on_hist)$bin_min)
  }

  # Not determining x_max separately here so no need to mention it, it gets
  #  passed on in ...

  mlebin_fit <- fit_size_spectrum_mlebin(dat,
                                         x_min = x_min,
                                         ...)

  res <- list(mlebin_fit = mlebin_fit,
              h = hh)

  class(res) <- c("determine_xmin_and_fit_mlebin",
                  class(res))
  return(res)
}

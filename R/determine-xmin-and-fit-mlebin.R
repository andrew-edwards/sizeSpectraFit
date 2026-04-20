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
                                           x_min = NULL,
                                           ...){

  if(dat$bin_min[-1] != dat$bin_max[-nrow(dat)]){
    stop("The tibble `dat` needs to have consecutive bins; you may need to add some zero counts for any intermediate bins. Given there are many choices of binning, it is hard for sizeSpectraFit to automatically fill in intermediate bins with counts of zero.")



1. Check bins are consecutive, if not return an error and say to insert
  0's in intermediate bins (bit hard to automate as need to know the specified bin breaks)
2. Need to make this hist like in make_hist_for_binned_counts, but shouldn't
  really need to calculate anything, so make a new shorter function. Including
  normalised counts that will matter when bin widths are not constant.


# TODO think we need a new function for this:
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

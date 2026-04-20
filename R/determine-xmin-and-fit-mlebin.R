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

  stopifnot("dat needs to include columns bin_min, bin_max, and bin_count to use the MLEbin method"
  = c("bin_min", "bin_max", "bin_count") %in% names(dat))

  if(!all(dat$bin_min[-1] == dat$bin_max[-nrow(dat)])){
    stop("The data.frame `dat` needs to have consecutive bins; you may need to add some zero counts for any intermediate bins. Given there are many choices of binning, it is hard for sizeSpectraFit to automatically fill in intermediate bins with counts of zero.")
  }

  hh <- make_hist_for_binned_counts_mlebin(dat)

  if(is.null(x_min)){
    x_min_based_on_hist <- determine_xmin_based_on_hist(hh)

    # This will automatically be an original bin break, so no need to make it anything slightly higher like for MLE or MLEbins
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

##' Given histogram object of counts and bins (and maybe density) , determine the mode and return min of that bin as xmin
##'
##' @param h histogram object, e.g. from running [make_hist()] on a vector, or
##' [make_hist_for_binned_counts()] or
##' [make_hist_for_binned_counts_mlebin()] on a data.frame for MLEbins or MLEbin
##' method, respectively.
##' @return x_min to use to fit ISD, and to plot the histogram to show the full
##'   data and the mode.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'  TODO
##' }
determine_xmin_based_on_hist <- function(h){
  # changed function name to determine_xmin_based_on_hist, since adapting in the
  # various calls to have xmin not be the bin break.  TODO remove that?
  stopifnot("h needs to be a histogram object" =
              class(h) == "histogram")

  max_ind <- ifelse(h$equidist,
                    which.max(h$counts),  # returns the first one if they are ties
                    which.max(h$density)) # for unequal bin widths

  # TODO I'd commented this out, I guess because it might still fit, but the
  # resulting plots should clearly show the issue. Make a test to see if still fits.
  # if(max_ind == length(h$counts)){
  #   stop("The maximum count is in the final bin, so a descending PLB is not appropriate.")
  # }     # Also the fitting code will likely break.

  x_min <- h$breaks[max_ind]

  x_min
}

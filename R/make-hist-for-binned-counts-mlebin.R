##' Take a data.frame to be used for the MLEbin method and create the histogram
##' object to be used to determine `x_min`
##'
##' Called from [determine_xmin_and_fit_mlebin()], adapting from
##' [make_hist_for_binned_counts()]. This version should be simpler, as we are
##' not making up new bins (had to for MLEbins data because the bins overlap),
##' just using those specified by the user.
##'
##' Gives values as a histogram list object.
##' Can then use `plot()` which calls `plot.histogram()`.
##'
##' @param dat tibble of data in the format required for fitting
##'   using MLEbin or MLEbins methods; i.e. at a minimum has to include the columns:
##'     * `bin_min`
##'     * `bin_max`
##'     * `bin_count`.
##' @return a histogram list object with components (see `?hist`):
##'  - `breaks`
##'  - `mids`
##'  - `counts`
##'  - `xname`
##'  - `equidist` will be TRUE or FALSE depending on whether the bins in `dat`
##'     have equal bin widths
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODO counts_per_bin_example
##' make_hist(counts_per_bin_example)
##' TODO for test do
##' }
make_hist_for_binned_counts_mlebin <- function(dat){

  # determine_xmin_and_fit_mlebin() already checks required column names and
  #  that bins are consecutive
  # Here just need to convert information in `dat` into a histogram object

  num_bins <- nrow(dat)
  hist_mids <- (dat$bin_min + dat$bin_max)/2

  bin_breaks <- c(dat$bin_min,
                   dat$bin_max[num_bins])

  bin_widths <- diff(bin_breaks)

  # logical for whether the widths are equal
  equal_breaks <- isTRUE(all.equal(bin_widths,
                                   rep(bin_widths[1],
                                       length(bin_widths))))
  # all(x == x[1]) likely faster, but might get issues with floating point.

  hist_density <- dat$bin_count / bin_widths

  hist_name <- ifelse(equal_breaks,
                      "Total counts in each bin",
                      "Normalised counts in each bin")

  # From sizeSpectra::fitting() to do with LBNbiom method.
  # Don't think need this, but had though might for MLEbin if the function needs
  # adapting, so keep. Think it was almost like a resonance effect.
  #                      eps = 0.0000001){   # This was an extra argument for something

  # Check the bin widths are compatible with (i.e. multiples of) bin_width
  # Problem is due to floating point resolution, the remainder might be
  # 0.9999*bin_width. So can't just look at the absolute remainder.

  #   bin_diffs_remainder <- diff(counts_per_bin$binMid) %% bin_width

  # That gives some just above 0, and some just below bin_width
  # (if bin_width = 1 don't think this happens).

  #  bin_diffs_remainder_not_zero <- which(bin_diffs_remainder > eps &
  #                                        bin_diffs_remainder < bin_width - eps)

  #  if(length(bin_diffs_remainder_not_zero) != 0){
  #    stop("Need counts_per_bin$binMid to all be multiples of bin_width")
  #  }

  #  all_bins <- tibble::tibble(binMid = seq(min(counts_per_bin$binMid),
  #                                          max(counts_per_bin$binMid),
  #                                          bin_width))

  hist_res_list <- list(breaks = bin_breaks,
                   mids = hist_mids,
                   counts = dat$bin_count,
                   density = hist_density, # but not going to use for equal_breaks
                   xname = hist_name,
                   equidist = equal_breaks)

  class(hist_res_list) <- "histogram"

  hist_res_list
}

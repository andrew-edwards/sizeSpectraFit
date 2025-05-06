##' Take a tibble with `bin_min`, `bin_max`, and `bin_count` and assign counts
##' to new equal bins based on `bin_min`, summing the original counts (which can
##' be non-integer)
##'
##' Called from `determine_xmin_and_fit_mlebins()` to determine xmin for MLEbins
##' method, and should work for MLEbin
##' method also TODO. Adapting from `sizeSpectra2::make_hist()` and
##' sizeSpectraHake::make_hist()`, think the first was adapted from the former,
##' for simple counts, but now we want the more complex latter version for
##' binned data.
##'
##' Makes results a histogram list object and create 0 counts for missing bins.
##'
##' Can then use `plot()` which calls `plot.histogram()`. Without the 0 counts for missing bins
##' `plot.histogram()` does not plot counts because bins appear to have unequal widths.
##'
##' @param dat_for_mlebins  tibble of data in the format required for fitting
##'   using MLEbins method. TODO can join up with `determine_xmin_and_fit_mlebins()`
##' @param bin_width numeric bin width to fit a histogram to help determine xmin
##' @param bin_start numeric value for the first bin to start at; if `NULL` then
##'   is set to the highest multiple of `bin_width` value below `min(dat_for_mlebins$bin_min)`.
##' @return a histogram list object with components (see `?hist`):
##'  - `breaks`
##'  - `mids`
##'  - `counts`
##'  - `xname`
##'  - `equidist` TRUE since have equal bin widths
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' TODO counts_per_bin_example
##' make_hist(counts_per_bin_example)
##' TODO for test do
##' }
make_hist_for_binned_counts <- function(dat_for_mlebins,
                                        bin_width = 1,
                                        bin_start = NULL){

  if(is.null(bin_start)){
    bin_start <- min(dat_for_mlebins$bin_min) -
      min(dat_for_mlebins$bin_min) %% bin_width
  }

  # Vector of histogram bin breaks that we want to ascribe data to:
  hist_breaks <- seq(from = bin_start,
                     to = max(dat_for_mlebins$bin_min) + bin_width,
                     by = bin_width)
  hist_bin_min <- hist_breaks[-length(hist_breaks)]

  # Work out the hist_break for each data bin_min, naming the factors using the
  # left-end of each hist_breaks bin. Vector in the order of the data
  dat_in_which_hist_bin <- cut(x = dat_for_mlebins$bin_min,
                               breaks = hist_breaks,
                               # labels = hist_bin_min,
                               right = FALSE,
                               include.lowest = TRUE)

  # as.numeric on the levels messes up. Using this from ?cut to make them
  #  correct bin minima
  dat_in_which_hist_bin <- as.numeric( sub("\\[(.+),.*", "\\1",
                                           dat_in_which_hist_bin))

  dat_ascribed_to_hist_bins <- tibble::add_column(dat_for_mlebins,
                                                  hist_bin_min =
                                                    dat_in_which_hist_bin)


  hist_bin_totals <- dplyr::summarise(dplyr::group_by(dat_ascribed_to_hist_bins,
                                                      hist_bin_min),
                                      total_count = sum(bin_count)) %>%
    dplyr::mutate(hist_bin_min = as.numeric(hist_bin_min))

  # For histogram plotting need values in all hist bins, so create tibble to join
  hist_bin_all_bins <- tibble::tibble(hist_bin_min = hist_bin_min) %>%
    dplyr::left_join(hist_bin_totals,
                     by = "hist_bin_min") %>%
    tidyr::replace_na(list(total_count = 0))

  # From sizeSpectra::fitting() to do with LBNbiom method.
  # Don't think need this, but might for MLEbin  if the function needs
  # adapting. Think it was almost like a resonance effect. TODO
  #                      eps = 0.0000001){

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


  # Don't think it matters if first or final bins have zero counts (latter
  # should probably not happen anyway given how bins are constructed, though
  # might do in edge cases). This is just for determining xmin, not actually fitting.

  hist_res_list <- list(breaks = hist_breaks,
                   mids = hist_bin_min + bin_width/2,
                   counts = hist_bin_all_bins$total_count,
                   xname = "Total counts in each bin",
                   equidist = TRUE)

  class(hist_res_list) <- "histogram"

  hist_res_list
}

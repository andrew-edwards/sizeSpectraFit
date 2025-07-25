##' Construct bins that either double in size or are of equal width, and encompass
##'  the data
##'
##' Takes either a `numeric` vector of values (e.g. body masses) or a `data.frame`
##' of counts of values, and bins the values or counts into bins. In particular
##' needed for
##' MLEbin method (TODO function is ??) and the goodness-of-fit tests.
##' The counts can take non-integer values, which can occur when standardising field
##' measurements.
##'
##' Constructs bins that start from `floor(min(x))` or `min(x)` and either double
##' in size or are of equal width, and encompass the data. User must specify
##' either `bin_width` or
##' `bin_breaks`.
##'
##' Bins are defined intervals `[w_j, w_{j+1})`, which means values
##' `x` in the interval satisfy `w_j <= x < w_{j+1}`, i.e. closed on the left
##' and open on the right.  The exception being for
##' the highest bin `[w_{J-1}, w_J]` which includes both bin breaks; see p11 of
##' Appendix of MEE paper for more details. Functions are adapted from
##' `sizeSpectra::binData()`.
##'
##' @param dat numeric vector of individual values (e.g. body masses) OR a
##'   data.frame with first column `x` being the measured values
##'  (e.g. body masses or lengths), and second column `counts` being the counts of the
##'  number of individuals for that value. The `counts` column can have
##'   non-integer values. The corresponding function `bin_data.numeric()` or
##'   `bin_data.data.frame()` then gets used.
##' @param bin_width type of bins to use:
##'   * `"2k"` will result in `bin_breaks` that:
##'     + with `start_integer=TRUE` are powers of 2, i.e. ..., 0.25, 0.5, 1, 2, 4, 8, 16,....
##'     + with `start_integer=FALSE` are bins that double in size and  start with
##'       `min(x)`; not yet implemented, since have to think about what the width of
##'       the first bin should be.
##'   * numeric value (call it `a`) will result in bin_breaks are separated by `a` and span the
##'       data, that:
##'     + with `start_integer=TRUE` start from `z = floor(min(x))` and are then
##'          `z, z+a, z+2a, z+3a, ....`   (if `z = 0` then power-law cannot be fit
##'        so then need to use `start_integer=FALSE`)
##'     + with `start_integer=FALSE` start from `z = min(x)` and are then
##'           `z, z+a, z+2a, z+3a, ....`
##'   * exactly one of `bin_width` or `bin_breaks` must be specified.
##' @param bin_breaks pre-defined bin breaks as a vector. Exactly one of  `bin_width`
##'   or `bin_breaks` must be specified.
##' @param start_integer TRUE or FALSE, whether to start the bin breaks at an integer
##'   power of 2 (for method `"2k"`) or an integer. See `bin_width` above.
##'   `start_integer` is ignored if `bin_breaks` is specified.
##' @param truncate_top_bin TRUE of FALSE, whether to truncate the top bin at
##'   the max of the data, or let it have a maximum value based on the
##'   bin-generating definition. Usually have it FALSE, but using TRUE for doing
##'   LBN plot when we have the raw individual data.
##' @return list containing:
##'   * bin_for_each_x: tibble with a row for each `counts_df$x` value, with columns:
##'      + `x`: original `x` or `counts_df$x` values (depending on the input)
##'      + `bin_mid`, `bin_min`, `bin_max`, `bin_width`: midpoint, minimum,
##'      maximum, and width, respectively, of the bin within
##'      which the `x` value falls.  If bin_for_each_x has `>=10^6` rows then it isn't
##'      saved.
##'   * bin_vals: tibble with a row for each new bin and columns:
##'      + `bin_mid`, `bin_min`, `bin_max`, `bin_width`: midpoint, minimum,
##'         maximum, and width, respectively, of the bin
##'      + `bin_count`: total count of numbers of individuals in that bin
##'      + `bin_count_norm`: normalised bin count, `bin_count / bin_width`
##'      + `bin_sum`: sum of numbers of individuals * x values in that bin
##'   (appropriate if `x` represents biomass, but not length)
##'      + `bin_sum_norm`: `bin_sum / bin_width`
##'      + `log10....` - `log10()` of some of the above quantities
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' x <- c(1:5, 3:7, 7, 7, 7)
##' bin_data(x, bin_width = 2)
##'
##' bin_data(sim_vec, bin_width = "2k")
##'
##' counts_df <- tibble::tibble(x = as.numeric(1:50), counts = rep(c(0.19, 27.05, 9, 3.1, 0.001), 10))
##' bin_data(counts_df, bin_width = 6)
##' }
bin_data <- function(dat,
                     ...){
  UseMethod("bin_data")
}

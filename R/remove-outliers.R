##' Remove outliers (large values with gaps from remaining continuous values)
##' from MLEbins (TODO others to come) results object
##'
##' @param res One of:
##' * `sizespectrum_mlebins` object.
##' @param number numeric value for how many of the top measurements to remove;
##' calculate it manually for now
##' @return list containing two tibbles plus two numerics. Each tibble contains just the data values needed for
##' calculations, which are `species`, `bin_min`, `bin_max`, and
##' `bin_count`. `count_gte_bin_min` etc. will be recalculated in the new
##' analysis. They are:
##'  * `dat-keep` records that are being kept
##'  * `dat-removed` records that are removed
##' Numeric values are:
##'  * `bin_count_removed` sum of the counts in bins that have been removed
##'  * `bin_count_removed_prop` proportion of the counts in bins that have been
##' removed (total counts removed divided by total counts in original data set).
##'
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' remove_outliers(**)
##' }
remove_outliers <- function(dat,
                            ...){
  UseMethod("remove_outliers")
}

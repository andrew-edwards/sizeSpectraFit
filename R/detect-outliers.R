##' Analyse gaps to look for outliers (large values with gaps from remaining
##' continuous values) from MLEbins (TODO others to come) results object
##'
##' If already run through remove_outliers.size_spectrum_mlebins then it wil
##' use `detect_outliers.remove_outliers.mlebins()` and can keep iterating that.
##' @param res One of:
##' * `sizespectrum_mlebins` object.
##' ##' @return tibble
##'
##' `gap_ratio` is gap divided by second largest gapTODO list containing two tibbles plus two numerics. Each tibble contains just the data values needed for
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
##' detect_outliers(todo)
##' }
detect_outliers <- function(dat,
                            ...){
  UseMethod("detect_outliers")
}

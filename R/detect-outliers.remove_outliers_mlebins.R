##' @rdname detect-outliers
##' @export
detect_outliers.remove_outliers_mlebins <- function(res){

  dat_to_use <- list()
  dat_to_use$dat <- res$dat_keep

  # Can then use remove_outliers.size_spectrum_mlebins as only
  #  need the dat object (since wrote that first)
  ret <- detect_outliers.size_spectrum_mlebins(dat_to_use)

  return(ret)
}

##' @rdname remove-outliers
##' @export
remove_outliers.determine_xmin_and_fit_mlebins <- function(res,
                                                           ...){

  res_fit_only <- res$mlebins_fit

  return(remove_outliers.size_spectrum_mlebins(res_fit_only,
                                               ...))
}

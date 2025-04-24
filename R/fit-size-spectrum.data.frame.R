##' @rdname fit_size_spectrum
##' @export
fit_size_spectrum.data.frame <- function(dat,
                                         strata = NULL,
                                         ...){
#                                         x_min = NULL,
#                                         x_max = NULL,
#                                         b_vec = NULL,
#                                         b_vec_inc = 0.00001){

  # TODO could check if bin_min is in column names, in which case treat as
  # definitely needing mlebin - may want to do mlebin and each strata/year
  # though. Be nice to make strata = "year" the default.

  if(!is.null(strata)){
    # Individual measurements, x, for a strata, do MLE for each strata
    # separately and then combine results.
    res <- fit_size_spectrum_mle_strata(dat,
                                        strata = strata,
                                        ...)  # TODO set up a loop, or the function.
  } else {
    res <- fit_size_spectrum_mlebin(dat,
                                    ...)
  }

  return(res)
}

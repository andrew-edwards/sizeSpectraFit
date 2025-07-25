##' @rdname fit_size_spectrum
##' @export
fit_size_spectrum.data.frame <- function(dat,
                                         strata = NULL,
                                         ...){
#                                         x_min = NULL,
#                                         x_max = NULL,
#                                         b_vec = NULL,
#                                         b_vec_inc = 0.00001){
  # TODO, make fit_size_spectrum_strata  which then loops round the existing ones
  # TODO May need to get people to specify method, bit
  # TODO hard to fully automate, and no guarantee what other column names people have.
  # TODO could check if bin_min is in column names, in which case treat as
  # definitely needing mlebin - may want to do mlebin and each strata/year
  # though. Be nice to make strata = "year" the default.
  # TODO emphasise in help that species in name means that we're doing
  # MLEbins. Could do a check to see if MLEbins on data without species-specific
  # bins gives the same asnwer as MLEbin - TODO presumably it should, so try it.
  if(!is.null(strata)){
    # Individual measurements, x, for a strata, do MLE for each strata
    # separately and then combine results.
    res <- fit_size_spectrum_mle_strata(dat,
                                        strata = strata,
                                        ...)  # TODO set up a loop, or the function.
  } else {
    if(!("species" %in% names(dat))){   # TODO  risky approach
      # MLEbin
      res <- fit_size_spectrum_mlebin(dat,
                                      ...)
    } else {
      # MLEbins
      res <- fit_size_spectrum_mlebins(dat,
                                      ...)
    }
  }
  return(res)
}

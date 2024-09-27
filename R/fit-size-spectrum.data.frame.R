##' @rdname fit_size_spectrum
##' @export
fit_size_spectrum.data.frame <- function(dat,
                                         strata = NULL,
                                         ...){
#                                         x_min = NULL,
#                                         x_max = NULL,
#                                         b_vec = NULL,
#                                         b_vec_inc = 0.00001){

  if(!is.null(strata)){
    # then do each strata separately and then combine. Still need to pick a
    # method TODO. May want user to specify, or maybe not. Need mlebin done first
    res <- fit_size_spectrum.mle.strata(dat,
                                    strata = strata,
                                    ...)  # TODO
  } else {
    res <- fit_size_spectrum.mlebin(dat,
                                    ...)
  }

  return(res)
}

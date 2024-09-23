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


##' @rdname fit_size_spectrum
##' @export
fit_size_spectrum.mlebin <- function(dat,
                                     x_min = NULL,
                                     x_max = NULL,
                                     b_vec = NULL,
                                     b_vec_inc = 0.00001){

  # Fitting MLEbin method. Need dat
  stopifnot("dat needs to include columns bin_min, bin_max, and bin_count to use the MLEbin method") = c("bin_min, bin_max, bin_count" %in% names(dat))

  df <- tibble::as_tibble(dat)     # df is tibble to be fitted, can get
                                   # restricted in next lines

  # TODO might have to take out 0 counts in end bins
  if(!is.null(x_min)){
    df <- dplyr::filter(df,
                        bin_min >= x_min)
  } else {
    x_min <- min(bin_min)
  }


  if(!is.null(x_max)){
    df <- dplyr::filter(df,
                        bin_max <= x_max)
  } else {
    x_max <- max(bin_max)
  }

  # n <- length(x)

  stopifnot("Need x_min < x_max (if not NULL)" =
              x_min < x_max)



  HERE

  This was from sizeSpectra: need to [see .Rmd for next steps]

    MLEbin.res <- calcLike(negLL.PLB.binned,
                           p = p,
                           w = c(bins_this_seg$wmin,
                                 max(bins_this_seg$wmax)),
                           d = bins_this_seg$binCount,
                           vecDiff = 1,             # increase this if hit a bound
                           vecInc = 1e-10)



# This was MLE in sizeSpectra2:
  sum_log_x  <- sum(log(x))

  # Use analytical value of MLE b for PL model (Box 1, Edwards et al. 2007)
  #  as a starting point for nlm for MLE of b for PLB model. Everything after
  #  this is for PLB.
  pl_b_mle = 1/( log(x_min) - sum_log_x/length(x)) - 1

  min_ll <- nlm(negll_mle,
                p = pl_b_mle,
                x = x,
                n = n,
                x_min = x_min,
                x_max = x_max,
                sum_log_x = sum_log_x)

  b_mle <- min_ll$estimate

  if(is.null(b_vec)){
    b_vec <- seq(b_mle - 0.5,
                 b_mle + 0.5,
                 b_vec_inc)
  }

  b_conf <- calc_confidence_interval(min_ll = min_ll,
                                     x = x,
                                     n = n,
                                     x_min = x_min,
                                     x_max = x_max,
                                     sum_log_x = sum_log_x,
                                     b_vec = b_vec)

  # If confidence interval hits a bound then redo it over a larger range
  while(b_conf[1] == min(b_vec) | b_conf[2] == max(b_vec)){
    b_vec <- seq(min(b_vec) - 0.5,
                 max(b_vec) + 0.5,
                 b_vec_inc)

    b_conf <- calc_confidence_interval(min_ll = min_ll,
                                       x = x,
                                       n = n,
                                       x_min = x_min,
                                       x_max = x_max,
                                       sum_log_x = sum_log_x,
                                       b_vec = b_vec)
  }

  res <- list(b_mle = b_mle,
              b_conf = b_conf,
              x = x)

  class(res) = c("size_spectrum_numeric",
                 class(res))

  return(res)

}

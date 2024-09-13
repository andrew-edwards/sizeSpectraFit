##' @rdname fit_size_spectrum
##' @export
fit_size_spectrum.numeric <- function(dat,
                                      x_min = NULL,
                                      x_max = NULL,
                                      b_vec = NULL,
                                      b_vec_inc = 0.00001){
  x <- dat     # x will be the data to be fitted, might get restricted in next lines
  if(!is.null(x_min)){
    x <- x[x >= x_min]
  } else {
    x_min <- min(x)
  }

  if(!is.null(x_max)){
    x <- x[x <= x_max]
  } else {
    x_max <- max(x)
  }

  n <- length(x)

  stopifnot("Need x_min < x_max (if not NULL) and at least TODO values between them" =
              x_min < x_max & n >= 2)

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

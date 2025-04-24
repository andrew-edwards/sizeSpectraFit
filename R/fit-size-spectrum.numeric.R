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
  pl_b_mle <- 1/( log(x_min) - sum_log_x/length(x)) - 1


  mle_and_conf <- calc_mle_conf(this_neg_ll_fn = neg_ll_mle_method,
                                p = pl_b_mle,
                                vec = b_vec,
                                vec_inc = b_vec_inc,
                                x = x,
                                n = n,
                                x_min = x_min,
                                x_max = x_max,
                                sum_log_x = sum_log_x)

  res <- list(b_mle = mle_and_conf$mle,
              b_conf = mle_and_conf$conf,
              x = x,
              x_min = x_min,
              x_max = x_max,
              method = "MLE")

  class(res) = c("size_spectrum_numeric",
                 class(res))

  return(res)

}

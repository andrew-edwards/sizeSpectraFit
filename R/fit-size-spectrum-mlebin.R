##' @rdname fit_size_spectrum
##' @export
fit_size_spectrum_mlebin <- function(dat,
                                     x_min = NULL,
                                     x_max = NULL,
                                     b_start = -1.9,   # Starting estimate for
                                        # b, since no analytical value
                                     b_vec = NULL,
                                     b_vec_inc = 0.00001){

  # Fitting MLEbin method. Need dat
  stopifnot("dat needs to include columns bin_min, bin_max, and bin_count to use the MLEbin method" = c("bin_min", "bin_max", "bin_count") %in% names(dat))

  df <- tibble::as_tibble(dat)     # df is tibble to be fitted, can get
                                   # restricted in next lines
  df <- dplyr::arrange(df,
                       bin_min)

  # TODO might have to take out 0 counts in end bins. Check here or reduce the
  # bins. And add to help.
  if(!is.null(x_min)){
    df <- dplyr::filter(df,
                        bin_min >= x_min)
  } else {
    x_min <- min(df$bin_min)
  }

  if(!is.null(x_max)){
    df <- dplyr::filter(df,
                        bin_max <= x_max)
  } else {
    x_max <- max(df$bin_max)
  }

  stopifnot("Need x_min < x_max (if not NULL)" =
              x_min < x_max)

  # To match equations:
  w <- c(df$bin_min,
         max(df$bin_max))

  d <- df$bin_count
  J <- length(d)             # Number of bins
  n <- sum(d)     # TODO check if can be non-integer, think maybe. Check GoF stuff.

  if(x_min <= 0 | x_min >= x_max | d[1] == 0 | d[J] == 0 | min(d) < 0){
    stop("Parameters out of bounds in fit_size_spectrum.data.frame()")
  }

  mle_and_conf <- calc_mle_conf(this_neg_ll_fn = neg_ll_mlebin_method,
                                p = b_start,
                                vec = b_vec,
                                vec_inc = b_vec_inc,
                                x_min = x_min,
                                x_max = x_max,
                                w = w,
                                d = d,
                                J = J,
                                n = n)

  # Need for plotting rectangles in ISD type plots, so calculate here to be able
  # to extract values and to simplify passing through functions.

    # For non-overlapping in sizeSpectra highCount and countGWEwmin will be the same since only one set of
    # bin breaks; but did them both to save adapting ISD_bin_plot(). So might need lowCount and highCount for overlapping bins, but just do nonoverlapping for now.

  # count_gte_bin_min is, for a given bin, the total counts >= than that bin's minimum.

  df$count_gte_bin_min <- rep(NA, length = J)
  df$low_count <- count_gte_bin_min
  df$high_count <- count_gte_bin_min

  res <- list(b_mle = mle_and_conf$mle,
              b_conf = mle_and_conf$conf,
              data = df)    # TODO mention can be different to dat, document,
                            # including low_count etc.

  class(res) = c("size_spectrum_mlebin",
                 class(res))

  return(res)
}

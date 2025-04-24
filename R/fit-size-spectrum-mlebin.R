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

  # To match equations, especially for internal tricky computations, but stick
  # with bin_min and bin_max for user code. TODO though I replace wmin and wmax
  # maybe with bin_min and bin_max, but not sure if the former was actually a
  # thing. w is actually all the bin breaks.
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

  count_gte_bin_min <- rep(NA, length = J)
  low_count <- count_gte_bin_min
  high_count <- count_gte_bin_min

  # yRange = c(min(data_year$lowCount), max(data_year$highCount))
  # The above does not work because first val is 0 which is not permissable on
  #  log axis_ Which also means that the rectangle that goes to 0 has to be
  #  added manually (below)_ Picking the y-axis to go down to 0_75 of the
  #  minimum value of CountGTEwmin_
  for(iii in 1:length(count_gte_bin_min)){
    count_gte_bin_min[iii] <- sum( (dat$bin_min >= dat$bin_min[iii]) * dat$bin_count)
    low_count[iii] <- sum( (dat$bin_min >= dat$bin_max[iii]) * dat$bin_count)
    high_count[iii] <- sum( (dat$bin_max > dat$bin_min[iii]) * dat$bin_count)
    # TODO understand high_count again
  }

  df$count_gte_bin_min <- count_gte_bin_min
  df$low_count <- low_count
  df$high_count <- high_count

  res <- list(b_mle = mle_and_conf$mle,
              b_conf = mle_and_conf$conf,
              data = df,
              x_min = x_min,
              x_max = x_max,
              method = "MLEbin")
          # TODO mention data can be different to dat, document,
                            # including low_count etc.

  class(res) = c("size_spectrum_mlebin",
                 class(res))

  return(res)

}

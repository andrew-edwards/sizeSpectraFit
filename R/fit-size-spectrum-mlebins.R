##' @rdname fit_size_spectrum
##' @export
fit_size_spectrum_mlebins <- function(dat,
                                      x_min = NULL,
                                      x_max = NULL,
                                      b_start = -1.9,   # Starting estimate for
                                      # b, since no analytical value
                                      b_vec = NULL,
                                      b_vec_inc = 0.00001){

  # Need to amalgamate; see TODO in determine_smin_and_fit_mlebins.

  # TODO in help note that bin_count can be non-integer. And need species column
  # since species-specific bins, though don't need to know what these are now,
  # and don't actually need them, so not a requirement.

  # Fitting MLEbins method. Need dat
  stopifnot("dat needs to include columns bin_min, bin_max, and bin_count to use the MLEbins method" = c("bin_min", "bin_max", "bin_count") %in% names(dat))

  df <- tibble::as_tibble(dat)     # df is tibble to be fitted, can get
                                   # restricted in next lines

  # TODO here combine repeated ones and arrange; do in mlebin function also.


  df <- dplyr::arrange(df,
                       bin_min)
                       # species,    # don't want to do that as it messes up
                       # plotting, and kind of don't care about specific species
                       # as have already dealt with the species-specific aspects


  # TODO might have to take out 0 counts in end bins. Check here or reduce the
  # bins. And add to help. SEe also mlebin. Though don't think will have any 0
  # counts

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

  if(x_min <= 0 | x_min >= x_max){
    stop("Parameters out of bounds in fit_size_spectrum.data.frame() for MLEbins method")
  }


# Had for MLEbin but dont want for MLEbins, can delete once happy.
#  w <- c(df$bin_min,
#         max(df$bin_max))

#  d <- df$bin_count
#  J <- length(d)             # Number of bins
  n <- sum(df$bin_count)

  mle_and_conf <- calc_mle_conf(this_neg_ll_fn = neg_ll_mlebins_method,
                                p = b_start,
                                vec = b_vec,
                                vec_inc = b_vec_inc,
                                x_min = x_min,
                                x_max = x_max,
                                data_for_mlebins = df,
                                # w = w,
                                # d = d,
                                # J = J,
                                n = n)

  # Need for plotting rectangles in ISD type plots, so calculate here to be able
  # to extract values and to simplify passing through functions.

    # For non-overlapping in sizeSpectra highCount and countGWEwmin will be the same since only one set of
    # bin breaks; but did them both to save adapting ISD_bin_plot(). So might
    # need lowCount and highCount for overlapping bins, but just do
    # nonoverlapping for now. TODO

  # count_gte_bin_min is, for a given bin, the total counts >= than that bin's minimum.


  # Can't do these in dplyr, but then cbind later:
  count_gte_bin_min <- rep(NA, length = nrow(df))
  low_count <- count_gte_bin_min
  high_count <- count_gte_bin_min

  # yRange = c(min(data_year$lowCount), max(data_year$highCount))
  # The above does not work because first val is 0 which is not permissable on
  #  log axis_ Which also means that the rectangle that goes to 0 has to be
  #  added manually (below)_ Picking the y-axis to go down to 0_75 of the
  #  minimum value of CountGTEwmin_   TODO can prob delete that if dealt with in
  # plotting functions, though prob best to do here to then not have to calc in plots.
  for(iii in 1:length(count_gte_bin_min)){
    count_gte_bin_min[iii] <- sum( (df$bin_min >= df$bin_min[iii]) * df$bin_count)
    low_count[iii] <- sum( (df$bin_min >= df$bin_max[iii]) * df$bin_count)
    high_count[iii] <- sum( (df$bin_max > df$bin_min[iii]) * df$bin_count)
  }

  df$count_gte_bin_min <- count_gte_bin_min
  df$low_count <- low_count
  df$high_count <- high_count

  res <- list(b_mle = mle_and_conf$mle,
              b_conf = mle_and_conf$conf,
              data = df,
              x_min = x_min,
              x_max = x_max,
              method = "MLEbins")
          # TODO mention data can be different to dat, document,
                            # including low_count etc. Maybe call it something else?

  class(res) = c("size_spectrum_mlebins",
                 class(res))

  return(res)

}

##' @rdname p_biomass_bins
##' @param ... arguments to pass onto bin_data at least
##' @export
##' TODO dont think this is needed: numeric version could create a data.frame first like bin_data.numeric does,
##'   then use p_biomass_bins.data.frame. Something like:
##'   ifelse(!is.null(binValsTibble),
##'         binTibble <- binValsTibble,
##'      # Create tibble from the vector binBreaks:
##'         binTibble <- dplyr::tibble(wmin = binBreaks[-length(binBreaks)],
##'                                    wmax = binBreaks[-1])
##'         )
p_biomass_bins.size_spectrum_numeric <- function(res_mle   # result from MLE method
                                                 ){

  # TODO put some checks in, though given we have the class defined it should be
  # good

      # Need to create bins manually
  data <- bin_data(res_mle$x,
                   bin_width = "2k")$bin_vals

  n <- sum(data$bin_count)
  xmin <- res_mle$x_min
  xmax <- res_mle$x_max

  res <- dplyr::mutate(data,
                       low_biomass = bin_min * bin_count,
                       high_biomass = bin_max * bin_count,
                       low_biomass_norm = low_biomass / bin_width,
                       high_biomass_norm = high_biomass / bin_width,
                       mle_biomass =
                         p_biomass(x = data$bin_max,
                                   b = res_mle$b_mle,
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n) -
                         p_biomass(x = data$bin_min,
                                   b = res_mle$b_mle,
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n),
                       mle_conf_1_biomass =
                         p_biomass(x = data$bin_max,
                                   b = res_mle$b_conf[1],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n) -
                         p_biomass(x = data$bin_min,
                                   b = res_mle$b_conf[1],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n),
                       mle_conf_2_biomass =
                         p_biomass(x = data$bin_max,
                                   b = res_mle$b_conf[2],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n) -
                         p_biomass(x = data$bin_min,
                                   b = res_mle$b_conf[2],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n),
                       mle_biomass_norm = mle_biomass / bin_width,
                       mle_conf_1_biomass_norm = mle_conf_1_biomass / bin_width,
                       mle_conf_2_biomass_norm = mle_conf_2_biomass / bin_width)
 # TODO think about cases of xmax - think have done, just need to do tests
return(res)
}

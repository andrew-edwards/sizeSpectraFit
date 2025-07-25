##' @rdname p_biomass_bins
##' @param ... arguments to pass onto bin_data at least
##' @export
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

  # There is no uncertainty in the biomass in each bin, because we know the
  # individual body masses. So setting low_biomass and high_biomass to be the
  res <- dplyr::mutate(data,
                       low_biomass = bin_sum,
                       high_biomass = bin_sum,
                       low_biomass_norm = bin_sum_norm,
                       high_biomass_norm = bin_sum_norm,
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

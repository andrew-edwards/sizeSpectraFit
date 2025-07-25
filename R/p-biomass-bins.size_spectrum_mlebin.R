##' @rdname p_biomass_bins
##' @param ... arguments to pass onto ????
##' @export
p_biomass_bins.size_spectrum_mlebin <- function(res_mlebin){   # result from mlebin

  # TODO put some checks in, though given we have the class defined it should be good
  data <- res_mlebin$data %>%
    dplyr::mutate(bin_width = bin_max - bin_min)   # might already exist
  n <- sum(data$bin_count)
  xmin <- res_mlebin$x_min
  xmax <- res_mlebin$x_max

  # for binned data, the range of possible biomass in a bin is the count in bin
  #  * bin_min to count * bin_max. If individual values are known we also have
  # low_count and high_count and exact biomass. But I think we wouldn't really
  # do that (once we bin it we assume that's all we know). TODO clarify that
  res <- dplyr::mutate(data,
                       low_biomass = bin_min * bin_count,
                       high_biomass = bin_max * bin_count,
                       low_biomass_norm = low_biomass / bin_width,
                       high_biomass_norm = high_biomass / bin_width,
                       mle_biomass =
                         p_biomass(x = res_mlebin$data$bin_max,
                                   b = res_mlebin$b_mle,
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n) -
                         p_biomass(x = res_mlebin$data$bin_min,
                                   b = res_mlebin$b_mle,
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n),
                       mle_conf_1_biomass =
                         p_biomass(x = res_mlebin$data$bin_max,
                                   b = res_mlebin$b_conf[1],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n) -
                         p_biomass(x = res_mlebin$data$bin_min,
                                   b = res_mlebin$b_conf[1],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n),
                       mle_conf_2_biomass =
                         p_biomass(x = res_mlebin$data$bin_max,
                                   b = res_mlebin$b_conf[2],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n) -
                         p_biomass(x = res_mlebin$data$bin_min,
                                   b = res_mlebin$b_conf[2],
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n),
                       mle_biomass_norm = mle_biomass / bin_width,
                       mle_conf_1_biomass_norm = mle_conf_1_biomass / bin_width,
                       mle_conf_2_biomass_norm = mle_conf_2_biomass / bin_width)
 # TODO think about cases of xmax
return(res)
}

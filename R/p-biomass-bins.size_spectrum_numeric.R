##' @rdname p_biomass_bins
##' @param ... arguments to pass onto ????
##' @export
##' TODO numeric version could create a data.frame first like bin_data.numeric does,
##'   then use p_biomass_bins.data.frame. Something like:
##'   ifelse(!is.null(binValsTibble),
##'         binTibble <- binValsTibble,
##'      # Create tibble from the vector binBreaks:
##'         binTibble <- dplyr::tibble(wmin = binBreaks[-length(binBreaks)],
##'                                    wmax = binBreaks[-1])
##'         )
p_biomass_bins.size_spectrum_numeric <- function(res,   # result from MLE method
                                                 ){


  # TODO put some checks in, though given we have the class defined it should be good
  data <- res_mlebin$data
  n <- sum(data$bin_count)
  b <- res_mlebin$b
  xmin <- res_mlebin$xmin
  xmax <- res_mlebin$xmax

  # for binned data, the range of possible biomass in a bin is the count in bin
  #  * bin_min to count * bin_max. If individual values are known we also have
  # low_count and high_count and exact biomass. But I think we wouldn't really
  # do that (once we bin it we assume that's all we know). TODO clarify that
  res <- dplyr::mutate(data,
                       low_biomass = bin_min * bin_count,
                       high_biomass = bin_max * bin_count,
                       low_biomass_norm = mle_low_biomass / bin_width,
                       high_biomass_norm = mle_high_biomass / bin_width,
                       # TODO test that this is allowed (passing columns as
                       # arguments to functions; think it should be as things
                       # like mean() work
                       mle_biomass =
                         p_biomass(x = bin_max,
                                   b = res_mlebin$b,
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n) -
                         p_biomass(x = bin_min,
                                   b = res_mlebin$b,
                                   xmin = xmin,
                                   xmax = xmax,
                                   n = n),
                       mle_biomass_norm = mle_biomass / bin_width)
  # TODO if this works then do  for mle conf # intervals also

  }
  return(res)
}

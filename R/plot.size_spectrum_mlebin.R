##' Plot a binned ISD plots similar to MEPS Figure 7 (but with nonoverlapping bins)
##'
##' ##'
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
##' @param res_list size_spectrum_mlebin object resulting from running
##'   `fit_size_spectrum()` on binned data (such that the function
##'   `fit_size_spectrum_mlebin()` is used; see vignette TODO). # TODO link help
##'   to plot.size_spectrum_numeric()
##' @param x_plb vector of values to use to plot the fitted PLB curve; if NA then
##'   automatically calculated (sometimes need to manually extend it to hit the
##'   x-axis, but tricky to automate that on a log-scale
##' @param y_scaling numeric scaling of y-minimum of y-axis. Axis can't go to zero on
##'   log-log plot, but goes to the proportion `y_scaling` (<1)
##'   of the minimum value of counts greater than the highest `bin_min` value. Do
##'   such that can see the right-most bin in all plots.
plot.size_spectrum_mlebin <- function(res_mlebin,
                                      # from plot.size_spectrum_numeric(), best
                                      # to use these for consistency
                                      log_y_axis = "both", # or yes, or no
                                      xlim = c(min(res_mlebin$data$bin_min),
                                               max(res_mlebin$data$bin_max)),
                                      ylim = NA,
                                      x_plb = NA,
                                      y_scaling = 0.75,
                                      ){   # TODO decide if want ...

  stopifnot("Cannot define both x_small_ticks and x_small_ticks_by" =
              !(!is.null(x_small_ticks) & !is.null(x_small_ticks_by)))
  stopifnot("Cannot define both y_small_ticks and y_small_ticks_by" =
              !(!is.null(y_small_ticks) & !is.null(y_small_ticks_by)))
  stopifnot("log_y_axis must be both, yes, or no" =
              log_y_axis %in% c("both", "yes","no"))

  # Work out calculations needed for both types of plot and then pass them on to
  # plot_isd_binned()

  dat <- res_binned$data
  n <- sum(dat$bin_count)

  # Cpying from sizeSpectra::ISD_bin_plot()

  # Used later, but not may have functions for that
  # if(is.na(ySmall.inc)){
  #   ySmall.inc = yBig.inc/4
  #}

  # Think I can just put in arguments as defaults
#  if(missing(xlim)){
#    xlim = c(min(dat$bin_min),
#             max(dat$bin_max))
#  }

  # TODO don't think want to be able to define these, are they have been by
  # definition when do the mlebin calculation.
#  if(is.na(xmin)){
  x_min = min(dat$bin_min)

  #if(is.na(xmax)){
  x_max = max(dat$bin_max)

  # x values to plot PLB if not provided; need high resolution for both plots.
  if(is.na(x_plb)){
    x_plb <- exp(seq(log(xmin),
                     log(xmax),
                     length = 10000))

    #  Need to insert value close to xmax to make log-log curve go down further;
    #   since log(1 - pplb(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
    x_plb_length <- length(x_plb)
    x_plb <- c(x_plb[-x_plb_length],
               0.9999999999 * x_plb[x_plb_length],
               x_plb[x_plb_length])
  }

  y_plb = (1 - pPLB(x = x_plb,
                    b = b_MLE,
                    xmin = min(x_plb),
                    xmax = max(x_plb))) * n
  # To add curves for the limits of the 95% confidence interval of b:
  y_plb_conf_min = (1 - pPLB(x = x_plb,
                             b = res_binned$b_conf[1],
                            xmin = min(x_plb),
                            xmax = max(x_plb))) * n
  y_plb_conf_max = (1 - pPLB(x = x_plb,
                             b = res_binned$b_conf[1],
                             xmin = min(x_plb),
                             xmax = max(x_plb))) * n

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


    if(is.na(ylim)){
      ylim <- c(y_scaling * min(count_gte_bin_min),
                max(high_count))
    }


  if(log_y_axis == "both"){
    par(mfrow = c(2,1))
    plot_isd_binned(res_mlebin = res_mlebin,
                    log_y_axis = "no",
                    ylim = ylim,   # TODO copy these to next options
                    x_plb = x_plb,
                    y_plb_conf_min = y_plb_conf_min,
                    y_plb_conf_max = y_plb_conf_max,

                    ...)

    plot_isd_binned(res_mlebin = res_mlebin,
                    log_y_axis = "yes",
                    ylim = ylim,
                    ...)
  } else {
    plot_isd_binned(res_mlebin = res_mlebin,
                    log_y_axis = log_y_axis,  # should automatically work
                    ylim = ylim,
                    ...)
  }
}

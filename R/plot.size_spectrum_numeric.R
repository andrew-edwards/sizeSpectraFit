##' Plot individual size distribution of values and the MLE fit with
##' confidence intervals
##'
##' Plots one- or two-panel plot of the ISD and data. Two-panel plot is only for when
##' the data represent body masses, and gives the normalised biomass on log-log
##' axes and the individual points on log-log axes, both with the fitted
##' PLB distribution from the MLE method; this is essentially Fig. 6 of MEE
##' paper, but with bins in the top panel rather than points.
##'
##' For one-panel plot, it is either a linear y-axis or a logarithmic y-axis
##' (the latter being Fig. 6b of MEE paper, and is the default). Type of plot is
##' determined by the `log_y_axis` argument.
##'
##'
##' TODO this defaults to doing log-log and
##' option for recommended plot from MEE if it's biomass. Can always bin anyway
##' and just not normalise.
##'
##' @inheritParams plot_isd
##' @param res size_spectrum_numeric object, as output from
##'   [fit_size_spectrum.numeric()], which gets called when applying
##'   [fit_size_spectrum()] to a numeric vector
##' @param log_y_axis character either `"both"`, do two plots (like Fig. 6 of
##'   MEE paper, where the top panel is bins of normalized biomss, so only
##'   suitable when the data are body masses TODO change that option to
##'   "biomass", then add a both option to mean linear and logarithmic axis; be
##'   comsinstent with other plotting functions TODO), `"no"` for single plot with
##'   linear y axis, `"yes"` for single plot with logarithmic y axis (just Fig. 6b
##'   of MEE paper). Legends are automatically set, but can be tailored with the
##'   arguments defined below.
##' @param ... Further arguments for `plot_isd()` and then `plot()`
##' @return One- or two-panel plot of raw data and PLB distribution (and fits of
##'   confidence limits) as solid (and dashed) fitted using MLE method; returns
##'   nothing.
##'
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # TODO
##' res_vec <- fit_size_spectrum(sim_vec)
##' plot(res_vec)
##' plot(res_vec, log = "x")
##' plot(res_vec, log = "")
##' plot(res_vec, x_small_ticks_labels = c(5, 50, 500), log = "x") # Tailor the
##'   labels for a particular figure
##' }
plot.size_spectrum_numeric <- function(res,
                                       log_y_axis = "both",
                                       xlim = c(min(res$x),
                                                max(res$x)),
                                       ylim = NA,
                                       x_plb = NA,
                                       y_scaling = 0.75,
                                       mle_round = 2,
                                       legend_label_a = "(a)",
                                       legend_label_b = "(b)",
                                       legend_label_single = NULL, # for just one
                                       # panel
                                       # Use the a ones for single also TODO in help
                                       legend_text_a = paste0("b=",
                                                              round(res$b_mle,
                                                                    mle_round)),
                                       legend_text_a_n = paste0("n=",
                                                                round(length(res$x))),
                                       legend_text_b = NULL,
                                       legend_text_b_n = NULL,
                                       ...){

  stopifnot("log_y_axis must be both, yes, or no" =
              log_y_axis %in% c("both", "yes","no"))

  # Work out calculations needed for both types of plot and then pass them on to
  # plot_isd() (and plot_isd_binned() for `both`).:

  x <- res$x

  x_min <- min(x)   # or from results??? TODO
  x_max <- max(x)
  n <- length(x)

  # not sure these are needed; if xlim, ylim don't get specified won't they just
  # end up as these? TODO
  ## if(is.na(xlim_global[1])){
  ##   xlim_global = c(min(x),
  ##                   max(x))
  ## }
  ## if(is.na(ylim_global[1])){
  ##   ylim_global = c(1, length(x))
  ##   }

  # x values to plot PLB if not provided; need high resolution for both plots.
  if(is.na(x_plb)){
    x_plb <- exp(seq(log(x_min),
                     log(x_max),
                     length = 10000))

    #  Need to insert value close to xmax to make log-log curve go down further;
    #   since log(1 - pplb(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
    x_plb_length <- length(x_plb)
    x_plb <- c(x_plb[-x_plb_length],
               0.9999999999 * x_plb[x_plb_length],
               x_plb[x_plb_length])
  }

  y_plb = (1 - pPLB(x = x_plb,
                    b = res$b_mle,
                    xmin = min(x_plb),
                    xmax = max(x_plb))) * n
  # To add curves for the limits of the 95% confidence interval of b:
  y_plb_conf_min = (1 - pPLB(x = x_plb,
                             b = res$b_conf[1],
                             xmin = min(x_plb),
                             xmax = max(x_plb))) * n
  y_plb_conf_max = (1 - pPLB(x = x_plb,
                             b = res$b_conf[2],
                             xmin = min(x_plb),
                             xmax = max(x_plb))) * n

  if(is.na(ylim)){
    ylim <- c(y_scaling,
              length(x))
   # TODO in help mention for this one it just scales
                           # the y-axis since always multiplied by 1, the rank
                           # of largest value
  }

  if(log_y_axis == "both"){
    par(mfrow = c(2,1))
    # TODO this is LBN then log ISD like figure 6 MEE, going to change option to
    # "biomss" though TODO (see above)

    # HERE think now just want
    plot_lbn_style(res,
                   x_plb = x_plb)   # prob want ...
      # HERE

    # Going to plot with LBN method, so create an object that looks like output
    # of `plot.size_spectrum_mlebin()` to be called by
    # `plot_isd_binned()`. Rest of this comment might be wrong, since we want to
    # just calculate manually here what we actually need. THere is no
    # uncertainty in body mass of individuals in each bin, as we do know the
    # individual body masses.
    # NOPE: Since also need the `count_gte_bin_min` etc. values,
    # just fit the data using MLEbin but then change the MLE and conf intervals
    # and more to match the ones already calculated using MLE. Though some of
    # the further columns in res_mlebin$data are not relevant, since there is no
    # uncertainty in the counts in a bin, since we have the raw data.
    # ACTUALLY, some of that is wrong, we don't actually have vertical
    # uncertainty, and this already calcs bin_sum_norm which is what we want for
    # plotting.

#HERE    actually, just call it with res and then do the bin_data within the plotting
#    function, so plotting can cope with MLEbin results also

    ## # don't want this:
    ## res_mlebin <- fit_size_spectrum.data.frame(x_binned)
    ## res_mlebin_for_plot <- res_mlebin
    ## # Replace results with those from MLE method, to then facilitate plotting.
    ## res_mlebin_for_plot$b_mle <- res$b_mle
    ## res_mlebin_for_plot$b_conf <- res$b_conf
    ## res_mlebin_for_plot$x_min <- res$x_min
    ## res_mlebin_for_plot$x_max <- res$x_max      # So curve might stop before end
    ##                                     # of last bin, but that's the xmax calculated.

## Now using plot_lbn_style() above, but might want to add some of these in
##                     log = "x",  # maybe not, since always doing log-log?
##                     xlim = xlim,
##                     ylim = ylim,
##                     x_plb = x_plb,
##                     y_plb = y_plb,
##                     y_plb_conf_min = y_plb_conf_min,
##                     y_plb_conf_max = y_plb_conf_max,
##                     legend_label = legend_label_a,
##                     legend_text = legend_text_a,
##                     legend_text_n = legend_text_a_n,
##                     ...)  # ADD in more options maybe, see plot_isd_binned; figure out
##                           # useArgs() thing. Copy to next ones

   plot_isd(res = res,
             log = "xy",
             xlim = xlim,
             ylim = ylim,
             x_plb = x_plb,
             y_plb = y_plb,
             y_plb_conf_min = y_plb_conf_min,
             y_plb_conf_max = y_plb_conf_max,
             legend_label = legend_label_single,
             legend_text = legend_text_a,
             legend_text_n = legend_text_a_n,
             ...)
   } else {
    plot_isd(res = res,
             log = ifelse(log_y_axis == "yes",
                          "xy",
                          "x"),
             xlim = xlim,
             ylim = ylim,
             x_plb = x_plb,
             y_plb = y_plb,
             y_plb_conf_min = y_plb_conf_min,
             y_plb_conf_max = y_plb_conf_max,
             legend_label = legend_label_single,
             legend_text = legend_text_a,
             legend_text_n = legend_text_a_n,
             ...)
  }
}

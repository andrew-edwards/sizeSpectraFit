##' Plot individual size distribution of values and the MLE fit with
##' confidence intervals
##'
##' Plots one- or two-panel plot of the ISD and data, with maximum likelihood
##' fits shown, as calcualted using the MLE method.. Two-panel plot is either
##' the ISD style (individual points and fitted distribution) with (a) linear
##' y-axis and (b) logarithimc y-axis (so like Fig 7 of MEPS paper but for
##' unbinned data), or, only for when
##' the data represent body masses, (a) the normalised biomass on log-log
##' axes with fitted estimates and (b) same as (b) above, essentially the
##' recommended Fig. 6 of the MEE paper, but improved by showing bins in the top
##' panel rather than points.
##' Single plots are either the ISD style with logarithmic or linear y-axis. See
##' the `style` argument.
##'
##' Legends are automatically set, but can be tailored with the
##'   arguments defined below.
##'
##' @inheritParams plot_isd
##' @param res size_spectrum_numeric object, as output from
##'   [fit_size_spectrum.numeric()], which gets called when applying
##'   [fit_size_spectrum()] to a numeric vector
##' @param style character either:
##'   * `"log_y_axis"` - single ISD plot with logarithmic y axis (Fig. 6b of MEE paper)
##'   * `"linear_y_axis"` - for single ISD plot with linear y axis
##'   * `"both_y_axes"` - both the above plots as a two-panel plot
##'   * `"biomass_and_isd"` - to use only if the data represent body masses. Does two-panel
##'   plot, essentially the recommended Fig. 6 of MEE paper where the top panel
##'   is bins of normalized biomass (but improved here by showing bins in the top
##'   panel rather than points) and the `"log_y_axis"` plot described above.
##' TODO prob have to make that consistent in other plotting function
##'
##' @param ... Further arguments for `plot_isd()` and then `plot()` TODO check
##' @return One- or two-panel plot of raw data and PLB distribution (and fits of
##'   confidence limits) as solid (and dashed) fitted using MLE method; returns
##'   nothing. TODO could return invisible biomass calcs
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
                                       style = "log_y_axis",
                                       xlim = c(min(res$x),
                                                max(res$x)),
                                       ylim = NA,
                                       x_plb = NA,
                                       y_scaling = 0.75,
                                       mle_round = 2,
                                       inset_label = c(0, -0.02),
                                       xlab = expression(paste("Body mass, ",
                                                               italic(x), "(g)")),
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
                                       par_mai = c(0.4, 0.5, 0.05, 0.3),
                                       par_cex = 0.7,   # only for two panel
                                         # plots, use par() as usual for single plots
                                       ...){

  stopifnot("style must be log_y_axis, linear_y_axis, both_y_axes, biomass, or biomass_and_isd" =
              style %in% c("log_y_axis", "linear_y_axis", "both_y_axes",
                           "biomass", "biomass_and_isd"))

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

  if(style == "both_y_axes"){

    par(mfrow = c(2,1),
        mai = par_mai,
        cex = par_cex)

    plot_isd(res = res,
             log = "x",
             xlim = xlim,
             ylim = ylim,
             x_plb = x_plb,
             y_plb = y_plb,
             y_plb_conf_min = y_plb_conf_min,
             y_plb_conf_max = y_plb_conf_max,
             xlab = xlab,
             inset_label = inset_label,
             legend_label = legend_label_a,
             legend_text = legend_text_a,
             legend_text_n = legend_text_a_n,
             ...)

    plot_isd(res = res,
             log = "xy",
             xlim = xlim,
             ylim = ylim,
             x_plb = x_plb,
             y_plb = y_plb,
             y_plb_conf_min = y_plb_conf_min,
             y_plb_conf_max = y_plb_conf_max,
             xlab = xlab,
             inset_label = inset_label,
             legend_label = legend_label_b,
             legend_text = legend_text_b,
             legend_text_n = legend_text_b_n,
             ...)

  }

  if(style == "biomass"){

    plot_lbn_style(res,
                   x_plb = x_plb,
                   xlab = xlab,
                   inset_label = inset_label,
                   legend_label = legend_label_single,
                   legend_text = legend_text_a,
                   legend_text_n = legend_text_a_n,
                   ...)
  }

  if(style == "biomass_and_isd"){
    par(mfrow = c(2,1),
        mai = par_mai,
        cex = par_cex)

    plot_lbn_style(res,
                   x_plb = x_plb,
                   xlab = xlab,
                   inset_label = inset_label,
                   legend_label = legend_label_a,
                   legend_text = legend_text_a,
                   legend_text_n = legend_text_a_n,
                   ...)

## Now using plot_lbn_style() above, but might want to add some of these in
##                     log = "x",  # maybe not, since always doing log-log?
##                     ...)  # ADD in more options maybe, see plot_isd_binned; figure out

    plot_isd(res = res,
             log = "xy",
             xlim = xlim,
             ylim = ylim,
             x_plb = x_plb,
             y_plb = y_plb,
             y_plb_conf_min = y_plb_conf_min,
             y_plb_conf_max = y_plb_conf_max,
             xlab = xlab,
             inset_label = inset_label,
             legend_label = legend_label_b,
             legend_text = legend_text_b,
             legend_text_n = legend_text_b_n,
             ...)
  }

  if(style %in% c("linear_y_axis", "log_y_axis")){
    log_axes <- ifelse(style == "log_y_axis",
                       "xy",
                       "x")    # TODO test this

    plot_isd(res = res,
             log = log_axes,
             xlim = xlim,
             ylim = ylim,
             x_plb = x_plb,
             y_plb = y_plb,
             y_plb_conf_min = y_plb_conf_min,
             y_plb_conf_max = y_plb_conf_max,
             xlab = xlab,
             legend_label = legend_label_single,
             legend_text = legend_text_a,
             legend_text_n = legend_text_a_n,
             ...)
  }
}

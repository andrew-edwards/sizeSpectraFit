##' Plot a binned ISD plots similar to MEPS Figure 7 (but with nonoverlapping
##' bins). Also directly called from `plot.size_spectrum_mlebins()` with no
##' extra arugments for MLEbins method.
##'
##' @inheritParams plot.size_spectrum_numeric
##' @inheritParams plot_isd_binned
##' @param res_mlebin size_spectrum_mlebin object resulting from running
##'   `fit_size_spectrum()` on binned data (such that the function
##'   `fit_size_spectrum_mlebin()` is used; see vignette TODO). # TODO link help
##'   to plot.size_spectrum_numeric - may want to put them all together
##' @param log_y_axis character either `"both"`, do two plots (like Fig. 7 of
##'   MEPS paper), `"no"` for single plot with linear y axis (like Fig. 7a of
##'   MEPS paper), `"yes"` for single plot with logarithmic y axis (like Fig. 7b
##'   of MEPS paper). Legends are automatically set, but can be tailored with the
##'   arguments defined below.
##' @param legend_label_a character label (default `"(a)"`) to use for panel a for two-panel plot
##'   (`log_y_axis = "both"`).
##' @param legend_label_b character label to use for panel b for two-panel plot
##'   (`log_y_axis = "both"`).
##' @param legend_label_single character label to use for the only panel for a one-panel plot (`log_y_axis = "yes"` or `"no"`).
##' @param legend_text_a text to include in the legend for panel
##'   a for two-panel plot (`log_y_axis = "both"`), the `b = -1.58` in Fig. 7a
##'   of MEPS paper, or the only panel for a one-panel plot (`log_y_axis =
##'   "yes"` or `"no"`).
##' @param legend_text_b text to include in the legend for panel
##'   b for two-panel plot (`log_y_axis = "both"`); ignored for one-panel plot
##' @param legend_text_a_n, legend_text_b_n as for `legend_text_a` and
##'   `legend_text_b` but for another row of information, default being `n =
##'   <sample size>` as in Fig. 7a of MEPS paper.
##' @return one- or two-panel plot of the ISD with data in binned form like in
##'   Fig. 7, 7a or 7b (depending on settings) of MEPS paper, but with nonoverlapping bins; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
plot.size_spectrum_mlebin <- function(res_mlebin,
                                     # from plot.size_spectrum_numeric(), best
                                      # to use these for consistency
                                      log_y_axis = "both",
                                      xlim = c(min(res_mlebin$data$bin_min),
                                               max(res_mlebin$data$bin_max)),
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
                                                           round(res_mlebin$b_mle,
                                                                  mle_round)),
                                      legend_text_a_n = paste0("n=",
                                                               round(sum(res_mlebin$data$bin_count))),
                                      legend_text_b = NULL,
                                      legend_text_b_n = NULL,
                                      ...
                                      ){   # TODO decide if want ... yes, just
                                        # make sure help files link to all functions

  stopifnot("log_y_axis must be both, yes, or no" =
              log_y_axis %in% c("both", "yes","no"))

  # Work out calculations needed for both types of plot and then pass them on to
  # plot_isd_binned():

  dat <- res_mlebin$data
  n <- sum(dat$bin_count)

  # Cpying from sizeSpectra::ISD_bin_plot()

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
                    b = res_mlebin$b_mle,
                    xmin = min(x_plb),
                    xmax = max(x_plb))) * n
  # To add curves for the limits of the 95% confidence interval of b:
  y_plb_conf_min = (1 - pPLB(x = x_plb,
                             b = res_mlebin$b_conf[1],
                             xmin = min(x_plb),
                             xmax = max(x_plb))) * n
  y_plb_conf_max = (1 - pPLB(x = x_plb,
                             b = res_mlebin$b_conf[2],
                             xmin = min(x_plb),
                             xmax = max(x_plb))) * n


  if(is.na(ylim)){
    ylim <- c(y_scaling * min(dat$count_gte_bin_min),
              max(dat$high_count))
  }

  if(log_y_axis == "both"){
    par(mfrow = c(2,1))
    plot_isd_binned(res_mlebin = res_mlebin,
                    log = "x",
                    xlim = xlim,
                    ylim = ylim,
                    x_plb = x_plb,
                    y_plb = y_plb,
                    y_plb_conf_min = y_plb_conf_min,
                    y_plb_conf_max = y_plb_conf_max,
                    legend_label = legend_label_a,
                    legend_text = legend_text_a,
                    legend_text_n = legend_text_a_n,
                    ...)  # ADD in more options maybe, see plot_isd_binned; figure out
                          # useArgs() thing. Copy to next ones

    plot_isd_binned(res_mlebin = res_mlebin,
                    log = "xy",
                    xlim = xlim,
                    ylim = ylim,
                    x_plb = x_plb,
                    y_plb = y_plb,
                    y_plb_conf_min = y_plb_conf_min,
                    y_plb_conf_max = y_plb_conf_max,
                    legend_label = legend_label_b,
                    legend_text = legend_text_b,
                    legend_text_n = legend_text_b_n,
                    ...)
  } else {
    plot_isd_binned(res_mlebin = res_mlebin,
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

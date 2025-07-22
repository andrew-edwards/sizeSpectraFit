##' Plot a single binned ISD plot, as called from `plot.size_spectrum_mlebin()`,
##' or used directly from `plot.size_spectrum_numeric()` to make an LBN-style
##' plot of individual values and MLE plot.
##'
##' @inheritParams plot.size_spectrum_numeric
##' @inheritParams plot.size_spectrum_mlebin
##' @param x_plb vector of values to use to plot the fitted PLB curve; if NA then
##'   automatically calculated (sometimes need to manually extend it to hit the
##'   x-axis, but tricky to automate that on a log-scale)
##' @param y_plb vector of values corresponding to the MLE fit of the PLB
##'   distribution at each value of `x_PLB`
##' @param y_plb_conf_min, y_plb_conf_max vector of values corresponding to the fit, using the
##'   minimum/maximum value confidence interval for exponent b, of the PLB
##'   distribution at each value of `x_PLB`
##' @param y_scaling numeric scaling of y-minimum of y-axis. Axis can't go to zero on
##'   log-log plot, but goes to the proportion `y_scaling` (<1)
##'   of the minimum value of counts greater than the highest `bin_min` value. Do
##'   such that can see the right-most bin in all plots.
##' @param plot_conf_ints logical whether to plot confidence intervals or not
##' @param par_mai vector of values to use for `par(mai)`
##' @param par_cex numeric value to use for `par(cex)` (font size)
##' @param seg_col colour to use for the segments (green in Fig. 7 of
##'   MEPS paper).
##' @param fit_col colour to use for fitted curves
##' @param fit_lwd line thickness to use for fitted curves
##' @param conf_lty line type to use for confidence intervals
##' @param legend_text_second_row_multiplier numeric multiplier of the second
##'   row of legend text to space it out, especially for smaller panel plots.
##' @param LBN_style Whether to plot an LBN style plot (similar to Fig. 6a of MEE
##'   paper), used directly from `plot.size_spectrum_numeric()` to give the
##'   recommended Fig. 6 plot for MLE results, provided data are body masses.
##' @return single panel plot of the ISD with data in binned form like in
##'   Fig. 7a or 7b of MEPS paper, but with nonoverlapping bins; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
plot_isd_binned <- function(res_mlebin,
                            log,
                            xlim,
                            ylim,
                            x_plb,
                            y_plb,
                            y_plb_conf_min,
                            y_plb_conf_max,
                            plot_conf_ints = TRUE,
                            xlab = expression(paste("Values, ", italic(x))),
                            ylab = expression( paste("Total ", counts >= x),
                                              sep=""),  # Figure out how to do
                                                # it as optional but feed through.
                            mgp_val = c(1.6, 0.5, 0),
                            tcl_small = -0.2,
                            inset_label = c(0, 0),
                            inset_text = c(0, 0.04),
                            legend_label = NULL,
                            legend_text = NULL,
                            legend_text_n = NULL,
                            legend_text_second_row_multiplier = 2,
                            legend_position = "topright",
                            x_big_ticks = NULL,
                            x_big_ticks_labels = NULL,
                            x_small_ticks = NULL,
                            x_small_ticks_by = NULL,
                            x_small_ticks_labels = NULL,
                            y_big_ticks = NULL,
                            y_big_ticks_labels = NULL,
                            y_small_ticks = NULL,
                            y_small_ticks_by = NULL,
                            y_small_ticks_labels = NULL,
                            y_scaling = 0.75,
                            seg_col = "green",   # want these parsed along if
                                        # they're changed by users in original
                                        # call - useArgs or something? TODO
                            rect_col = "grey",
                            fit_col = "red",
                            fit_lwd = 2,
                            conf_lty = 2
                            # decide if want to have , ...)
                            # From sizeSpectra::ISD_bin_plot, may want some
                              #        xlim = NA,
                              #       xmin = NA,
                              #        xmax = NA,
                              # xLabel.small = c(5, 50, 500, 5000),
                              #        yBig.inc = 1000,
                              #        yBig.max = 10,
                              #        ySmall.inc = NA,
                              #        ySmall.tcl = -0.2,
                            #mgp.vals = c(1.6,0.5,0),
                            ){
  # Not sure if needed, see plot_isd() also and plot_lbn_style.
  stopifnot("Cannot define both x_small_ticks and x_small_ticks_by" =
              !(!is.null(x_small_ticks) & !is.null(x_small_ticks_by)))
  stopifnot("Cannot define both y_small_ticks and y_small_ticks_by" =
              !(!is.null(y_small_ticks) & !is.null(y_small_ticks_by)))

# mgp maybe need - see above commented option
# From ISD_bin_plot to adapt here, this is for linear y-axis, then have to tweak
  # to have the log option also:

  dat <- res_mlebin$data %>%
    dplyr::arrange(desc(bin_min))
                                   # Should overlay rectangles like in MEPS
                                  # Fig. 7, and not matter for
                                  # non-overlapping. TODO check.


    # y-axis not logged
  plot.default(dat$bin_min,      #    nothing plotted anyway as type = "n"
               dat$count_gte_bin_min,
               log = log,
               xlab = xlab,
               ylab = ylab,
               xlim = xlim,
               ylim = ylim,
               type = "n",
               axes = FALSE,
               mgp = mgp_val) # TODO

  # Add tickmarks and labels, replacing what was in ISD_bin_plot with this
  add_ticks(#x_lim = x_lim,
    #y_lim = y_lim,
    log = log,   # TODO make general, unless making big if switches
    tcl_small = tcl_small,
    mgp_val = mgp_val,
    x_big_ticks = x_big_ticks,
    x_big_ticks_labels = x_big_ticks_labels,
    x_small_ticks = x_small_ticks,
    x_small_ticks_by = x_small_ticks_by,
    x_small_ticks_labels = x_small_ticks_labels,
    y_big_ticks = y_big_ticks,
    y_big_ticks_labels = y_big_ticks_labels,
    y_small_ticks = y_small_ticks,
    y_small_ticks_by = y_small_ticks_by,
    y_small_ticks_labels = y_small_ticks_labels)

  rect(xleft = dat$bin_min,
       ybottom = dat$low_count,
       xright = dat$bin_max,
       ytop = dat$high_count,
       col = rect_col)
  segments(x0 = dat$bin_min,
           y0 = dat$count_gte_bin_min,
           x1 = dat$bin_max,
           y1 = dat$count_gte_bin_min,
           col = seg_col)

  if(log == "xy"){    # TODO didn't have for MLEbin plot, think if we need it
                      # for that, need to test
    # Need to manually draw the rectangle with low_count = 0 since it doesn't
    #  get plotted on log-log plot
    extra_rect <- dplyr::filter(dat,
                                low_count == 0)
    # if(nrow(extra.rect) > 1) stop("Check rows of extra rect.")
    rect(xleft = extra_rect$bin_min,
         ybottom = rep(0.01 * ylim[1],
                       nrow(extra_rect)),
         xright = extra_rect$bin_max,
         ytop = extra_rect$high_count,
         col = rect_col)

  segments(x0 = dat$bin_min,
           y0 = dat$count_gte_bin_min,
           x1 = dat$bin_max,
           y1 = dat$count_gte_bin_min,
           col = seg_col)
  }

  lines(x_plb, y_plb, col = fit_col, lwd = fit_lwd)   # Plot line last so can see it
  if(plot_conf_ints){
    lines(x_plb, y_plb_conf_min, col = fit_col, lty = conf_lty)
    lines(x_plb, y_plb_conf_max, col = fit_col, lty = conf_lty)
  }

# TODO fix the legend

  if(!is.null(legend_label)){   # plot_isd has as.character
    legend("topright",
           legend_label,
           bty = "n",
           inset = inset_label)
  }
# TODO if needed
#  if(!is.na(year)){  # might need if keep strata/year in there
#    legend("topright",
#           legend = year,
#           bty = "n",
#           inset = inset_year)
#  }

  if(!is.null(legend_text)){
  legend("topright",
         legend = legend_text,
         bty = "n",
         inset = inset_text)
  }


  # Add n
  if(!is.null(legend_text_n)){
  legend("topright",
         legend = legend_text_n,
         bty = "n",
         inset = legend_text_second_row_multiplier * inset_text)
  }

  box()     # to redraw axes over any boxes

  invisible()
}

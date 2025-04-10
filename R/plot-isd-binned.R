##' Plot a single binned ISD plot, as called from `plot.size_spectrum_mlebin()`
##'
##' ##'
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
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
                            ylab = expression( paste("Number of ", values >= x),
                                              sep=""),  # Figure out how to do
                                                # it as optional but feed through.
                            mgp_val = c(1.6, 0.5, 0),
                            tcl_small = -0.2,
                            inset_label = c(0, 0),
                            inset_text = c(0, 0.04),
                            legend_label = NULL,
                            legend_text = NULL,
                            legend_text_n = NULL,
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
                            par_mai = c(0.4, 0.5, 0.05, 0.3),
                            par_cex = 0.7,
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
  # Not sure if needed, see plot_isd() also.
  stopifnot("Cannot define both x_small_ticks and x_small_ticks_by" =
              !(!is.null(x_small_ticks) & !is.null(x_small_ticks_by)))
  stopifnot("Cannot define both y_small_ticks and y_small_ticks_by" =
              !(!is.null(y_small_ticks) & !is.null(y_small_ticks_by)))

  par(mai = par_mai,
      cex = par_cex)  # Affects all figures, TODO reset after
# mgp maybe need - see above commented option
# From ISD_bin_plot to adapt here, this is for linear y-axis, then have to tweak
  # to have the log option also:

  dat <- res_mlebin$dat

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
  lines(x_plb, y_plb, col = fit_col, lwd = fit_lwd)   # Plot line last so can see it
  lines(x_plb, y_plb_conf_min, col = fit_col, lty = conf_lty)
  lines(x_plb, y_plb_conf_max, col = fit_col, lty = conf_lty)

# TODO fix the legend

  if(!is.null(legend_label)){
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
         inset = 2 * inset_text)
  }

  box()     # to redraw axes over any boxes

  # Prob need to return some things
}

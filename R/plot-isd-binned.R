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
                            log_y_axis,
                            plot_conf_ints = TRUE,
                            xlab = expression(paste("Values, ", italic(x))),
                            ylab = expression( paste("Number of ", values >= x),
                                              sep=""),  # Figure out how to do
                                                # it as optional but feed through.
                            mgp_val = c(1.6, 0.5, 0),
                            tcl_small = -0.2,
                            legend_text = expression(paste("b=",
                                                           signif(res$b_mle,
                                                                  3),
                                                           sep="")),
                            legend_position = "topright",
                            inset = c(0, 0),
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
                            par_mai = c(0.4, 0.5, 0.05, 0.3),
                            par_cex = 0.7,




                            # decide if want to have , ...)


                            # From sizeSpectra::ISD_bin_plot, may want some
                                      xlim = NA,
                                      xmin = NA,
                                      xmax = NA,
                                      yScaling = 0.75,
                                      MLE.round = 2,
                                      xLabel.small = c(5, 50, 500, 5000),
                                      yBig.inc = 1000,
                                      yBig.max = 10,
                                      ySmall.inc = NA,
                                      ySmall.tcl = -0.2,
                                      inset.a = c(0, 0),
                                      inset.year = c(0, 0.04),
                                      seg.col = "green",
                                      rect.col = "grey",
                                      fit.col = "red",
                                      fit.lwd = 2,
                                      conf.lty = 2,
                                      par.mfrow = c(2, 1),
                                      mgp.vals = c(1.6,0.5,0),
                                      IBTS_MEPS_figs = FALSE,
                                      x.PLB = NA

                            ){

  par(mai = par_mai,
      cex = par_cex)  # Affects all figures, TODO reset after

# From ISD_bin_plot to adapt here, this is for linear y-axis, then have to tweak
  # to have the log option also:

  dat <- res_mlebin$dat

    # y-axis not logged
  plot(dat$bin_min,      #    nothing plotted anyway as type = "n"
       dat$count_gte_bin_min,
       log = "x",
       xlab = xlab,
       ylab = ylab,
       xlim = xlim,
       ylim = ylim,
       type = "n",
       axes = FALSE,
       mgp = mgp.vals) # TODO

  # Add tickmarks and labels, replacing what was in ISD_bin_plot with this
  add_ticks(#x_lim = x_lim,
    #y_lim = y_lim,
    log = "x",   # TODO make general, unless making big if switches
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
       ybottom = dat$lowCount,
       xright = dat$wmax,
       ytop = dat$highCount,
       col = rect.col)
  segments(x0 = dat$bin_min,
           y0 = dat$countGTEbin_min,
           x1 = dat$wmax,
           y1 = dat$countGTEbin_min,
           col = seg.col)
  lines(x.PLB, y.PLB, col = fit.col, lwd = fit.lwd)   # Plot line last so can see it
  lines(x.PLB, y.PLB.confMin, col = fit.col, lty = conf.lty)
  lines(x.PLB, y.PLB.confMax, col = fit.col, lty = conf.lty)

  legend("topright", "(a)",
         bty = "n",
         inset = inset.a)
  if(!is.na(year)){  # might need if keep strata/year in there
    legend("topright",
           legend = year,
           bty = "n",
           inset = inset.year)
  }

  legend("topright",
         legend = paste0("b=", round(b.MLE, MLE.round)),
         bty = "n",
         inset = 2 * inset.year)

  legend("topright",
         legend = paste0("n=", round(yRange[2], 2)),
         bty = "n",
         inset = 3 * inset.year)
  box()     # to redraw axes over any boxes




    if(is.na(ySmall.inc)){
    ySmall.inc = yBig.inc/4
  }

}

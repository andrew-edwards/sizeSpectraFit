##' Single plot of individual size distribution of values and the MLE fit with confidence intervals
##'
##' Plot the results from a fit to a vector of values, as in Figure 2h and 6b of
##' [1](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12641/full).
##' TODO add argument to do all of Figure 6.
##'
##' @param res size_spectrum_numeric object, as output from
##'   [fit_size_spectrum.numeric()], which gets called when applying
##'   [fit_size_spectrum()] to a numeric vector
##'
##'
##' @param panel Which panel number for multi-panel plots. `"h"` gives the
##'   method and MLE estimate (as in MEE Figure 2(h)), `"b"` gives just (b) as
##'   in Figure 6(b) and plots confidence interval curves, and NULL gives nothing.
##' @param log.xy Which axes to log, for `plot(..., log = log.xy)`. So "xy" for
##'   log-log axes, "x" for only x-axis logged.
##' @param mgpVals mgp values to use, as in `plot(..., mgp = mgpVals)`.
##' @param inset Inset distance for legend
##' @param xlim_global Define global x-axis limits
##' @param ylim_global Define global y-axis limits
##' @param ... Further arguments for `plot()`
##' @return Single figure of ISD on log-log plot (or log-linear depending on the
##'   options given).
##'
##' @export
##' @author Andrew Edwards
plot.size_spectrum_numeric <- function(res,
                                       panel = FALSE,
                                       log = "xy",
                                       xlab = expression(paste("Values, ", italic(x))),
                                       ylab = expression( paste("Number of ", values >= x), sep=""),
                                       mgp_val = c(1.6, 0.5, 0),
                                       inset = c(0, -0.04),
                                       tcl_small = -0.2,
                                       x_label_small = NULL,
                                       x_label_big = NULL,
                                       x_tick_by = 10,   # linear axis, small
                                       # ticks. Give a better name
                                       x_ticks_linear_default = TRUE,
                                       y_ticks_linear_default = TRUE,
                                       ...
                                       ){

  x <- res$x

  # not sure these are needed; if xlim, ylim don't get specified won't they just
  # end up as these? TODO
  ## if(is.na(xlim_global[1])){
  ##   xlim_global = c(min(x),
  ##                   max(x))
  ## }
  ## if(is.na(ylim_global[1])){
  ##   ylim_global = c(1, length(x))
  ##   }

  # To plot rank/frequency style plot:
  plot(sort(x,
            decreasing=TRUE),
       1:length(x),
       log = log,
       xlab = xlab,
       ylab = ylab,
       mgp = mgp_val,
       axes = FALSE,
       ...)

  # xlim, ylim will get automatically used above. These might not be needed
  # explicitly if just calc them in add_ticks():
#  x_lim = 10^par("usr")[1:2]
#  y_lim = 10^par("usr")[3:4]

  # Add tickmarks and labels
  add_ticks(#x_lim = x_lim,
    #y_lim = y_lim,
    log = log,
    tcl_small = tcl_small,
    mgp_val = mgp_val,
    # Add all these to arguments at top
    x_tick_start = x_tick_start,
    x_tick_by = x_tick_by,
    x_tick_end = x_tick_end,
    x_tick_big_labels = NULL,  # vector of values of big
          # tickmarks to add labels
    x_tick_small_labels = NULL,  # vector of values of small
    # ticks to add labels to
    x_ticks_linear_default = x_ticks_linear_default,   # if TRUE and linear axis used then default
                                     # tick marks are used; to tailor them set
                                     # to FALSE and specify in x_tick_start etc.
    y_tick_start = y_tick_start,
    y_tick_by = y_tick_by,
    y_tick_end = y_tick_end,
    y_tick_big_labels = NULL,
    y_ticks_linear_default = y_ticks_linear_default)

  box()


  ## if(log.xy == "xy"){
  ##   logTicks(xLim,
  ##            yLim,
  ##            xLabelSmall = c(5, 50, 500))   # Tick marks.
  ## }

  ## if(log.xy == "x"){
  ##   mgpVal = c(2, 0.5, 0)
  ##   logTicks(xLim,
  ##        yLim = NULL,
  ##        xLabelSmall = c(5, 50, 500),
  ##        mgpVal = mgpVal)
  ##   yBig = c(0, 500, 1000)
  ##   # Big labelled:
  ##   axis(2,
  ##        at = yBig,
  ##        labels = yBig,
  ##        mgp = mgpVal)
  ##   # Small unlabelled:
  ##   axis(2,
  ##        seq(yBig[1],
  ##            yBig[length(yBig)],
  ##            by = 100),
  ##    labels = rep("", 11),
  ##    tcl = -0.2,
  ##    mgp = mgpVal)
  ##   }
  if(FALSE){  # just ignore this for now
  x_plb = seq(min(x),
              max(x),
              length=1000)     # x values to plot PLB
  y_plb = (1 - pPLB(x = x_plb,
                    b = res$b_mle,
                    xmin = min(x_plb),
                    xmax = max(x_plb))) * length(x)
  lines(x_plb,
        y_plb,
        col="red")
  if(panel == "b"){
    for(i in 1:length(res_vec$b_conf)){
      lines(x_plb,
      (1 - pPLB(x = x_plb,
                      b = res_vec$b_conf[i],
                      xmin = min(x_plb),
                      xmax = max(x_plb))) * length(x),
            col="red",
            lty=2)
    }
    legend("topright",
           "(b)",
           bty = "n",
           inset = inset)
  }
  if(panel == "h"){
    legJust(c("(h) MLE",
            paste("b=", signif(b, 3), sep="")),
            inset=inset,
            logxy=TRUE)
  }
  }

}

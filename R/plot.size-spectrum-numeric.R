##' Single plot of individual size distribution of values and the MLE fit with confidence intervals
##'
##' Plot the results from a PLB fit to a vector of values, as in Figure 2h and 6b of
##' [1](http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12641/full). Also
##' shows PLB fits using the values of `b` at the ends of the confidence intervals.
##' TODO add argument to do all of Figure 6.
##'
##' @param res size_spectrum_numeric object, as output from
##'   [fit_size_spectrum.numeric()], which gets called when applying
##'   [fit_size_spectrum()] to a numeric vector
##' @param log Which axes to log, for `plot(..., log = log)`. So "xy" for
##'   log-log axes, "x" for only x-axis logged.
##' @param plot_conf_ints logical whether to plot PLB fit for confidence
##'   intervals or not
##' @param xlab, ylab x/y labels, explicitly given default values here which can
##'   be modified as required.
##' @param mgp_val mgp values to use, as in `plot(..., mgp = mgp_vals)`; see [?par].
##' @param tcl_small Length of small tickmarks; see [?axis].
##' @param legend_text text to put in the corner, defaults to `b = <value>`. Set to
##'   `NA` to have none, and modify default if needed.
##' @param legend_position where to place legend, gets used as the first
##'   argument in [legend()].
##' @param inset Inset distance for legend
##' @param x_big_ticks, y_big_ticks numeric vector of big tick marks on
##'   x-axis/y-axis. If NULL then gets done automatically, so if that does not
##'   look good then define explicitly here, plus `x_big_ticks_labels` and/or `y_big_ticks_labels`.
##' @param x_big_ticks_labels, y_big_ticks_labels numeric vector of big tick
##'   marks to label on x-axis/y-axis. If NULL then gets done automatically, so
##'   if that does not look good then define explicitly here
##' @param x_small_ticks, y_small_ticks  numeric vector of small tick marks on x-axis/y-axis. If
##'   NULL then gets done automatically, so if that does not  look good then
##'   define explicitly here.
##' @param x_small_ticks_by, y_small_ticks_by  numeric vector of increment to
##'   use to generate small tick marks on x-axis/y-axis. Will conincide with the
##'   big tick marks and extend beyond them. Can only define `x_small_ticks` or
##'   `x_small_ticks_by` (same for `y_...`). Only relevant for linear axes. Set
##'   to `NA` to force no small unlabelled tickmarks.
##' @param x_small_ticks_labels, y_small_ticks_labels numeric vector of big tick
##'   marks to label on x-axis/y-axis. If NULL then gets done automatically, so
##'   if that does not look good then define explicitly here
##' @param ... Further arguments for `plot()`
##' @return Single figure of ISD on log-log plot (or log-linear depending on the
##'   options given), with values as points and PLB fit (and fits of confidence
##'   limits) as solid (and dashed) lines.
##'
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res_vec <- fit_size_spectrum(sim_vec)
##' plot(res_vec)
##' plot(res_vec, log = "x")
##' plot(res_vec, log = "")
##' plot(res_vec, x_small_ticks_labels = c(5, 50, 500), log = "x") # Tailor the
##'   labels for a particular figure
##' }
plot.size_spectrum_numeric <- function(res,
                                       log = "xy",
                                       plot_conf_ints = TRUE,
                                       xlab = expression(paste("Values, ", italic(x))),
                                       ylab = expression( paste("Number of ", values >= x), sep=""),
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
                                       ...){

  stopifnot("Cannot define both x_small_ticks and x_small_ticks_by" =
              !(!is.null(x_small_ticks) & !is.null(x_small_ticks_by)))
  stopifnot("Cannot define both y_small_ticks and y_small_ticks_by" =
              !(!is.null(y_small_ticks) & !is.null(y_small_ticks_by)))

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

  box()

  ## if(log.xy == "x"){
  ##   mgpVal = c(2, 0.5, 0)
  ##   logTicks(xLim,
  ##        yLim = NULL,
  ##        xLabelSmall = c(5, 50, 500),
  ##        mgpVal = mgpVal)

  x_plb = seq(min(x),
              max(x),
              length=1000)     # x values to plot PLB
  y_plb = (1 - pPLB(x = x_plb,
                    b = res_vec$b_mle,
                    xmin = min(x_plb),
                    xmax = max(x_plb))) * length(x)
  lines(x_plb,
        y_plb,
        col="red")

  if(plot_conf_ints){
    for(i in 1:length(res_vec$b_conf)){
      lines(x_plb,
            (1 - pPLB(x = x_plb,
                      b = res_vec$b_conf[i],
                      xmin = min(x_plb),
                      xmax = max(x_plb))) * length(x), # TODO make more negative
                                        # I think, see other example
      col="red",
      lty=2)
    }
  }

  if(!is.na(as.character(legend_text))){
    legend("topright",
           eval(legend_text),
           bty = "n",
           inset = inset)
  }

    # Original code had this, may need but doubt it
    # if(panel == "h"){
    # legJust(c("(h) MLE",
    #             paste("b=", signif(b, 3), sep="")),
    #           inset=inset,
    #           logxy=TRUE)
    # }
}

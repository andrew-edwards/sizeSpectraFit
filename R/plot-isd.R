##' Single plot of individual size distribution of values and the MLE fit with
##' confidence intervals.
##'
##' Is called from `plot.size_spectrum_numeric()` with logarithmic or linear
##' y-axis TODO.
##'
##' @inheritParams plot.size_spectrum_numeric
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
##'   options given), with values as points or normalised binned biomass and PLB
##'   fit (and fits of confidence limits) as solid (and dashed) lines.
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
plot_isd <- function(res,
                     log,
                     xlim,
                     ylim,
                     x_plb,
                     y_plb,
                     y_plb_conf_min,
                     y_plb_conf_max,
                     plot_conf_ints = TRUE,
                     xlab = NULL,
                     ylab = expression(paste("Total ", counts >= x),
                                       sep=""),  # Figure out how to do
                     # it as optional but feed through.
                     mgp_val = c(1.6, 0.5, 0),
                     tcl_small = -0.2,
                     inset_label = c(0, 0),
                     inset_text = c(0, 0.02),
                     legend_label = NULL,
                     legend_text = NULL,
                     legend_text_n = NULL,
                     legend_text_second_row_multiplier = 3,
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
                     fit_col = "red",
                     fit_lwd = 2,
                     conf_lty = 2,
                     ...){
  # decide if want to have , ...)

  stopifnot("Cannot define both x_small_ticks and x_small_ticks_by" =
              !(!is.null(x_small_ticks) & !is.null(x_small_ticks_by)))
  stopifnot("Cannot define both y_small_ticks and y_small_ticks_by" =
              !(!is.null(y_small_ticks) & !is.null(y_small_ticks_by)))

  x <- res$x

  plot(sort(x,
            decreasing=TRUE),
       1:length(x),
       log = log,
       xlab = xlab,
       ylab = ylab,
       xlim = xlim,
       ylim = ylim,
       type = "p",
       axes = FALSE,
       mgp = mgp_val, # TODO
       ...)

  # Add tickmarks and labels
  add_ticks(
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

  lines(x_plb, y_plb, col = fit_col, lwd = fit_lwd)   # Plot line last so can see it

  if(plot_conf_ints){
    lines(x_plb, y_plb_conf_min, col = fit_col, lty = conf_lty)
    lines(x_plb, y_plb_conf_max, col = fit_col, lty = conf_lty)
  }

# TODO fix the legend maybe, see plot_isd_binned
  if(!is.null(legend_label)){
    legend("topright",
           eval(legend_label),
           bty = "n",
           inset = inset_label)
  }

    # Original code had this, may need but doubt it. Maybe.
    # if(panel == "h"){
    # legJust(c("(h) MLE",
    #             paste("b=", signif(b, 3), sep="")),
    #           inset=inset,
    #           logxy=TRUE)
# }

  if(!is.null(legend_text)){
    legend(legend_position,
           legend = legend_text,
           bty = "n",
           inset = inset_text)
  }

  if(!is.null(legend_text_n)){
    legend(legend_position,
           legend = legend_text_n,
           bty = "n",
           inset = legend_text_second_row_multiplier * inset_text)
  }
}

##' Add axes and tick marks to a plot, including for log axes to represent
##' unlogged values.
##'
##' Useful because you can then interpret the unlogged values, e.g. Figures 2(h)
##' and 6(b) of MEE paper and Figure 7 of MEPS paper.
##'
##' @param xLim the x limits for the plot (unlogged scale); if NULL then do not
##'   add anything to x-axis
##' @param yLim  the y limits for the plot (unlogged scale); if NULL then
##'   do not add anything to y-axis
##' @param tclSmall size of small tick marks
##' @param xLabelSmall which small tick marks on x-axis to label
##' @param yLabelSmall which small tick marks on y-axis to label
##' @param xLabelBig which big tick marks on the x-axis to label
##'   (when automated they can overlap, so may need to specify)
##' @param mgpVal `mgp` values for axes. See `?par`
##' @inherit plot.size_spectrum_numeric
##' @return Adds axes and big and small tick marks to the plot. Returns NULL
##' @examples
##' \dontrun{
##' # Adapt the following (could make an explicit example):
##'   plot(..., log="xy", xlab=..., ylab=..., xlim=..., ylim=..., axes=FALSE)
##'   xLim = 10^par("usr")[1:2]
##'   yLim = 10^par("usr")[3:4]
##'   logTicks(xLim, yLim, xLabelSmall = c(5, 50, 500))
##' }
##' @export
##' @author Andrew Edwards
add_ticks = function(log = "",
                     tcl_small = -0.2,
                     mgp_val = c(1.6, 0.5, 0),
                     x_big_ticks = NULL,
                     x_big_ticks_labels = NULL,
                     x_small_ticks = NULL,
                     x_small_ticks_by = NULL,
                     x_small_ticks_labels = NULL,
                     y_big_ticks = NULL,
                     y_big_ticks_labels = NULL,
                     y_small_ticks = NULL,
                     y_small_ticks_by = NULL,
                     y_small_ticks_labels = NULL){

  # Add tickmarks to x-axis
  if(log %in% c("x", "xy")){
    add_ticks_to_one_axis(log_scale = TRUE,
                          x_or_y = "x",
                          mgp_val = mgp_val,
                          tcl_small = tcl_small,
                          big_ticks = x_big_ticks,
                          big_ticks_labels = x_big_ticks_labels,
                          small_ticks = x_small_ticks,
                          small_ticks_by = x_small_ticks_by,
                          small_ticks_labels = x_small_ticks_labels)
  } else {
    add_ticks_to_one_axis(log_scale = FALSE,
                          x_or_y = "x",
                          mgp_val = mgp_val,
                          tcl_small = tcl_small,
                          big_ticks = x_big_ticks,
                          big_ticks_labels = x_big_ticks_labels,
                          small_ticks = x_small_ticks,
                          small_ticks_by = x_small_ticks_by,
                          small_ticks_labels = x_small_ticks_labels)
  }

  # Add tickmarks to y-axis
  if(log %in% c("y", "xy")){
    add_ticks_to_one_axis(log_scale = TRUE,
                          x_or_y = "y",
                          mgp_val = mgp_val,
                          tcl_small = tcl_small,
                          big_ticks = y_big_ticks,
                          big_ticks_labels = y_big_ticks_labels,
                          small_ticks = y_small_ticks,
                          small_ticks_by = y_small_ticks_by,
                          small_ticks_labels = y_small_ticks_labels)
  } else {
    add_ticks_to_one_axis(log_scale = FALSE,
                          x_or_y = "y",
                          mgp_val = mgp_val,
                          tcl_small = tcl_small,
                          big_ticks = y_big_ticks,
                          big_ticks_labels = y_big_ticks_labels,
                          small_ticks = y_small_ticks,
                          small_ticks_by = y_small_ticks_by,
                          small_ticks_labels = y_small_ticks_labels)
  }
}

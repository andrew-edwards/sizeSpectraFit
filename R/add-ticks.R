##' Add labelled and unlabelled tick marks to a plot, for linear or log axes
##' (where labels represent unlogged values),
##' calculated automatically if needed, or can be explicitly specified.
##'
##' Useful because you can then interpret the unlogged values, e.g. Figures 2(h)
##' and 6(b) of MEE paper and Figure 7 of MEPS paper. `log` argument needs to
##' match the original [plot()] call (this is not checked).
##'
##' @inherit plot.size_spectrum_numeric
##' @return Adds axes and big and small tick marks to the plot. Returns invisible.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot(1:10, axes = FALSE)
##' add_ticks()
##' box()
##'
##' plot(1:1000, axes = FALSE, log = "xy")
##' add_ticks(log = "xy")
##' box()
##' }
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
  invisible()
}

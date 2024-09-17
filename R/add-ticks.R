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
                     x_small_ticks_labels = NULL,
                     y_big_ticks = NULL,
                     y_big_ticks_labels = NULL,
                     y_small_ticks = NULL,
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
                          small_ticks_labels = x_small_ticks_labels)
  } else {
    add_ticks_to_one_axis(log_scale = FALSE,
                          x_or_y = "x",
                          mgp_val = mgp_val,
                          tcl_small = tcl_small,
                          big_ticks = x_big_ticks,
                          big_ticks_labels = x_big_ticks_labels,
                          small_ticks = x_small_ticks,
                          small_ticks_labels = x_small_ticks_labels)
}  # TODO then repeat for y-axis
    ## # Repeat for y axis:
    ## if(!is.null(yLim))               # if NULL then ignore
    ##   {
    ##   # Do enough tick marks to encompass axes:
    ##   yEncompassLog = c(floor(log10(yLim[1])), ceiling(log10(yLim[2])))
    ##   yBig = 10^c(yEncompassLog[1]:yEncompassLog[2])
    ##   # Big labelled:
    ##   axis(2, at= yBig, labels = yBig, mgp = mgpVal)
    ##   # Small unlabelled:
    ##   axis(2, yBig %x% ll, labels=rep("", length(yBig %x% ll)), tcl=tclSmall)
    ##   # Small labelled:
    ##   if(!is.null(yLabelSmall))
    ##       {
    ##       axis(2, at=yLabelSmall, labels=yLabelSmall, mgp=mgpVal, tcl=tclSmall)
    ##       }
    ##   }
}



##' Add tickmarks to an existing plot; from pacea, may want to use some. TODO
##' then delete
##'
##' Add sensible smaller (unlabelled) tickmarks to both axes of an existing
##' pacea temporal plot. Called from `plot.pacea_index()`,
##' `plot.pacea_recruitment()` etc. Is exported but unlikely to be needed externally.
##'
##' @param obj_lub obj a `pacea_index` object, which is a time series, with a date
##'   column that is the lubridate `date` class.
##' @inherit plot.pacea_index
##' @return adds tickmarks to an existing plot
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot.pacea_index(oni)   # see end of that function for usage
##' }
add_tickmarks <- function(obj_lub,
                          y_tick_by,
                          y_tick_start,
                          y_tick_end,
                          x_tick_extra_years,
                          start_decade_ticks){
  min <- min(lubridate::floor_date(obj_lub$date,
                                   unit = "year")) -
    lubridate::years(x_tick_extra_years)

  max <- max(lubridate::ceiling_date(obj_lub$date, unit = "year")) +
    lubridate::years(x_tick_extra_years)

  if(is.null(y_tick_start)){
    y_tick_start <- floor(par("usr")[3])
  }
  if(is.null(y_tick_end)){
    y_tick_end  <- ceiling(par("usr")[4])
  }


  # Small ticks every year
  axis(1,
       seq(min,
           max,
           by = "years"),
       labels = FALSE,
       tcl = -0.2)

  # Slightly larger ticks every decade (since not all get labelled automatically)
  axis(1,
       seq(start_decade_ticks,
           max,
           by = "10 years"),
       labels = FALSE,
       tcl = -0.3)

  axis(2,
       seq(y_tick_start,
           y_tick_end,
           by = y_tick_by),
       labels = FALSE,
       tcl = -0.2)
}

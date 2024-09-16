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
                     x_tick_start = NULL,
                     x_tick_by = NULL,
                     x_tick_end = NULL,
                     x_tick_big_labels = NULL,  # vector of values of big
                     # tickmarks to add labels
                     x_tick_small_labels = NULL,  # vector of values of small
                                        # ticks to add labels to
                     y_tick_start = NULL,
                     y_tick_by = NULL,
                     y_tick_end = NULL,
                     y_tick_big_labels = NULL
                     ){
#                    xLabelSmall = NULL,
#                    yLabelSmall = NULL,
#                    xLabelBig = NULL,
#                    mgpVal=c(1.6,0.5,0))

  # Add tickmarks to x-axis
  if(log %in% c("x", "xy")){
    add_ticks_to_one_axis(log_scale = TRUE,
                          x_or_y = "x",
                          increment = x_tick_by,   # only for linear axis
                          tick_big_labels = x_tick_big_labels,
                          tick_small_labels = x_tick_small_labels,
                          mgp_val = mgp_val,
                          tcl_small = tcl_small)
  } else {
    add_ticks_to_one_axis(log_scale = FALSE,
                          x_or_y = "x",
                          increment = x_tick_by,   # only for linear axis
                          tick_big_labels = x_tick_big_labels,
                          tick_small_labels = x_tick_small_labels,
                          mgp_val = mgp_val,
                          tcl_small = tcl_small)
}
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





##' Add ticks and lable to one axis
add_ticks_to_one_axis <- function(log_scale,
                                  x_or_y,
                                  increment,
                                  tick_big_labels,
                                  tick_small_labels,
                                  mgp_val,
                                  tcl_small){
  ll = 1:9
  # log10_ll = log10(ll)  not needed?

  if(x_or_y == "x"){
    axis_to_do <- 1
    lim <- par("usr")[1:2]   # But 10^ this below if logscale
  } else {
    axis_to_do <- 2
    lim <- par("usr")[3:4]
  }

  if(log_scale){
    lim <- 10^lim
    # Do enough tick marks to encompass axes:
    encompass_log <- c(floor(log10(lim[1])),
                       ceiling(log10(lim[2])))
    big <- 10^c(encompass_log[1]:encompass_log[2])
  } else {
# HERE, figure out what to do with ticks on linear. Add small and big options to plot.size_spectrum_numeric()
    big <- seq(0.9 * lim[1],
               1.1 * lim[2],
               by = increment)
  }

  # Big unlabelled, always want these:
  axis(axis_to_do,
       at = big,
       labels = rep("", length(big)),
       mgp = mgp_val)

  # Big labelled, if not specified then label them all
  if(is.null(tick_big_labels)){
    tick_big_labels = big
  }
  axis(axis_to_do,
       at = tick_big_labels,
       labels = tick_big_labels,
       mgp = mgp_val)

  # Small unlabelled:
  axis(axis_to_do,
       big %x% ll,
       labels=rep("",
                  length(big %x% ll)),
       tcl = tcl_small)

  # Small labelled:
  if(!is.null(tick_small_labels)){
    axis(axis_to_do,
         at = tick_small_labels,
         labels = tick_small_labels,
         mgp = mgp_val,
         tcl = tcl_small)
  }
}







##' Add tickmarks to an existing plot
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

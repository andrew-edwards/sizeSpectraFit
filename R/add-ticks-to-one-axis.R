
##' Add ticks and lable to one axis
add_ticks_to_one_axis <- function(log_scale,
                                  x_or_y,
                                  ticks_default,
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

#    big <- seq(0.9 * lim[1],
#               1.1 * lim[2],
#               by = increment)
  }

  if(ticks_default & !log_scale){
    # Default ticks and labelling
    axis(axis_to_do,
         mgp = mgp_val)
  } else {
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

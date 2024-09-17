##' Add ticks and labels to one axis
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

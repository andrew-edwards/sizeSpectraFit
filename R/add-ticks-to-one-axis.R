##' Add ticks and labels to one axis
##'
##' Called from [add_ticks()] which is called from [plot.size_spectrum_numeric()].
##' @param log_scale logical, whether not this axis is on the log10 scale
##' @param x_or_y character, either "x" or "y" to specify which axis the tick
##'   marks and labels are being added to
##' @param big_ticks, big_ticks_labels, small_ticks, small_ticks_by,
##'   small_ticks_labels See corresponding `x_...` definitions in
##'   [plot.size_spectrum_numeric()].
##' @inherit plot.size_spectrum_numeric
##' @return adds tickmarks to one axis of a plot. Returns invisible.
##' @export
##' @author Andrew Edwards
add_ticks_to_one_axis <- function(log_scale,
                                  x_or_y,
                                  tcl_small,
                                  mgp_val,
                                  big_ticks,
                                  big_ticks_labels,
                                  small_ticks,
                                  small_ticks_by,
                                  small_ticks_labels){
  ll = 1:9

  if(x_or_y == "x"){
    axis_to_do <- 1
    lim <- par("usr")[1:2]   # But then 10^ this below if logscale
  } else {
    axis_to_do <- 2
    lim <- par("usr")[3:4]
  }

  # Log scale
  if(log_scale){
    # Big ticks
    if(is.null(big_ticks)){      # If NULL then create them
      lim <- 10^lim
      # Do enough tick marks to encompass axes:
      encompass_log <- c(floor(log10(lim[1])),
                         ceiling(log10(lim[2])))
      big_ticks <- 10^c(encompass_log[1]:encompass_log[2])
    }

    # If big labels not specified then label them all, can assume always want that
    if(is.null(big_ticks_labels)){
      big_ticks_labels <- big_ticks
    }

    stopifnot("big_ticks_labels must be a subset of big_ticks; check the inputs for x and y" =
                all(big_ticks_labels %in% big_ticks))

    # Big unlabelled
    axis(axis_to_do,
         at = big_ticks,
         labels = rep("", length(big_ticks)),
         mgp = mgp_val)

    # Big labelled
    axis(axis_to_do,
         at = big_ticks_labels,
         labels = big_ticks_labels,
         mgp = mgp_val)

    # Small ticks
    if(is.null(small_ticks)){     # If NULL then create them
      small_ticks <- big_ticks %x% ll
    }

    # Small unlabelled:
    axis(axis_to_do,
         small_ticks,
         labels=rep("",
                    length(small_ticks)),
         tcl = tcl_small)

    # Small labelled:
    if(!is.null(small_ticks_labels)){
      stopifnot("small_ticks_labels must be a subset of small_ticks; check the inputs for x and y" =
                  all(small_ticks_labels %in% small_ticks))

      axis(axis_to_do,
           at = small_ticks_labels,
           labels = small_ticks_labels,
           mgp = mgp_val,
           tcl = tcl_small)
    }
  } else {
    # Linear scale, do the specified values else the usual default for big ticks.
    if(!is.null(big_ticks)){
      axis(axis_to_do,
           at = big_ticks,
           labels = rep("", length(big_ticks)),
           mgp = mgp_val)
    } else {
      axis(axis_to_do,
           mgp = mgp_val)
      if(axis_to_do == 1){
        axp <- "xaxp"
      } else {
        axp <- "yaxp"
      }

      big_ticks <- seq(par(axp)[1],
                       par(axp)[2],
                       length = par(axp)[3] + 1)
      # Not really, but just need the max and min below.
    }

    # Small ticks if specified
    if(!is.null(small_ticks)){
      axis(axis_to_do,
           small_ticks,
           labels=rep("",
                      length(small_ticks)),
           tcl = tcl_small)
    }

    # Small ticks if not specified then create them, unless small_ticks_by is
    #  NA then just add none. Cannot do is.na(NULL) so have to get creative.
    if(is.null(small_ticks)){          # Going to create them
      add_these_ticks <- TRUE
      if(!is.null(small_ticks_by)){    # Could still be NA
        if(!is.na(small_ticks_by)){
          small_ticks <- seq(min(big_ticks),
                             max(big_ticks),
                             small_ticks_by) # Could maybe check divisible
          # Then extend 20% of number of values each way to ensure full coverage.
          small_ticks <- seq(min(small_ticks) - floor(0.2 * length(small_ticks)) *
                               small_ticks_by,
                             max(small_ticks) + floor(0.2 * length(small_ticks)) *
                               small_ticks_by,
                             small_ticks_by)
        } else {
          add_these_ticks <- FALSE
        }
      } else {       # small_ticks_by is NULL and so is small_ticks, so create ticks
        small_ticks_by <- (big_ticks[2] - big_ticks[1])/10 # TODO small_ticks_per_big
        small_ticks <-  seq(min(big_ticks),
                            max(big_ticks),
                            small_ticks_by) # Could maybe check divisible
        # Then extend 20% of number of values each way to ensure full coverage.
          small_ticks <- seq(min(small_ticks) - floor(0.2 * length(small_ticks)) *
                             small_ticks_by,
                             max(small_ticks) + floor(0.2 * length(small_ticks)) *
                             small_ticks_by,
                             small_ticks_by)
      }

      if(add_these_ticks){
        axis(axis_to_do,
             small_ticks,
             labels=rep("",
                        length(small_ticks)),
             tcl = tcl_small)
      }
    }

    # Small labelled (even if small_ticks not specified)
    if(!is.null(small_ticks_labels)){
      axis(axis_to_do,
           at = small_ticks_labels,
           labels = small_ticks_labels,
           mgp = mgp_val,
           tcl = tcl_small)

    }
  }
}

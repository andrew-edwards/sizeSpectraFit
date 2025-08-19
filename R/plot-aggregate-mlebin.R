##' Do an aggregated MLEbin plot of several PLB fits, each to a separate (but related)
##' group of individuals.
##'
##' TODO forcussing on MLEbins first, but should be general enough but need
##' testing for MLEbin. This is copying [plot_aggregate] and then editing.
##'
##' TODO Am making plot_aggregate_mlebin() and maybe
##'   plot_aggregate_mlebins(), though prob best to make the latter the same,
##'   and don't want the green lines as a bit too confusing maybe.; could check
##'   the class here and have a general outfacing function,
##' or also make plot_aggregate_numeric and just call
##'   the right one. Some of the details could be shared here maybe.
##'
##' Given a list of MLEbin results, combine
##' the data and show an aggregated distribution, as well as the individual fits.
##'
##'
##' @param res_list list of results, with each component a list object of a
##'   given type TODO share help with plot-aggregate.
##' @param col_vec vector of colours to assign for each group
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' # See aggregating-size-spectra.Rmd TODO copy something to here maybe
##' }
##'
plot_aggregate_mlebin <- function(res_list,
                                  col_vec = c("orange", "lightblue", "green",
                                              "darkblue", "darkgreen"),
                                  col_agg = "magenta",
                                  xlim_global = NULL,
                                  ylim_global = NULL,
                                  y_scaling = 0.25,
                                  ...){

  # Basing this on plot_aggregate() for size_spectrum_numeric results and using
  # code from plot.size_spectrum_mlebin(). Also moving in calculations that were
  # in aggregate_mlebins(). Hard to make the plotting functions
  # general enough to do this, so just copying the relevant bits here.

  if(length(res_list) > length(col_vec)){
    stop("Need to add more colours to col_vec to have one for each results component in res_list.")
  }

  # TODO could generalise this in the master wrapper function
  if(!("list" %in% class(res_list))){
    stop("res_list need to be a list of lists of MLE results.")
  }

  # TODO could generalise this in the master wrapper function; may want mlebin
  # here also
  if(!("size_spectrum_mlebins" %in% class(res_list[[1]]))){
    stop("res_list need to be a list of size_spectrum_mlebinsTODO results.")
  }

  S <- length(res_list)                     # Number of species groups
  group_names <- names(res_list)

  # This is for size_spectrum_numeric:
#  x_global <- numeric()
#  for(s in 1:S){
#    x_global <- c(x_global,
#                  res_list[[s]]$x)
#  }

  # Aggregate all the data together to plot
  aggregated_data_temp <- tibble::tibble()

  for(i in 1:S){
#    res <- res_list[[i]]
    # Just want the mlebins_fit object, keeping it intact it is class
    # size_spectrum_mlebins and the plotting works automatically.

    aggregated_data_temp <- rbind(aggregated_data_temp,
                                  res_list[[i]]$data)
  }

  # Next aggregate matching bin_min and bin_max (might not be any, as would
  # require two species in different groups to have same bin_min and bin_max),
  # but need to recalculate anyway since count_gte_bin_min etc. will be different
  # for aggregated data set compared to values in each group (these get ignored
  # once we do the summarise, so no need to filter out).

  aggregated_data <- dplyr::summarise(group_by(aggregated_data_temp,
                                               bin_min,
                                               bin_max),
                                      bin_count = sum(bin_count)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(bin_min)

  # Can't do in dplyr, same approach as in fit_size_spectrum_mlebins(); maybe
  # create a function TODO
  count_gte_bin_min <- rep(NA,
                           length = nrow(aggregated_data))
  low_count <- count_gte_bin_min
  high_count <- count_gte_bin_min
# TODO think if this works for mlebin, expect it should
  for(iii in 1:length(count_gte_bin_min)){
    count_gte_bin_min[iii] <- sum( (aggregated_data$bin_min >= aggregated_data$bin_min[iii]) * aggregated_data$bin_count)
    low_count[iii] <- sum( (aggregated_data$bin_min >= aggregated_data$bin_max[iii]) * aggregated_data$bin_count)
    high_count[iii] <- sum( (aggregated_data$bin_max > aggregated_data$bin_min[iii]) * aggregated_data$bin_count)
  }

  aggregated_data$count_gte_bin_min <- count_gte_bin_min
  aggregated_data$low_count <- low_count
  aggregated_data$high_count <- high_count


  if(is.null(xlim_global)){
    xlim_global <- c(min(aggregated_data$bin_min),
                     max(aggregated_data$bin_min))
  }

  if(is.null(ylim_global)){
    ylim_global <- c(y_scaling * min(aggregated_data$count_gte_bin_min),
                     max(aggregated_data$high_count))
  }

  # Extract required values (need as vectors to create the aggregate fit)
  b_vec <- numeric()
  n_vec <- numeric()
  xmin_vec <- numeric()
  xmax_vec <- numeric()
  for(s in 1:S){
    b_vec[s] <- res_list[[s]]$b_mle
    n_vec[s] <- max(res_list[[s]]$data$high_count)
    xmin_vec[s] <- res_list[[s]]$x_min
    xmax_vec[s] <- res_list[[s]]$x_max   # TODO change to x_max_vec etc. Maybe,
                                        # thought I was trying to be
                                        # consistent. See plot_aggregate also if do
  }

  # col = col_vec[s])     need to decide on colurs of everything
  rect_col = col_vec    # TODO just do that for now then tweak when working
  seg_col = col_vec     # TODO tidy up also, may want vec for borders of rect?
  # Plot first one to automatically set up axes etc.
  plot(res_list[[1]],
       xlim = xlim_global,
       ylim = ylim_global,
       # col = col_vec[1],
       fit_col = col_vec[1],
       legend_text_a = NA,
       legend_text_a_n = NA,
       seg_col = rect_col[1],    # make an argument? TODO  NEED arg for rect, colours
                            # are off
       )   # want ... I think xlab etc

  # Full data, taking from plot_isd_binned():
    rect(xleft = aggregated_data$bin_min,
       ybottom = aggregated_data$low_count,
       xright = aggregated_data$bin_max,
       ytop = aggregated_data$high_count,
       col = "black")  #rect_col[1])
  segments(x0 = aggregated_data$bin_min,
           y0 = aggregated_data$count_gte_bin_min,
           x1 = aggregated_data$bin_max,
           y1 = aggregated_data$count_gte_bin_min,
           col = "black")  # seg_col[1])  # TODO even need these?

  # if(log == "xy")    # Not including any other option yet, TODO see if decide to

  # Need to manually draw the rectangle with low_count = 0 since it doesn't
  #  get plotted on log-log plot
  extra_rect <- dplyr::filter(aggregated_data,
                              low_count == 0)
  # if(nrow(extra.rect) > 1) stop("Check rows of extra rect.")

  rect(xleft = extra_rect$bin_min,
       ybottom = rep(0.01 * ylim_global[1],
                     nrow(extra_rect)),
       xright = extra_rect$bin_max,
       ytop = extra_rect$high_count,
       col = "black")  # TODO argument

  segments(x0 = aggregated_data$bin_min,
           y0 = aggregated_data$count_gte_bin_min,
           x1 = aggregated_data$bin_max,
           y1 = aggregated_data$count_gte_bin_min,
           col = "black")  # TODO
  # }

  # x values at which to calculate PLB's and PLB_agg; may have to do each one
  # manually here, though have already automatically done the first one above TODO
  # Doing evenly on a log scale since range is quite large for aggregated, and
  # x-axis is always logged
  x_plb_agg <- 10^seq(log10(xlim_global[1]),
                      log10(xlim_global[2]),
                      length = 1000)     # x values to plot PLB

  #  Need to insert value close to xmax to make log-log curve go down further;
  #   since log(1 - pPLB(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
  x_plb_agg_length <- length(x_plb_agg)

  x_plb_agg <- c(x_plb_agg[-x_plb_agg_length],
                 0.9999999999 * x_plb_agg[x_plb_agg_length],
                 x_plb_agg[x_plb_agg_length])

  # Add aggregated distribution first so that the right-most distribution shows up okay as
  # it overlays the aggregated one (and is thinner line).

  y_plb_agg = (1 - pPLB_agg(x = x_plb_agg,
                            b_vec = b_vec,
                            n_vec = n_vec,
                            xmin = xmin_vec,
                            xmax = xmax_vec)) * sum(n_vec)
  lines(x_plb_agg,
        y_plb_agg,
        col = col_agg,
        lwd = 4)

  # Now do remaining groups, just add them manually here
  for(s in 2:S){
    this_group_data <- res_list[[s]]$data

    rect(xleft = this_group_data$bin_min,
         ybottom = this_group_data$low_count,
         xright = this_group_data$bin_max,
         ytop = this_group_data$high_count,
         col = rect_col[s],
         border = rect_col[s])
    segments(x0 = this_group_data$bin_min,
             y0 = this_group_data$count_gte_bin_min,
             x1 = this_group_data$bin_max,
             y1 = this_group_data$count_gte_bin_min,
             col = seg_col[s])

  # if(log == "xy")    # Not including any other option yet, TODO see if decide to

  # Need to manually draw the rectangle with low_count = 0 since it doesn't
  #  get plotted on log-log plot
    extra_rect <- dplyr::filter(this_group_data,
                                low_count == 0)
    # if(nrow(extra.rect) > 1) stop("Check rows of extra rect.")
    rect(xleft = extra_rect$bin_min,
         ybottom = rep(0.01 * ylim_global[1],
                       nrow(extra_rect)),
         xright = extra_rect$bin_max,
         ytop = extra_rect$high_count,
         col = rect_col[s],
         border = rect_col[s])

    segments(x0 = this_group_data$bin_min,
             y0 = this_group_data$count_gte_bin_min,
             x1 = this_group_data$bin_max,
             y1 = this_group_data$count_gte_bin_min,
             col = seg_col[s])

    x_plb <- 10^seq(log10(xmin_vec[s]),
                    log10(xmax_vec[s]),
                    length = 1000)     # x values to plot PLB

    #  Need to insert value close to xmax to make log-log curve go down further;
    #   since log(1 - pPLB(xmax, ...)) = log(0) = -Inf   we need to force the asymptopte
    x_plb_length <- length(x_plb)

    x_plb <- c(x_plb[-x_plb_length],
               0.9999999999 * x_plb[x_plb_length],
               x_plb[x_plb_length])

    lines(x_plb,
          (1 - pPLB(x = x_plb,
                    b = b_vec[s],
                    xmin = xmin_vec[s],
                    xmax = xmax_vec[s])) * n_vec[s],
          col = col_vec[s])
  }
}

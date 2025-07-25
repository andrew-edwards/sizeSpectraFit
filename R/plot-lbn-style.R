##' Biomass size spectrum plot on log-log scale for body-mass data demonstrating
##' uncertainties
##'
##' Biomass size spectrum plot for binned data on a log-log scale like people
##' have been used to seeing. Shows the bin widths
##' explicitly and the normalised biomass in
##' each bin (with resulting uncertainties if applicable). So extending MEE Fig. 6 for already
##' binned data, using a new approach motivated by MEPS Fig. 7. Should probably
##' then be recommended to replace MEE Fig. 6. Axes are logged, and labelled as either
##' logged or (more intuitive) logged values. So it's an LBN-style plot but the
##' fitting is done using likelihood.
##'
##' If from the MLE method based on known individual body masses then there is
##' no uncertainty of the normalised biomass within each bin (no vertical
##' uncertainty on the plot). But if from the MLEbin method where the data are
##' already binned, then there is uncertainty and so we have uncertainty in the
##' vertical direction on the plot. Only appropriate for nonoverlapping bins.
##'
##' Adapting from `sizeSpectra::LBN_bin_plot()` and `plot_isd_binned()` (wanting
##' it consistent with the latter, but seemed better to make a new function).
##'
##' TODO use sizeSpectra vignette
##' `MLEbin_recommend.Rmd` for new vignette.
##'
##'
##' @param res results of class `size_spectrum_numeric` or
##' `size_spectrum_mlebin`
##'   ignore I think: TODO but if called
##'   from `plot.size_spectrum_numeric()` then the results are from the MLE
##'   method and do not have all the columns that would have from MLEbin method.
##'
##'
##' TODO onwards:
##' @param binValsTibble tibble of binned data with each row representing a bin
##'   and with columns `binMin` and `binMmax` (min and max break of each bin)
##'   and `binCount` (count in that bin), as in the `binVals` component of the
##'   output of `binData`. `wmin` and `wmax` can also be used instead of
##'   `binMin` and `binMax`. Extra columns are ignored.
##' @param binBreaks vector of bin breaks
##' @param binCounts vector of bin counts
##' @param b.MLE maximum likelihood estimate of *b* (ideally from the MLEbin method)
##' @param b.confMin lower 95\% confidence limits of *b*
##' @param b.confMax upper 95\% confidence limits of *b*
##' @param plot.binned.fitted if TRUE then also plot the estimated normalised
##'   biomass in each bin for the MLE of *b* and it's confidence limits
##' @param log.xy Which axes to log, for `plot(..., log = log.xy)`. So "xy" for
##'   log-log axes, "x" for only x-axis logged, "" for both axes unlogged.
##' @param xLim
##' @param yLim
##' @param rect.col
##' @param logLabels
##' @param xLab
##' @param ""))
##' @param yLab
##' @param x.PLB vector of values to use to plot the fitted PLB curve; if NA then
##'   automatically calculated
##' @param legend if TRUE then add legend
##' @param leg.pos position of legend, from "bottomright"', '"bottom"',
##'   '"bottomleft"', '"left"', '"topleft"', '"top"', '"topright"', '"right"'
##'   and '"center"'.
##' @param inset inset distance vector for legend
##' @param leg.text text for legend
##' @param ... further arguments to be passed to `plot()` and
##'   `plot_binned_fitted()`
##' @return TODO should return a tibble of results
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @
##' @}
plot_lbn_style <- function(res,
                           x_plb,
                           xlim = NULL,    # xlim for figures and x_lim for
                                        # fitting, should adopt that
                           ylim = NULL,
                           plot_conf_ints = TRUE,
                           plot_binned_fitted = TRUE,  # plot the binned fitted version
                           xlab = NULL,
                           ylab = "Normalised biomass",
                           mgp_val = c(1.6, 0.5, 0),
                           tcl_small = -0.2,
                           inset_label = c(0, 0),
                           inset_text = c(0, 0.04),
                           legend_label = NULL,
                           legend_text = NULL,
                           legend_text_n = NULL,
                           legend_text_second_row_multiplier = 2,
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
                           par_mai = c(0.4, 0.5, 0.05, 0.3),
                           par_cex = 0.7,
                           seg_col = "green",   # want these parsed along if
                           # they're changed by users in original
                           # call - useArgs or something? TODO
                           rect_col = "grey",
                           fit_col = "red",
                           fit_lwd = 2,
                           conf_lty = 2,
                           ...){

  # Not sure if needed, see plot_isd() also and plot_isd_binned()
  stopifnot("Cannot define both x_small_ticks and x_small_ticks_by" =
              !(!is.null(x_small_ticks) & !is.null(x_small_ticks_by)))
  stopifnot("Cannot define both y_small_ticks and y_small_ticks_by" =
              !(!is.null(y_small_ticks) & !is.null(y_small_ticks_by)))

  par(mai = par_mai,
      cex = par_cex)  # Affects all figures, TODO reset after
  # mgp maybe need - see above commented option

      # TODO might not need the if statement as have class-specific ones
  if("size_spectrum_numeric" %in% class(res)){
    # using bin_sum_norm for
    # plotting, no uncertainty in y-axis for data, by setting the
    # low_biomass_norm etc. to be same as bin_sum_norm in
    #  p_biomass_bins.size_spectrum_numeric()

    dat <- p_biomass_bins(res)

    n <- sum(dat$bin_count)

    plot_data_boxes_first = FALSE   # plot the data boxes last, since only
                                    # horizontal lines representing data
                                    # (which are actually drawn as rectangles).

  } else {
    dat <- p_biomass_bins(res)   # might not need n calc as above as maybe
    # already saved in res, need to tease out here TODO, next line should work:
    n <- sum(res$data$bin_count)

    plot_data_boxes_first = TRUE   # Will have uncertainty in data, so plot
                                      # data first as fitted boxes won't
                                      # completely cover them up
  }




# TODO may need to adjust limits, using something like this (to be adapted):
  if(is.null(xlim)){
     xlim <- c(min(dat$bin_min),
               max(dat$bin_max))
  }

  if(is.null(ylim)){
    ylim <- c(min(dat$low_biomass_norm),
              max(dat$high_biomass_norm))          # TODO use the conf intervals also
  }

# copying  and editing from plot_isd_binned() (too hard to just generalise that)

  plot.default(dat$bin_min,      #    nothing plotted anyway as type = "n"
               dat$high_biomass_norm,
               log = "xy",
               xlab = xlab,
               ylab = ylab,
               xlim = xlim,
               ylim = ylim,
               type = "n",
               axes = FALSE,
               mgp = mgp_val) # TODO

  # Add tickmarks and labels, replacing what was in ISD_bin_plot with this
  # TODO get working:
  add_ticks(
    log = "xy",
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


  # Data:
  if(plot_data_boxes_first){
    rect(xleft = dat$bin_min,
         ybottom = dat$low_biomass_norm,
         xright = dat$bin_max,
         ytop = dat$high_biomass_norm,
         col = rect_col,
         lwd = 1)     # TODO generalise, but want thick for .numeric below since no
    # uncertainty; this might be fine
  }


  # Option to plot binned version of fitted curve (do first to then overlay the
  # straight lines of biomass density)
  if(plot_binned_fitted){
    plot_lbn_fitted(dat,
                    ...)
  }

  if(!plot_data_boxes_first){
    rect(xleft = dat$bin_min,
         ybottom = dat$low_biomass_norm,
         xright = dat$bin_max,
         ytop = dat$high_biomass_norm,
         col = rect_col,
         lwd = 2)     # TODO generalise, but want thick for .numeric below since no
    # uncertainty; this might be fine
  }


  # Biomass density for each value of x, from MEE equation (4), using the MLE for b.
  lines(x_plb,
        dPLB(x_plb,
             b = res$b_mle,
             xmin = min(x_plb),
             xmax = max(x_plb)) * n * x_plb,
        col="red")   # TODO generalise once working

  if(plot_conf_ints){
    # Add lines at limits of the 95% confidence interval of b:
    lines(x_plb,
          dPLB(x_plb,
               b = res$b_conf[1],
               xmin = min(x_plb),
               xmax = max(x_plb)) * n * x_plb,
          col="red",
          lty=2)

    lines(x_plb,
          dPLB(x_plb,
               b = res$b_conf[2],
               xmin = min(x_plb),
               xmax = max(x_plb)) * n * x_plb,
          col="red",
          lty=2)
  }

# TODO fix the legend

  if(!is.null(legend_label)){   # plot_isd has as.character
    legend("topright",
           legend_label,
           bty = "n",
           inset = inset_label)
  }
# TODO if needed
#  if(!is.na(year)){  # might need if keep strata/year in there
#    legend("topright",
#           legend = year,
#           bty = "n",
#           inset = inset_year)
#  }

  if(!is.null(legend_text)){
  legend("topright",
         legend = legend_text,
         bty = "n",
         inset = inset_text)
  }


  # Add n
  if(!is.null(legend_text_n)){
  legend("topright",
         legend = legend_text_n,
         bty = "n",
         inset = legend_text_second_row_multiplier * inset_text)
  }

  box()     # to redraw axes over any boxes

  # TODO decide if might need something like this
#  if(is.na(x_plb)){
#    x_plb <- exp(seq(log(min(binTibble$wmin)),
#                     log(max(binTibble$wmax)),
#                     length = 10000))   # values to plot the MLE fit,
#                                        # encompassing data
#  }

  invisible()   # TODO can return something invisibly, might be worth doing
}

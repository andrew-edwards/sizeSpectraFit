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
                           xlim,
                           ylim,
                           x_plb,
                           y_plb,
                           y_plb_conf_min,
                           y_plb_conf_max,
                           plot_conf_ints = TRUE,
                           xlab = expression(paste("Body mass, ", italic(x))),
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

  if(class(res) == "size_spectrum_numeric"){
    # Need to create bins manually
    data <- bin_data(res$x,
                     bin_width = "2k")$bin_vals   # use bin_sum_norm for
    # plotting

  } else {
    data <- res$data
    # Need a range of normalised biomasses for plotting to account for
    #  possible ranges of body sizes within each bin:
    data <- dplyr::mutate(data,
                          low_biomass = bin_min * bin_count,
                          high_biomass = bin_max * bin_count,
                          low_biomass_norm = low_biomass / bin_width)
  }


  HERE
  # Calculate the fitted estimates of biomass in each bin, for b, b.confMin and
  # b.confMax. Should be able to do the same approach for both types, need to
  # look at this function and adapt it.
  binTibble <- pBiomassBinsConfs(binValsTibble = binTibble,
#                                 xmin = min(binTibble$wmin),
#                                 xmax = max(binTibble$wmax),
#                                 n = sum(binTibble$Number),
                                 b.MLE = b.MLE,
                                 b.confMin = b.confMin,
                                 b.confMax = b.confMax)



# TODO may need to adjust limits, using something like this (to be adapted):
  ##   if(is.na(xLim)){
  ##   xLim <- c(min(binTibble$wmin),
  ##             max(binTibble$wmax))
  ## }

  ## if(is.na(yLim)){
  ##   yLim <- c(min(binTibble$lowBiomassNorm),
  ##             max(binTibble$highBiomassNorm))          # may need pull
  ## }




copying  and editing from plot_isd_binned():

  dat <- res_mlebin$data

  # Need to calculate the low and high normalised biomasses in each bin, may
  # want to return this TODO  only for MLEbin I think:
HERE
  dat <- dplyr::mutate(dat,
                       lowBiomass = wmin * Number,
                       highBiomass = wmax * Number,
                       binWidth = wmax - wmin,
                       lowBiomassNorm       = lowBiomass / binWidth,
                       highBiomassNorm      = highBiomass / binWidth)

  plot.default(dat$bin_min,      #    nothing plotted anyway as type = "n"
               dat$bin_count_norm,
               log = "xy",
               xlab = xlab,
               ylab = ylab,
               xlim = xlim,
               ylim = ylim,
               type = "n",
               axes = FALSE,
               mgp = mgp_val) # TODO
HERE
  # Add tickmarks and labels, replacing what was in ISD_bin_plot with this
  add_ticks(#x_lim = x_lim,
    #y_lim = y_lim,
    log = log,   # TODO make general, unless making big if switches
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

  rect(xleft = dat$bin_min,
       ybottom = dat$low_count,
       xright = dat$bin_max,
       ytop = dat$high_count,
       col = rect_col)
  segments(x0 = dat$bin_min,
           y0 = dat$count_gte_bin_min,
           x1 = dat$bin_max,
           y1 = dat$count_gte_bin_min,
           col = seg_col)

  if(log == "xy"){    # TODO didn't have for MLEbin plot, think if we need it
                      # for that, need to test
    # Need to manually draw the rectangle with low_count = 0 since it doesn't
    #  get plotted on log-log plot
    extra_rect <- dplyr::filter(dat,
                                low_count == 0)
    # if(nrow(extra.rect) > 1) stop("Check rows of extra rect.")
    rect(xleft = extra_rect$bin_min,
         ybottom = rep(0.01 * ylim[1],
                       nrow(extra_rect)),
         xright = extra_rect$bin_max,
         ytop = extra_rect$high_count,
         col = rect_col)

  segments(x0 = dat$bin_min,
           y0 = dat$count_gte_bin_min,
           x1 = dat$bin_max,
           y1 = dat$count_gte_bin_min,
           col = seg_col)
  }

  lines(x_plb, y_plb, col = fit_col, lwd = fit_lwd)   # Plot line last so can see it
  if(plot_conf_ints){
    lines(x_plb, y_plb_conf_min, col = fit_col, lty = conf_lty)
    lines(x_plb, y_plb_conf_max, col = fit_col, lty = conf_lty)
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

  invisible()





  from sizeSpectra::LBN_bin_plot(), putting above what we need from
  plot_isd_binned() which has the correct formatting, then go through this for
  anything else we need:
 - means think we can get rid of but keep for a second for context



HERE

  plot(binTibble$wmin,         # not plotted, just need something
       binTibble$highBiomassNorm,
       log = log.xy,
       xlab = xLab,
       ylab = yLab,
#       mgp = mgpVals,
       xlim = xLim,
       ylim = yLim,
       xaxt = ifelse(log.xy %in% c("x", "xy") , "n", "s"),
       yaxt = ifelse(log.xy == "xy", "n", "s"),
       type = "n",            # empty plot
       ...)



  box()

  legend(leg.pos,
         leg.text,
         bty="n",
         inset = inset)

#  axis(2, at = yBigTicks,
#       mgp = mgpVals)
#  axis(2, at = ySmallTicks,
#       mgp = mgpVals,
#       tcl = -0.2,
#       labels = rep("", length(ySmallTicks)))

  rect(xleft = binTibble$wmin,
       ybottom = binTibble$lowBiomassNorm,
       xright = binTibble$wmax,
       ytop = binTibble$highBiomassNorm,
       col = rect.col)

  if(is.na(x.PLB)){
    x.PLB <- exp(seq(log(min(binTibble$wmin)),
                     log(max(binTibble$wmax)),
                     length = 10000))   # values to plot the MLE fit,
                                        # encompassing data
  }


  # Option to plot binned version of fitted curve (do first to then overlay the
  # straight lines of biomass density)
  if(plot.binned.fitted){
    plot_binned_fitted(binTibble,
                       ...)
  }

  # Biomass density for each value of x, from MEE equation (4), using the MLE for b.
  B.PLB <- dPLB(x.PLB,
                b = b.MLE,
                xmin=min(x.PLB),
                xmax=max(x.PLB)) * sum(binTibble$Number) * x.PLB

  lines(x.PLB,
        B.PLB,
        col="red")

  # Add lines at limits of the 95% confidence interval of b:
  lines(x.PLB,
        dPLB(x.PLB,
             b = b.confMin,
             xmin = min(x.PLB),
             xmax = max(x.PLB)) * sum(binTibble$Number) * x.PLB,
        col="red",
        lty=2)

  lines(x.PLB,
        dPLB(x.PLB,
             b = b.confMax,
             xmin = min(x.PLB),
             xmax = max(x.PLB)) * sum(binTibble$Number) * x.PLB,
        col="red",
        lty=2)

  # Go through this and tidy up. Could also do nonlogged version.
  #   And add red and (uncertainty) pink rectangles for ranges expected by the
  #   fitted distributions
  invisible(binTibble)
}


from sizeSpectra:

##' Add horizontal bars and shaded rectangles to `LBN_bin_plot()`
##'
##' These are to show the estimated normalised biomasses in each bin, based on
##' the MLE value of `b` and it's confidence limit values.
##' Each horizontal bar spans a bin (default colour is red), with it's vertical value indicating the
##' expected normalised biomass based on the MLE of `b`. The height of the
##' shaded rectangles (default colour pink) show the range of expected
##' normalised biomass based on the 95\% confidence interval of `b`.
##' @param binTibble tibble of values calculated in `LBN_bin_plot()`; this
##' function adds to that plot
##' @param bar.col colour for the horizontal bars representing the MLE values of
##' normalised biomass in each bin
##' @param bar.lwd thickness of horiztonal bars
##' @param rect.shading colour for shading of rectangles corresponding to the
##' normalised biomasses estimated from confidence intervals of `b`
##' @param shorter fraction shorter to make the rectangles, so can see them
##' overlapping with grey rectangles; may not work exacly as planned (won't be symmetric) when x-axis
##' not logged, but that's not going to be a useful plot anyway
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @donttest{
##' @
##' @}
plot_binned_fitted <- function(binTibble,
                               bar.col = "red",
                               bar.lwd = 3,
                               rect.shading = "pink",
                               shorter = 0.05
                               ){

  # Rectangles corresponding to confidence interval ranges, it doesn't matter
  # that sometimes we'll have ybottom > ytop (I think it might almost be guaranteed
  # to happen for at least one bin).m
  rect(xleft = (1 + shorter) * binTibble$wmin,
       ybottom = binTibble$estBiomassNormConfMin,
       xright = (1 - shorter) * binTibble$wmax,
       ytop = binTibble$estBiomassNormConfMax,
       col = rect.shading)

  # Horizontal bars corresponding to MLE values
  segments(x0 = binTibble$wmin,
           y0 = binTibble$estBiomassNormMLE,
           x1 = binTibble$wmax,
           y1 = binTibble$estBiomassNormMLE,
           col = bar.col,
           lwd = bar.lwd)


}

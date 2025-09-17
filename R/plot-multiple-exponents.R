##' Plot estimated *b* with confidence intervals for multiple groups and strata
##'
##' A somewhat specific plot for the Mediterranean analyses of five species
##' groups fitted for three strata, but applicable to other sets of results.
##' For each group there will be three strata (to do with fishing being
##' allowed), and we want to see how *b* changes within each group under the
##' changes in fishing. Bascially plotting out the results saved in
##' `res_tib`. For the Mediterranean analyses we also show the results for the
##' Full community, but with an option to shade that area in grey because the
##' fits were not great.
##'
##' For doing time series probably best to adapt and make a new function.
##'
##' @param res_tib tibble that includes columns:
##'  - `Group`: a group of related species for the Mediterranean data
##'  - `Strata`:three different scenarios for the Mediterranean data:
##'    - Baseline: before the no-take-reserve implemented)
##'    - FG: fishing grounds outside the no-take-reserve
##'    - NTR: inside the no-take-reserve
##'  - `Low b`: the 95\% lower confidence limit of the exponent *b*
##'  - `MLE b`: the maximum likelihood estimate of *b*
##'  -  `High b` the 95\% upper confidence limit of *b*
##' @param col_strata character vector of colours, need one colour for each strata
##' @param pch_strata vector of pch numeric values, one for each strata
##' @param pch_cex numeric, size of the points, gets used by `points(..., cex =
##'   pch_cex)`
##' @param shade_first logical, whether to shade the background for the first
##'   strata in lightgrey, because (for Mediterranean analyses) this is the full
##'   community and the fits were not great, so the results are not so reliable.
##' @param xlim, ylim vectors for axes limits; if NULL (the default) they get
##'   calculated automatically.
##' @param x_jitter Amount to left/right jitter each strata within a group;
##'   default works great for an example plot, needs generalising TODO
##' @param legend_position character description of keyword of where to put
##'   legend using `legend()`, one of '"bottomright"', '"bottom"',
##'   '"bottomleft"', '"left"', '"topleft"', '"top"', '"topright"', '"right"'
##'   or '"center"'
##' @param legend_inset inset distance(s) from the margins as a fraction of the plot
##'          region when legend is placed by keyword, used as `legend(..., inset = legend_inset)`.
##' @param legend_bty type of box to be drawn around the legend, either '"o"'
##'   (the default) or '"n"'. Used as `legend(..., bty = legend_bty)`.
##' @param xlab label for x-axis (default is nothing, as Group headings are
##'   shown at the top
##' @param ylab label for y-axis
##' @param y_minor_tick_start used as `add_minor_tickmarks(..., y_tick_start =
##'   y_minor_tick_start)` but is automatically calculated; specify a value if
##'   it needs tweaking
##' @param y_minor_tick_by numeric increment of minor tick marks, set to a high
##'   value (like 100) to have no minor tick marks.
##' @param gap_axis used as `axis(3, gap.axis = gap_axis)` to specify minimal
##'   gap between labels on the top axis
##' @return desired plot in current device
##' @export
##' @author Andrew Edwards, adapted from `timeSerPlot()`
plot_multiple_exponents = function(res_tib,
                                   col_strata = c("blue",
                                                  "darkgreen",
                                                  "red"),
                                   pch_strata = c(15, 16, 17),
                                   pch_cex = 1.3,
                                   shade_first = FALSE,
                                   xlim = NULL,
                                   ylim = NULL,
                                   x_jitter = NULL,
                                   legend_position = "bottomright",
                                   legend_inset = c(0, 0),
                                   legend_bty = "o",
                                   xlab= "",
                                   ylab = expression(paste("Estimate of ", italic(b)),
                                                     sep=""),
                                   y_minor_tick_start = NULL,
                                   y_minor_tick_by = 0.2,
                                   gap_axis = 0.3){

  if(is.null(x_jitter)){
    x_jitter <- 0.5
  }

  groups <- unique(res_tib$Group)
  num_groups <- length(groups)

  strata <- unique(res_tib$Strata)
  num_strata <- length(strata)

  num_combinations <- num_groups * num_strata

  if(length(col_strata) != num_strata){
    stop(paste("Need col_strata to have", num_strata,
               "colours, one for each strata."))
  }
  if(length(pch_strata) != num_strata){
    stop(paste("Need pch_strata to have", num_strata,
               "values, one for each strata."))
  }



  # Centre for each group
  x_group <- (1:num_groups) * num_strata - 1
  x_values <- c(x_group - x_jitter,
                x_group,
                x_group + x_jitter) %>%
    sort()


  if(is.null(xlim)){
    xlim = c(x_group[1] - 0.5 * diff(x_group)[1],
             max(x_group) + 0.5 * diff(x_group)[1])
  }

  if(is.null(ylim)){
    ylim = range(c(res_tib$`Low b`,
                   res_tib$`High b`),
                 na.rm=TRUE)
  }

  # Plot in order of the tibble
  plot(NA,
       xlim = xlim,
       ylim = ylim,
       xlab = xlab,
       ylab = ylab,
       axes = FALSE,
       xaxs = "i")

  if(shade_first){
    rect(xlim[1],
         par("usr")[3],
         x_group[1] + 0.5 * diff(x_group)[1],
         par("usr")[4],
         col = "lightgrey")
  }

  box()
  axis(2)

  # Minor tickmarks on y-axis
  if(is.null(y_minor_tick_start)){
    y_minor_tick_start <- floor(par("usr")[3])
  }

  add_minor_tickmarks(x_tick_start = 1000,   # don't want any
                      x_tick_by = 10,
                      x_tick_end = 1020,
                      y_tick_start = y_minor_tick_start,
                      y_tick_by = y_minor_tick_by,
                      y_tick_end = NULL)

  axis(3,
       at = x_group,
       tick = FALSE,
       labels = groups,
       gap.axis = gap_axis)

  # Just the MLEs
  points(x_values,
         res_tib$`MLE b`,
         pch = rep(pch_strata,
                   num_groups),
         col = rep(col_strata,
                   num_groups),
         cex = pch_cex
         )

  # Confidence intervals:
  segments(x0 = x_values,
           y0 = res_tib$`Low b`,
           x1 = x_values,
           y1 = res_tib$`High b`,
           col = rep(col_strata,
                     num_groups))

  # Add grey lines to separate groups
  abline(v = x_group[-length(x_group)] + 0.5 * diff(x_group)[1],
         col = "lightgrey")

  # Add legend
  legend(x = legend_position,
         legend = strata,
         col = col_strata,
         pch = pch_strata,
         pt.cex = pch_cex,
         inset = legend_inset,
         bty = legend_bty)

  invisible()
}

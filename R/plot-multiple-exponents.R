##' Plot estimated *b* with confidence intervals for multiple groups and strata
##'
##' A somewhat specific plot for the Mediterranean analyses of five species
##' groups fitted for three strata, but applicable to other sets of results.
##' For each group there will be three strata (to do with fishing being
##' allowed), and we want to see how *b* changes within each group under the
##' changes in fishing. Bascially plotting out the results saved in `res_tib`.
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
##'
##' ##' @param legName legend name for that panel
##' @param method method used to obtain the inputted estimates of `b`
##' @param weightReg  TRUE if doing weighted regression (using standard errors)
##'   or FALSE to not do weighted regression.
##' @param bCol colour for points for *b*
##' @param pchVal pch for points for *b*
##' @param cexVal size of points for *b*
##' @param confCol colour for confidence intervals for *b*
##' @param confThick thickness of vertical line for confidence intervals
##' @param xLim x-axis limits
##' @param yLim y-axis limits
##' @param xLab label for x-axis
##' @param yLab label for y-axis
##' @param xTicksSmallInc increments for where to have small (unlabelled)
##'   tickmarks on x-axis
##' @param xTicksSmallTck tick length for small (unlabelled) tickmarks on x-axis
##' @param yLabels whether or not to label main tickmarks on y-axis
##' @param yTicksSmallInc increments for where to have small (unlabelled)
##' tickmarks on y-axis
##' @param yTicksSmallTck tick length for small (unlabelled) tickmarks on y-axis
##' @param legPos legend position
##' @param newPlot TRUE to create a new plot, FALSE to add to existing
##' @param regPlot TRUE to plot the regression line and conf intervals
##' @param regColNotSig colour for regression line (and its confidence intervals)
##' if the trend is not significant
##' @param regColSig colour for regression line (and its confidence intervals)
##' if the trend is significant
##' @param legExtra extra manually-specified legend (e.g. to distinguish two
##' sets of results)
##' @param legExtraPos position for extra manually-specified legend
##' @param legExtraCol colours (vector) for extra manually-specified legend
##' @param insetVal inset shift for naming the panel
##' @param xJitter value to jitter the x-values by (for comparison plot the
##'   confidence intervals overlap)
##' @param doRegression whether to calculate a regression for the time series of estimates
##' @return if `doRegression` is TRUE (default) then return dataframe of just
##'   one row with columns (else return nothing):
##'   * Method: method used
##'   * Low: lower bound of 95\% confidence interval
##'   * Trend: gradient of regression fit
##'   * High: upper bound of 95\% confidence interval
##'   * p: p-value of regression fit
##'   * Rsquared: r-squared of regression fit
##'   * adjRsquared: adjusted r-squared of regression fit
##' @export
##' @author Andrew Edwards, adapted from `timeSerPlot()`
plot_multiple_exponents = function(res_tib,
                                   col_strata = c("blue",
                                                  "darkgreen",
                                                  "red"),
                                   pch_strata = c(15, 16, 17),
                                   xlim = NULL,
                                   ylim = NULL,
                                   x_jitter = NULL,    # Amount to left/right jitter each
                                        # strata within a group; generalise for
                                   # more strata
                                   legend_position = "bottomright",
                                   legend_inset = c(0, 0),
                                   legend_bty = "o",
                                   ylab = expression(paste("Estimate of ", italic(b)),
                                                     sep=""),
                                   xlab= "",
                                   cexVal = 1,
                                   confCol = "black",
                                   confThick = 1,

                       xTicksSmallInc = NULL,
                       xTicksSmallTck = 0.01,
                       yLabels = TRUE,
                       yTicksSmallInc = NULL,
                       yTicksSmallTck = 0.01)  # TODO remove what not needed

  {
  if(is.null(x_jitter)){
    x_jitter <- 0.5
  }

  groups <- unique(res_tib$Group)
  num_groups <- length(groups)

  strata <- unique(res_tib$Strata)
  num_strata <- length(strata)

  num_combinations <- num_groups * num_strata

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
  box()

  axis(2)
  # TODO add tickmarks, use my function add_ticks or probably
  # add_ticks_to_one_axis() since only doing one


  axis(3,
       at = x_group,
       tick = FALSE,
       labels = groups)

  # Just the MLEs
  points(x_values,
         res_tib$`MLE b`,
         pch = rep(pch_strata,
                   num_groups),
         col = rep(col_strata,
                   num_groups))

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
         inset = legend_inset,
         bty = legend_bty)

  if(FALSE){
       legend(legPos, legName, bty="n", inset=insetVal)
       if(!is.null(yTicksSmallInc))
           { yTicksSmall = seq(yLim[1], yLim[-1], by=yTicksSmallInc)
             axis(2, at = yTicksSmall, labels = rep("", length(yTicksSmall)),
                tck=-yTicksSmallTck)
           }
       if(!is.null(xTicksSmallInc))
           { xTicksSmall = seq(xLim[1], xLim[-1], by=xTicksSmallInc)
             axis(1, at = xTicksSmall, labels = rep("", length(xTicksSmall)),
                tck=-xTicksSmallTck)
           }
  }

  invisible()
}

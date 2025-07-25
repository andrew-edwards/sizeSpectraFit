##' Add horizontal bars and shaded rectangles to `plot_lbn_style()`
##'
##' TODO check help. These are to show the estimated normalised biomasses in each bin, based on
##' the MLE value of `b` and it's confidence limit values.
##' Each horizontal bar spans a bin (default colour is red), with it's vertical value indicating the
##' expected normalised biomass based on the MLE of `b`. The height of the
##' shaded rectangles (default colour pink) show the range of expected
##' normalised biomass based on the 95\% confidence interval of `b`.
##' @param dat tibble of values calculated in `plot_lbn_style()`; this
##' function adds to that plot
##' @param bar_col colour for the horizontal bars representing the MLE values of
##' normalised biomass in each bin
##' @param bar_lwd thickness of horiztonal bars
##' @param rect_shading colour for shading of rectangles corresponding to the
##' normalised biomasses estimated from confidence intervals of `b`
##' @param shorter fraction shorter to make the rectangles, so can see them
##' overlapping with grey rectangles; may not work exacly as planned (won't be symmetric) when x-axis
##' not logged, but that's not going to be a useful plot anyway
##' @return  TODO
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' 3+3
##' }
plot_lbn_fitted <- function(dat,
                            bar_col = "red",
                            bar_lwd = 3,
                            rect_shading = "pink",
                            rect_border = "pink",
                            shorter = 0.05
                            ){
  # Rectangles corresponding to confidence interval ranges, it doesn't matter
  # that sometimes we'll have ybottom > ytop (I think it might almost be guaranteed
  # to happen for at least one bin, yes, think they must switch). So can't say
  # top corresponds to max or min of conf interval of b, I think. TODO tidy up.
  rect(xleft = (1 + shorter) * dat$bin_min,
       ybottom = dat$mle_conf_1_biomass_norm,
       xright = (1 - shorter) * dat$bin_max,
       ytop = dat$mle_conf_2_biomass_norm,
       col = rect_shading,
       border = rect_border)

  # Horizontal bars corresponding to MLE values
  segments(x0 = dat$bin_min,
           y0 = dat$mle_biomass_norm,
           x1 = dat$bin_max,
           y1 = dat$mle_biomass_norm,
           col = bar_col,
           lwd = bar_lwd)
}

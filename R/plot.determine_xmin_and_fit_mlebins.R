##' Plot results from determining xmin by mode method and then fitting use
##' MLEbins method
##'
##' @param res list of class `determine_xmin_for_mlebins_and_fit` as output from `determine_xmin_for_mlebins_and_fit()`
##' @param xlim_hist numeric vector of two values representing `xlim` for histogram plot; default is the full range
##'   of breaks (which might be too large for a clear figure, especially given
##'   the linear axis).
##' @param ... arguments to passed onto [hist()] or [plot.size_spectrum_mlebins()]
##' @inheritParams plot.size_spectrum_mlebin
##' # @inheritParams plot.size_spectrum_numeric   might need these two if not
##'   everything captured (i.e. if help doesn't flow through from these) TODO
##' # @inheritParams plot_isd_binned
##'
##' @return figure in current device
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' See .Rmd
##' }
plot.determine_xmin_and_fit_mlebins <- function(res,
                                                xlim_hist = NULL,
                                                par_mai = c(0.4, 0.5, 0.05, 0.3),
                                                par_cex = 0.7,
                                                seg_col = "green",
                                                ...){

  # Global xlim, might want to add functionality at some point
  #xlim_global <- c(min(unlist(lapply(res, '[[',
  #                                   "xmin"))[years_indices]),
  #                 max(unlist(lapply(res, '[[',
  #                                 "xmax"))[years_indices]))

  mai_orig <- par("mai")
  par(mfrow = c(3,1))

  # par(mai = mai_orig)     # TODO (from sizeSpectraHake): think about Since gets reset by ISD_bin_plot(). Should clean
                          # up that function in sizeSpectra.

  # Don't think needed now, this was specific for hake:
  # Have to make the full histogram (with 0 counts here) to get the colours
  #  right, but ISD plot (I think) requires no 0 counts (maybe they get ignored).
#  make_hist_full <- make_hist(res[[i]]$counts_per_bin,
#                                bin_width = res[[i]]$bin_width)



  col_hist <- ifelse(res$h$mids < res$mlebins_fit$x_min,
                     "grey",
                     "red")

  border_col = "black"

  # If too fine then don't have black borders:  TODO generalise, this was hake specific
  #if(res$h$bin_width < 0.1){
  #  border_col = col_hist
  #}

  # arguments <- list(...)

  # if (!"seg_col" %in% names(arguments)) {
  #  seg_col <- "green"    # the default in plot.size_spectrum_mlebins(); just
                          # can't use that automatically, see below.
  #}

  if(is.null(xlim_hist)){
    dots_parser(graphics:::plot.histogram,
                x = res$h,
                # xlim = xlim_global,
                col = col_hist,
                border = border_col,
                ...)} else {
    dots_parser(graphics:::plot.histogram,
                x = res$h,
                # xlim = xlim_global,
                col = col_hist,
                border = border_col,
                xlim = xlim_hist,
                ...)
  }

  par(mai = par_mai,
      cex = par_cex)

  dots_parser(plot.size_spectrum_mlebin,
              # This should be
              # plot.size_spectrum_mlebins but the
              # dots_parser doesn't pass on style (TODO had thought seg_col was
              # the issue)
              # because formals(FUN) I think does not
              # detect the arguments for the
              # subsequent function plot...mlebin(), and hence ignores the
              #  style = "linear_y_axis" argument given here (and things like xlab).
              # Option 1. since plot..mlebins() basically calls
              # plot...mlebin() with seg_col = "green", can circumvent the
              # ...mlebins() call here and add seg_col as an explicit
              # option. TODO check if that works for other options though; don't
              # think it will. Don't see what's special about seg_col now - yes,
              # want it to switch the default to green as for mlebins plots,
              # since now calling plot...mlebin.
              res_mlebin = res$mlebins_fit,
              style = "linear_y_axis",
              seg_col = seg_col,
              ...)

  dots_parser(plot.size_spectrum_mlebin,
              res_mlebin = res$mlebins_fit,
              style = "log_y_axis",
              seg_col = seg_col,
              ...)

  # par(mfrow = c(1,1))  # TODO
}

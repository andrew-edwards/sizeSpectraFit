##' Plot results from determining xmin by mode method and then fitting use
##' MLEbins method # TODO function is actually the same as
##' plot.determine_xmin_and fit, since plot is already assigned correctly. So
##' might not need this one, or just call the other one. TODO TODO NOT quite,
##' two lines changed.
##'
##' @param res list of class `determine_xmin_for_mlebins_and_fit` as output from `determine_xmin_for_mlebins_and_fit()`
##' @param xlim_hist numeric vector of two values representing `xlim` for histogram plot; default is the full range
##'   of breaks (which might be too large for a clear figure, especially given
##'   the linear axis). TODO could maybe add in log-x axis, but not option in
##'   `plot.hist()` so would have to make it up here.
##' @param ... arguments to passed onto [hist()] for now, but probably want
##'   more. TODO `sizeSpectra::ISD_bin_plot_nonoverlapping()`
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
                                                par_cex = 0.7,   # these two are
                                                  # only for isd plots TODO
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



  col_hist <- ifelse(res$h$mids < res$mlebins_fit$x_min,  # TODO changed also
                     "grey",
                     "red")

  border_col = "black"

  # If too fine then don't have black borders:  TODO generalise, this was hake specific
  #if(res$h$bin_width < 0.1){
  #  border_col = col_hist
  #}

  if(is.null(xlim_hist)){
    plot(res$h,
         # xlim = xlim_global,
         col = col_hist,
         border = border_col,
         ...)} else {
    plot(res$h,
         # xlim = xlim_global,
         col = col_hist,
         border = border_col,
         xlim = xlim_hist,
         ...)
 }

  par(mai = par_mai,
      cex = par_cex)
  plot(res$mlebins_fit,
       style = "linear_y_axis")    # TODO prob want ..., e.g. for seg_col

  plot(res$mlebins_fit,
       style = "log_y_axis")

  # plot(res$mle_fit)  # TODO this changes for mlebins, could functionalise the
  # hist plot above since needed for all data types

  # par(mfrow = c(1,1))  # TODO
}

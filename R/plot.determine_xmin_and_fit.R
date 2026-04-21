##' Plot results from determining xmin by mode method and then fitting use MLE hake spectra results
##'
##' TODO check if want features from plot.determine_xmin_and_fit_mlebins(),
##' which was adapted and improved from this. TODO combine help files.
##'
##' @param res list of class `determine_xmin_and_fit` as output from `determine_xmin_and_fit()`
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
plot.determine_xmin_and_fit <- function(res,
                                        xlim_hist = NULL,
                                        main_hist = "",
                                        ...){

  # Global xlim, might want to add functionality at some point
  #xlim_global <- c(min(unlist(lapply(res, '[[',
  #                                   "xmin"))[years_indices]),
  #                 max(unlist(lapply(res, '[[',
  #                                 "xmax"))[years_indices]))

  par_orig <- par(no.readonly = TRUE)
  on.exit(par(par_orig))

  mai_orig <- par("mai")
  par(mfrow = c(2,1))

  # Don't see that this was doing anything
#  par(mai = mai_orig)     # TODO (from sizeSpectraHake): think about Since gets reset by ISD_bin_plot(). Should clean
                          # up that function in sizeSpectra. TODO

  # Don't think needed now, this was specific for hake:
  # Have to make the full histogram (with 0 counts here) to get the colours
  #  right, but ISD plot (I think) requires no 0 counts (maybe they get ignored).
#  make_hist_full <- make_hist(res[[i]]$counts_per_bin,
#                                bin_width = res[[i]]$bin_width)



  col_hist <- ifelse(res$h$mids < res$mle_fit$x_min,
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
         main = main_hist,
         ...)} else {
    plot(res$h,
         # xlim = xlim_global,
         col = col_hist,
         border = border_col,
         xlim = xlim_hist,
         main = main_hist,
         ...)
 }

  plot(res$mle_fit)
}

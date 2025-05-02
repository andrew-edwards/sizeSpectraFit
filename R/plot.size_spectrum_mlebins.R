##' Plot a binned ISD plots similar to MEPS Figure 7 from MLEbins method.
##'
##' Think can just pass everything on, need to test TODO. And just link to
##'   plot.size_spectrum_mlebin() help.
##' @inheritParams plot.size_spectrum_numeric
##' @inheritParams plot_isd_binned
##' @return one- or two-panel plot of the ISD with data in binned form like in
##'   Fig. 7, 7a or 7b (depending on settings) of MEPS paper, with overlapping bins; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
plot.size_spectrum_mlebins <- function(res_mlebins,
                                       ...
                                       ){   # TODO decide if want ... yes, just
                                        # make sure help files link to all
                                        # functions; check it all works.
  plot.size_spectrum_mlebin(res_mlebin = res_mlebins,
                            ...)    # want to have mlebins in class, hence need
  # this separate function.
}

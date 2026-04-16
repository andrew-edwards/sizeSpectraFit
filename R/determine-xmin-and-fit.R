##' Determine x_min as the mode and then fit it. TODO only on a numeric vector
##' for now  TODO can't make a generic function because for mlebins we don't
##' have a class of input.  Seems better to explicitly specify the method to be
##' used, either mle, mlebin or mlebins.
##' TODO going to see if mlebins code will work for mlebin
##'
##' @param dat `numeric` vector of values (such as individual body masses or lengths), which uses
##'   the MLE method (via the function [fit_size_spectrum.numeric()]
##'   TODO join help files together
##' @param ... arguments to pass onto fit....
##' @return list of class `determine_xmin_and_fit` for plotting, containing TODO
##' \describe{
##' \item{bin_width}
##' \item{xmin}
##' \item{xmax}
##' \item{n}
##' \item{counts_per_bin}
##' \item{counts_per_bin_desc}{counts in the descending limb, including the
##'   peak}
##' \item{b_l}
##' \item{b_l_confMin}
##' \item{b_l_confMax}
##' }
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # Make data that needs xmin determined
##' data <- c(sim_vec + 9, runif(100, 0.1, 10))
##' res <- determine_xmin_and_fit(data)
##' plot(res)
##'
##' }
determine_xmin_and_fit <- function(dat,
                                   bin_width = 1,
                                   bin_start = NULL,
                                   ...){
  stopifnot("dat needs to be a numeric vector" =
              class(dat) == "numeric")

  hh <- make_hist(dat,
                  bin_width = bin_width,
                  bin_start = bin_start)

  x_min <- determine_xmin_based_on_hist(hh)

  # TODO change this to be the minimum x above that x_min
  mle_fit <- fit_size_spectrum.numeric(dat,
                                       x_min = x_min,
                                       ...)
  res <- list(mle_fit = mle_fit,
              h = hh)

  class(res) <- c("determine_xmin_and_fit", class(res))
  return(res)
}

##' Determine x_min as the mode and then fit it. TODO only on a numeric vector
##' for now
##'
##' @param x `numeric` vector of values (such as individual body masses or lengths), which uses
##'   the MLE method (via the function [fit_size_spectrum.numeric()] TODO could
##'   make `dat` for consistency with fit_size_spectrum() if going to make this
##'   more general. TODO join help files together
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
##' # see .Rmd file
##' HERE:
##' res <- determine_xmin_and_fit(sim_vec)
##' plot(res)
##'
##' TODO  res_all_strata <- fit_all_years(raw_simp_prop,
##'                     bin_width_each_year = bin_width_each_year)
##' }
determine_xmin_and_fit <- function(x,
                                   bin_width = 1,
                                   bin_start = NULL,
                                   ...){
  hh <- make_hist(x,
                  bin_width = bin_width,
                  bin_start = bin_start)


  x_min <- determine_xmin(hh)      # TODO add in here what I did for MLEbins,
                                   # and change determine_xmin() function name
  # to determine_xmin_based_on_hist


  # TODO change this to be the minimum x above that x_min
  mle_fit <- fit_size_spectrum.numeric(x,
                                       x_min = x_min,
                                       ...)
  res <- list(mle_fit = mle_fit,   # contains x, TODO change this if ends up
                                   # containing x_min and x_max
              h = hh,
              x_min = x_min)
              # TODO x_max = x_max)

  class(res) <- c("determine_xmin_and_fit", class(res))
  return(res)
}

##' Print size spectrum results for fit to a numeric vector, i.e. [fit_size_spectrum.numeric()]
##'
##' Since the raw data are included in the output from
##' [fit_size_spectrum.numeric()], the default [print()] would print all values,
##' so here we provide condensed output, and do not print the class attribute
##' (namely `size_spectrum_numeric` and `list`).
##'
##' @param res size_spectrum_numeric object, as output from
##'   [fit_size_spectrum.numeric()], which gets called when applying
##'   [fit_size_spectrum()] to a numeric vector
##' @param ... Further arguments passed onto [print.default()]
##' @return Invisible, as for for [print.default()].
##' @export
##' @author Andrew Edwards
##' @examples
##' res_vec <- fit_size_spectrum(sim_vec)
##' res_vec
##' }
print.size_spectrum_numeric <- function(res,
                                        ...){
  res_to_print <- res
  res_to_print$x <- head(res$x,
                         10)
  class(res_to_print) <- "list"    # So that it does not print all the attributes
  print.default(res_to_print)
}
